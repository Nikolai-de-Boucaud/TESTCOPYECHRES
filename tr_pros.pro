; file tr_pros.pro
; Contains pros w_to_x ( which calls wtox and xform) and translate_h
; Also xparxperp_to_xy and translate_xytable
;
; Used for translating a table of values written by cql3d in p-space
;	to a new table of values written in w=v/v_th space
; Usage:
;	w_to_x, $
;		v_norm, T_keV, gridsize, max_vt, b_min, b_loc, $	; inputs
;		x_par, x_perp, w_par, w_perp						; outputs
;	translate_h, $
;		flux_nc, xpar_nc, xperp_nc, $	; inputs from .nc in x_par,x_perp format
;		x_par, x_perp, flux_trans		; outputs

;w_to_x generates 2D tables of values of x_par[2*gridsize+1,gridsize], 
;	x_perp[2*gridsize+1,gridsize] at the outboard midplane which correspond 
;	to local values of w_par[2*gridsize+1] and w_perp[gridsize]
;translate_h interpolates the flux table at the new grid points and returns a
;	translated table flux_trans[2*gridsize+1,gridsize] with values specified for the 
;	regular grid w_par, w_perp 

; Or usage:
;	w_to_x, $
;		v_norm, T_keV, gridsize, max_vt, b_min, b_loc, $	; inputs
;		x_par, x_perp, w_par, w_perp						; outputs
;	translate_xytable, $
;		flux_nc, x_nc, y_nc, $	; inputs from .nc in x,y format
;		x_target, y_target, $	; input target x and y
;		flux_trans				; output table translated 


; wtox.pro
; Called only by w_to_x
; Inputs: 
;	w_par, w_perp are v/v_t, where v_t is thermal velocity 
;	v_norm is from cql
;	T_keV is local electron temperature
; Outputs:
;	x_par, x_perp for cql, = p_par/m_0/v_n, where v_n is the normalization
pro wtox, v_norm, T_keV, w_par, w_perp, x_par, x_perp
; b -> beta == v/c
b_t=sqrt((T_keV/511. +(T_keV/1022.)^2)/(1. + T_keV/511. +(T_keV/1022.)^2)); beta_th
b_n=(v_norm/2.9979e10)	; beta_norm
b_perp=w_perp*b_t		; beta_perp
b_par=w_par*b_t			; beta_par
di=1.-b_perp^2-b_par^2
if di le 0.0 then begin
	print, '  !!! di < 0 in wtox'
	if getenv('USER') eq 'prater' then stop
endif
denom=b_n*sqrt(di)
x_perp=b_perp/denom		; corresponding x_perp, x_par
x_par=b_par/denom
if finite(x_par) ne 1 or finite(x_perp) ne 1 then begin
	print, '  !!! NaN in wtox'
	if getenv('USER') eq 'prater' then stop
endif
end	

pro xform, xpar, xperp, bmin, bloc, xpartr, xperptr
; Called only by w_to_x
; transforms an element of momentum space (xpar,xperp) from
; magnetic field bmin to magnetic field bloc
; Returns transformed values (xpartr, xperptr)
xperptr=sqrt(xperp^2*bmin/bloc)			; constant magnetic moment
xpartr=sqrt(xpar^2+xperp^2-xperptr^2)	; constant energy
if xpar lt 0 then xpartr=-xpartr	; preserve the sign
end

pro w_to_x, v_norm, T_keV, wgridsize, maxv_over_vt, b_min, b_loc, $	; inputs
	x_par, x_perp, w_par, w_perp	; outputs
; Called only by translate_table
; transforms local w=v/v_th to x=p/m0 at minimum |B| coordinates
; Inputs:
;	v_norm from cql
;	T_keV is local electron temperature in keV
;	wgridsize is (0dd) number of elements in w_perp grid. w_par grid is 2x-1 in size.
;	maxv_over_vt is limit of v/v_th for w coordinates
;	b_min and b_loc are magnitudes of local and minimum |B| on flux surface
;	dxperp and dxpar are increments in x coordinate
; Outputs:
;	x_par, x_perp are momentum space coordinates at minimum of |B| used in CQL3D output
;		corresponding to w_par, w_perp (velocity normalized by v_th)
if b_min*b_loc lt 0 then b_min=-1.*b_min
w_perp=double(get_range(0., maxv_over_vt, wgridsize))
w_par=double(get_range(-maxv_over_vt, maxv_over_vt, 2*wgridsize-1))
x_perp=dblarr(2*wgridsize-1, wgridsize)
x_par=dblarr(2*wgridsize-1, wgridsize)
for i=0, 2*wgridsize-2 do begin
	for j=0, wgridsize-1 do begin
		wtox, v_norm, T_keV, w_par[i], w_perp[j], x_pari, x_perpi
		if finite(x_pari) ne 1 then begin
			print, '  !!! x_pari not finite in w_to_x'
			if getenv('USER') eq 'prater' then stop
			return
		endif
		xform, x_pari, x_perpi, b_min, b_loc, x_part, x_perpt
		if finite(x_part) ne 1 then begin
			print, '  !!! x_perpi not finite in w_to_x'
			if getenv('USER') eq 'prater' then stop
			return
		endif
		x_par[i,j]=x_part
		x_perp[i,j]=x_perpt
		;print, i, j, x_part, x_perpt
	endfor
endfor
sss=finite(x_par)
k=where(sss eq 0, count)
if count gt 0 then stop

end

pro translate_h, flux_nc, xpar_nc, xperp_nc, x_par, x_perp, $	; inputs
	flux_tr	; output
; Called only by translate_table
; Translates 2D table flux_nc in x-space to table flux_tr in regular w-space
; Inputs:
;	flux_nc is 2D table, eg flux from nc file
;	xpar_nc, xperp_nc are x-arrays from nc file
;	x_par, x_perp are 2D tables of x values corresponding to w values,
;		from w_to_x procedure
; Output:
;	flux_tr is table of values interpolated at grid points x_par, x_perp
gridsz=n_elements(x_perp[0,*])
flux_tr=dblarr(2*gridsz-1, gridsz)
perpinds=findgen(n_elements(xperp_nc))
parinds=findgen(n_elements(xpar_nc))
for i=0, 2*gridsz-2 do begin
	for j=0, gridsz-1 do begin
		xperp_ind=interpol(perpinds, xperp_nc, x_perp[i,j])		
		xpar_ind=interpol(parinds, xpar_nc, x_par[i,j])
		flux_tr[i,j]=interpolate(flux_nc, [xpar_ind], [xperp_ind], cubic=-.50, /grid)
	endfor
endfor
end

;***********************************************************************
; translate_table
; Translates a table from cql3d (eg, flux in momentum space at the outboard 
; midplane) to an equivaltent table at the local interaction location (b_local)
; gridded at regular values of w=v/v_t, along with corresponding tables of 
; x_parallel and x_perp appropriate at the values in the translated table
;pro translate_table, table_nc, x_par_nc, x_perp_nc, $
;	v_norm, T_keV, gridsize, max_vt, b_min, b_loc, $ ;inputs
;	w_par, w_perp, x_par, x_perp, table_trans	; outputs
;w_to_x, v_norm, T_keV, gridsize, max_vt, b_min, b_loc, $
;	x_par, x_perp, w_par, w_perp	; outputs
;	translate_h, table_nc, x_par_nc, x_perp_nc, x_par, x_perp, $
;	table_trans	; output
;end

;************************************************************
; For case of flux given in a table of x,y (=> u,theta) coordinates
; .nc file gives flux[ix,jy], x[ix], y[jy] 
; xy_to_xperpxpar gives 2D arrays xpar[k,m] and xperp[k,m] 
;	corresponding to the elements flux[k,m]
; w_to_x gives w_par[size], w_perp[size] and x_par_grid[2*size+1,size],
;	x_par_grid[2*size+1,size] for the desired grid, 
;	translated from the outboard midplane to the local interaction location
; translate_table2D translates the 2D table from the nc file to a 2D table
;	at the values of x_par_grid,x_perp_grid

; xy_to_xparxperp, x, y, xpart, xperpt
; x[0:jx] and y[0:iy] are u and theta from cql grid
; xpart[jx,iy] and xperpt[jx,iy] are equivalents
pro xy_to_xparxperp, x, y, xpart, xperpt
jx=n_elements(x)
iy=n_elements(y)
xpart=fltarr(jx, iy)
xperpt=fltarr(jx, iy)
for i=0, iy-1 do begin
	xpart[*,i]=x*cos(y[i])
	xperpt[*,i]=x*sin(y[i])
endfor
end

pro xparxperp_to_xy, xpart, xperpt, x, y
np=n_elements(xpart[0,*])
x=dblarr(2*np-1, np)
y=x
for i=0, np-1 do begin
	x[*,i]=sqrt(xpart[*,i]^2 + xperpt[*,i]^2)
	ind=where(xpart[*,i] eq 0., count)
	if count ne 0 then begin
		y[*,i]=atan(xperpt[*,i]/xpart[*,i])
		jind=where(xpart[*,i] lt 0.)
		if jind[0] ne -1 then y[jind,i]=!pi+y[jind,i] 
	endif else begin
		ind2=where(xpart[*,i] ne 0., count)
		if count gt 0 then begin
			y[ind2,i]=atan(xperpt[ind2,i]/xpart[ind2,i])
			jind2=where(xpart[*,i] lt 0.)
			if jind2[0] ne -1 then y[jind2,i]=!pi+y[jind2,i] 
			y[ind,i]=!dpi/2.
		endif
	endelse
endfor
end

; translate_xytable
; Inputs:
;	flux_nc[ix,jy]: flux table from .nc arranged on x,y grid (momentum, theta)
;	x[ix], y[jy] : regular x,y grid from cql .nc file 
;	x_target[2*nt-1,nt], y_target[2*nt-1, nt]: arrays of target values of 
;	x and y
; Output:
;	flux_trans[nt]: array of values of flux corresponding to (x,y) pairs

pro translate_xytable, flux_nc, x_nc, y_nc, x_target, y_target, $	;inputs
	flux_trans	; output
gridsize=n_elements(x_target[0,*])
flux_trans=dblarr(2*gridsize-1, gridsize)
xindx=dindgen(n_elements(x_nc))
yindx=dindgen(n_elements(y_nc))
for i=0, gridsize-1 do begin
	x_ind=interpol(xindx, x_nc, x_target[*,i])
	y_ind=interpol(yindx, y_nc, y_target[*,i])
	for k=0, 2*gridsize-2 do $
		flux_trans[k,i]=interpolate(flux_nc, [x_ind[k]], [y_ind[k]], cubic=-0.5, /grid)
endfor
end

