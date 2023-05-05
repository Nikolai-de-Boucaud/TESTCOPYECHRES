function str, a
return, string(strtrim(string(a),2))
end

function st2, a
return, strtrim(string(a,format='(f16.2)'),2)
end

function st3, a
return, strtrim(string(a,format='(f16.3)'),2)
end

function st4, a
return, strtrim(string(a,format='(f16.4)'),2)
end


pro plot_flux_echres, mode, drawid, plot_res, system
; set printit to print only, otherwise plot to screen
; mode eq 1 for rf flux, 2 for log rf flux, 3 for tot flux, 4 for log tot flux
; drawid is the id of the draw widget
; plot_res=0 or 1 resonance curves
; system is the EC system for which the resonance should be plotted

Common ECHCOM
Common WIDGETIDS, WidIDs
old_window=!D.window

cd, current=olddir
cd, PrgDat.cql_dir

; Plots fluxes from cql3d
; Same as plot_flux_xc, but uses newer .nc files with current density
; Same as plot_flux, but contour is in color
; Input files in local directory: 
;	toray.nc, mne.nc, mne_rf.nc, mne_flux_3.nc, mne_flux_4.nc,
;		cqlinput, and g_eqdsk, where 'mne' is the mnemonic (shotnumber)
; Output: optional postcript file flux_xc.ps
;	Plots of RF flux in velocity space, and total flux, with color contour
;		plot of |flux|

; Works for fluxes on (u, theta) grid (*vec='xtheta').

;***************************************
vparmin=0.

mne=strtrim(PrgDat.shot,2)
b_fudge_factor=PrgDat.cql_b_fudge_factor

obj=0			; obj=0 for screen, 1 for fig, 2 for vugraf
gridsize=		[ 31, 101,   25]
vgridsize=		[ 31,  25,   25]
arrow_thickness=[1.0, 2.75,  2.8]
arrow_len=		[4.0,1.25,  1.0]
res_thickness=	[2.5, 4.0, 10.0]
vel_thickness=	[1.0, 1.5,  4.0]

mxvt=4.d0		; maximum v/v_t
grid=150

; get the toray.nc file
fh=findfile('combined_toraync.nc', count=count)
if fh[0] eq '' then begin
	print, '  !!! Cound not find toray nc file combined_toraync.nc'
	goto, done
endif
t=readnc(fh[0])
if t.filename eq '' then begin
	print,  '  !!! Failure on opening combinedtoraync.nc file: ', torayncfile
	goto, done
endif

fhtoray=findfile('toray.nc_sys*', count=ngyros)
if count eq 0 then begin
	result=dialog_message('No toray.nc_sys* files available', /info)
	goto, done
endif
z=readnc(fhtoray[0])	; for number of rays/gyrotron
if z.filename eq '' then begin
	print, '  !!! Failure on opening toray.nc_sys*'
	goto, done
endif
nray=z.nray				; number of rays per gyrotron
weecrh=fltarr(200,ngyros)
weecrh[*,0]=z.weecrh
	
c=readnc(mne+'.nc')
if c.filename eq '' then begin
	print, '  !!! Failure on opening shot.nc file '
	goto, done
endif

ntime=size(c.curtor)
ntime=ntime[2]-1
r=readnc(mne+'_rf.nc')
if r.filename eq '' then begin
	print, '  !!! Failure on cql_rf.nc file '
	goto, done
endif

s=readnc(mne+'_flux_3.nc')
if s.filename eq '' then begin
	print, '  !!! Failure on cql_flux_3.nc file '
	goto, done
endif

u=readnc(mne+'_flux_4.nc')
if u.filename eq '' then begin
	print, '  !!! Failure on cql_flux_4.nc file '
	goto, done
endif

q=read_nl2('cqlinput')
;g=readg('eqdsk')
;f=fluxfun(g)
bsign=fix(gstruct.bcentr/abs(gstruct.bcentr))

; ********** done with input files ***************
white=0
black=1
red=2
blue=3
green=5
colax=1
brown=6

if r.nharms gt 1 then begin
	print, '  !!! ', str(r.nharms), ' considered'
	print, '  *** Only considering ', str(r.nharm1), ' harmonic'
endif ;else print, '  *** Considering harmonic ', str(r.nharm1)
B0=t.freqcy/r.nharm1/2.799376e6	; mag field of cold resonance

v_norm=c.vnorm

; see if enough flux to do the translation
gamma=fltarr(u.lrz)
for i=0, u.lrz-1 do gamma[i]=max(sqrt(u.gamma_x[*,*,i]^2 + u.gamma_theta[*,*,i]^2))
gammarel=gamma/max(gamma)

if PrgDat.cql_mode eq 0 then goto, plotit

if gammarel[PrgDat.cql_surf] lt 0.001 then begin
	mesg='Surface '+strtrim(PrgDat.cql_surf,2) +' has too little rf interaction to contour'
	result=dialog_message(mesg, /info)
	contour, fltarr(2,2), [-mxvt,mxvt], [0,mxvt]	; clear the plot
	goto, done
endif

centrayind=PrgDat.cql_sys*nray	; index of central ray this system

; does ray have any power absorbed?
if r.delpwr[r.nrayelt[centrayind]-1,centrayind]/r.delpwr[0,centrayind] gt 0.98 then begin
	result=dialog_message('System '+strtrim(PrgDat.cql_sys+1,2)+' has no absorption', /info)
	contour, fltarr(2,2), [-mxvt,mxvt], [0,mxvt]	; clear the plot
	goto, done
endif

; does ray have minimal absorption where rho=rya?
cqlrho=PrgDat.cql_rya[PrgDat.cql_surf]
pw=interp(z.xmrho, weecrh[*,PrgDat.cql_sys], cqlrho)
if PrgDat.cql_mode ne 0 then if pw lt max(weecrh[*,PrgDat.cql_sys])/100. then begin
	result=dialog_message('System '+strtrim(PrgDat.cql_sys+1,2)+' has little absorption near this surface', /info)
	contour, fltarr(2,2), [-mxvt,mxvt], [0,mxvt]	; clear the plot
	goto, done
endif 

; find whether central ray crosses rya
wr=float(r.wr[*,centrayind])/100.
inds=where(wr gt 0.)
wr=wr[inds]
wz=float(r.wz[inds,centrayind])/100.
delpwr=float(r.delpwr[inds,centrayind])
rhos=rho_rz(gstruct, wr, wz, /norm)
nrhos=n_elements(rhos)
ff=where(rhos lt cqlrho, npts)

fi=-1
; does ray miss the flux surface completely?
if PrgDat.cql_mode ne 0 then if npts eq 0 then begin
	result=dialog_message('System '+strtrim(PrgDat.cql_sys+1,2)+' does not intersect this surface', /info)
	contour, fltarr(2,2), [-mxvt,mxvt], [0,mxvt]	; clear the plot
	goto, done
endif
fj=min(ff)
fk=max(ff)
fi=fk

letsgo:	; get local values

if ((fi gt 0) and (fi lt nrhos)) then begin
	T_keV=float(t.ste[fi,centrayind])
	r_pk=wr[fi]
	z_pk=wz[fi]
	n_par=float(r.wnpar[fi,centrayind])
	minmaxb, gstruct, r_pk, z_pk, bmin, bloc, bmax
	b_loc=abs(bloc)
	b_min=abs(bmin)
	b_max=abs(bmax)
endif else begin
	result=dialog_message('No solution found to where ray interacts with surface', /info)
	goto, done
endelse

; overplot cql profiles on toray profiles window
if xregistered('plot_kinetic_profiles_echres') and max(abs(PrgDat.cql_pwr)) gt 0. then begin
	wndw=!D.window
	wset, PrgDat.kinetic_plot_window
	plot, c.rya, c.rfpwr[*,0,ntime], xtitle='rho', $
		ytitle='P_rf (W/cm3)', color=colax
	oplot, c.rya, c.rfpwr[*,0,ntime], psym=1, color=colax, symsize=1.25
	oplot, c.rya, fltarr(c.lrz), psym=4, color=colax, symsize=1.5
	plot, c.rya, c.curtor[*,ntime], xtitle='rho', $
		ytitle='J_ec (A/cm2)', color=colax
	oplot, c.rya, c.curtor[*,ntime], psym=1, color=colax, symsize=2.25
	oplot, c.rya, fltarr(c.lrz), psym=4, color=colax, symsize=1.5

	cd, current=current
	xyouts, 0., 0.05, systime(), color=colax, /normal
	xyouts, 0., 0.05, current, color=colax, /normal
	wset, wndw
endif

arrow_thick=arrow_thickness[obj]
arrow_length=arrow_len[obj]
res_thick=res_thickness[obj]
vel_thick=vel_thickness[obj]

; first get (x_par, x_perp) coordinates for (w_par,w_perp) grid for color contours
surf=PrgDat.cql_surf
w_to_x, $
	v_norm, T_keV, grid, mxvt, b_min, b_loc, $	; inputs
	x_par, x_perp, w_par, w_perp									; outputs
xparxperp_to_xy, x_par, x_perp, x, y		; convert to x-theta coords

ugammax=transpose(u.gamma_x[*,*,surf])
ugammatheta=transpose(u.gamma_theta[*,*,surf])
sgammax=transpose(s.gamma_x[*,*,surf])
sgammatheta=transpose(s.gamma_theta[*,*,surf])

translate_xytable, ugammax, u.x, u.y[*,surf], x, y, gam_tot_x
translate_xytable, ugammatheta, u.x, u.y[*,surf], x, y, gam_tot_theta
translate_xytable, sgammax, s.x, s.y[*,surf], x, y, gam_rf_x
translate_xytable, sgammatheta, s.x, s.y[*,surf], x, y, gam_rf_theta

siny=sin(y)
cosy=cos(y)
gam_tot_par=gam_tot_x*cosy - gam_tot_theta*siny
gam_tot_perp=gam_tot_x*siny + gam_tot_theta*cosy
gam_rf_par=gam_rf_x*cosy - gam_rf_theta*siny
gam_rf_perp=gam_rf_x*siny + gam_rf_theta*cosy

gam_rf_tot=sqrt(gam_rf_par^2 + gam_rf_perp^2)
gam_tot=sqrt(gam_tot_par^2 + gam_tot_perp^2)

maxtotflux=max(gam_tot)
maxrfflux=max(gam_rf_tot)
maxflux=max(maxrfflux, maxtotflux)

; get parallel system for plotting arrows
w_to_x, $
	v_norm, T_keV, vgridsize[obj], mxvt, b_min, b_loc, $	; inputs
	x_par_arrow, x_perp_arrow, w_par_arrow, w_perp_arrow									; outputs
xparxperp_to_xy, x_par_arrow, x_perp_arrow, x_arrow, y_arrow		; convert to x-theta coords

translate_xytable, ugammax, u.x, u.y[*,surf], x_arrow, y_arrow, gam_tot_x_arrow
translate_xytable, ugammatheta, u.x, u.y[*,surf], x_arrow, y_arrow, gam_tot_theta_arrow
translate_xytable, sgammax, s.x, s.y[*,surf], x_arrow, y_arrow, gam_rf_x_arrow
translate_xytable, sgammatheta, s.x, s.y[*,surf], x_arrow, y_arrow, gam_rf_theta_arrow

siny_arrow=sin(y_arrow)
cosy_arrow=cos(y_arrow)
gam_tot_par_a=gam_tot_x_arrow*cosy_arrow - gam_tot_theta_arrow*siny_arrow
gam_tot_perp_a=gam_tot_x_arrow*siny_arrow + gam_tot_theta_arrow*cosy_arrow
gam_rf_par_a=gam_rf_x_arrow*cosy_arrow - gam_rf_theta_arrow*siny_arrow
gam_rf_perp_a=gam_rf_x_arrow*siny_arrow + gam_rf_theta_arrow*cosy_arrow
gam_rf_a=sqrt(gam_rf_perp_a^2 + gam_rf_par_a^2)
gam_tot_a=sqrt(gam_tot_perp_a^2 + gam_tot_par_a^2)
 
; get parallel system for plotting arrows
w_to_x, $
	v_norm, T_keV, vgridsize[obj], mxvt, b_min, b_loc, $			; inputs
	x_par_arrow, x_perp_arrow, w_par_arrow, w_perp_arrow			; outputs
xparxperp_to_xy, x_par_arrow, x_perp_arrow, x_arrow, y_arrow		; convert to x-theta coords

translate_xytable, ugammax, u.x, u.y[*,PrgDat.cql_surf], x_arrow, y_arrow, gam_tot_x_arrow
translate_xytable, ugammatheta, u.x, u.y[*,PrgDat.cql_surf], x_arrow, y_arrow, gam_tot_theta_arrow
translate_xytable, sgammax, s.x, s.y[*,PrgDat.cql_surf], x_arrow, y_arrow, gam_rf_x_arrow
translate_xytable, sgammatheta, s.x, s.y[*,PrgDat.cql_surf], x_arrow, y_arrow, gam_rf_theta_arrow

wp=dindgen(400)/50.-4.
wpe=res_ellipse(T_keV, t.freqcy/1.d9, wp, n_par, PrgDat.cql_b_fudge_factor*b_loc,  $
	vp, bsign=bsign)
index=where(wpe eq 0.)
if index[0] ne -1 then wpe[where(wpe eq 0.)] = -1.

th=asin(sqrt(b_loc/b_max))
tpb=abs(w_par)*tan(th)

wthval=[1.,2.,3.,4.]
wth=fltarr(n_elements(wp), n_elements(wthval))
wth[*,*]=-1.
for kk=0, n_elements(wthval)-1 do begin
	wtemp2=wthval[kk]^2-wp^2
	inds=where(wtemp2 ge 0.)
	wth[inds,kk]=sqrt(wtemp2[inds])
endfor

dest=0
plotit:
case PrgDat.cql_quadrant of 
	0:xr=[-mxvt,mxvt]
	1:xr=[-mxvt,0]
	2:xr=[0,mxvt]
endcase
cd, current=current

if PrgDat.cql_window ge 0 then if PrgDat.cql_print eq 0 then wset, PrgDat.cql_window

if mode eq 0 then begin ; plot the flux magnitude
	plot, PrgDat.cql_rya, gammarel, xr=[0,1], yr=[0,1], xtit='rho', ytit='Flux Magnitude', $
		color=colax, title='Normalized CQL flux and normalized ECH powers'
	oplot, PrgDat.cql_rya, gammarel, psym=5, color=colax
	for i=0, n_elements(gammarel)-1 do xyouts, PrgDat.cql_rya[i]+0.005, gammarel[i], strtrim(i,2), charsize=0.8
	if ngyros gt 1 then for i=0, ngyros-1 do begin
		z=readnc(fhtoray[i])
		weecrh[*,i]=z.weecrh
	endfor
	xp=0.8
	yp=0.8
	dyp=0.03
	gyroon=where(PrgDat.status.plotrays gt 0)
	for i=0, ngyros-1 do begin
		if max(weecrh[*,i]) gt 0. then begin
			jj=where(weecrh[*,i]/max(weecrh[*,i]) gt 0.01, count)
			if count gt 2 then oplot, z.xmrho[jj], weecrh[jj,i]/max(weecrh[jj,i])/5., color=2+i
			xyouts, xp, yp, strtrim(i+1,2), color=2+i
			xyouts, xp+0.025, yp, ECHSYS.tank[gyroon[i]].gyroname, color=2+i
			yp -= dyp
		endif
	endfor
	xyouts, 0.9, 1.01, charsize=0.75, PrgDat.shottime, color=colax	
	xyouts, 0.0, 1.01, systime(), color=colax, charsize=0.75
	xyouts, 0.0, 1.04, current, color=colax, charsize=0.75
endif

if PrgDat.cql_mode eq 1 then begin ; plot dist func
	w_to_x, $
		v_norm, T_keV, grid, mxvt, b_min, b_loc, $	; inputs
		x_par, x_perp, w_par, w_perp									; outputs
	xparxperp_to_xy, x_par, x_perp, x, y		; convert to x-theta coords

	fc=transpose(c.f[*,*,PrgDat.cql_surf])

	translate_xytable, fc, c.x, c.y[*,PrgDat.cql_surf], x, y, fctrans

	lev=max(fctrans)/[4096.,2048.,1024.,512.,256.,128.,64.,32.,16.,8.,4.,2.]

	contour, fctrans, reverse(w_par), w_perp, $
		lev=lev, xr=xr, yr=[0,mxvt], color=colax, $
		title='Distribution function', /isotropic, $
		subtitle= 'rho=' + st3(cqlrho) + $
		'; max(gamma_rf)=' + string(maxrfflux, format='(e11.3)') + $
		'; n||=' + st3(n_par) + $
		'; B_loc=' + st3(b_loc) + ' T', $
		xtit='v_parallel/v_thermal', ytit='v_perpendicular/v_thermal'

	if plot_res gt 0 then oplot, wp, wpe, color=red, thick=res_thick
	oplot, w_par, tpb, color=green, thick=res_thick
	for kk=0, n_elements(wthval)-1 do oplot, wp, wth[*,kk], color=brown, thick=vel_thick
endif

if PrgDat.cql_mode eq 2 then begin ; contour delta f
	w_to_x, $
		v_norm, T_keV, grid, mxvt, b_min, b_loc, $	; inputs
		x_par, x_perp, w_par, w_perp									; outputs
	xparxperp_to_xy, x_par, x_perp, x, y		; convert to x-theta coords

	fc=transpose(c.f[*,*,PrgDat.cql_surf])
	
	; mock up the low power distribution function
	fc_lopwr=fc
	Tlocal=PrgDat.profiles.tein[nearest_index(PrgDat.profiles.RTEIN, PrgDat.cql_rya[PrgDat.cql_surf])]
	for jj=2, 299 do fc_lopwr[jj,*]=fc[0,0]*exp(-(511./(3.*Tlocal)*c.x[jj]^2))
	
	translate_xytable, fc-fc_lopwr, c.x, c.y[*,PrgDat.cql_surf], x, y, dftrans
	
	lev=[0.,max(abs(dftrans))/reverse(findgen(25)+1)]
	contour, dftrans, reverse(w_par), w_perp, $
		lev=lev, xr=xr, yr=[0,mxvt], color=black, c_colors=blue, $
		title='Delta f, blue=gain, red=loss', /isotropic, $
		subtitle= 'rho=' + st3(cqlrho) + $
		'; max(delta f)=' + string(max(abs(dftrans)), format='(e11.3)') + $
		'; n||=' + st3(n_par) + $
		'; B_loc=' + st3(b_loc) + ' T', $
		xtit='v_parallel/v_thermal', ytit='v_perpendicular/v_thermal'

	lev=-max(abs(dftrans))/findgen(25)
	contour, dftrans, reverse(w_par), w_perp, $
		lev=lev, xr=xr, yr=[0,mxvt], c_colors=red, /isotropic, /noerase

	if plot_res gt 0 then oplot, wp, wpe, color=red, thick=res_thick
	oplot, w_par, tpb, color=green, thick=res_thick
	for kk=0, n_elements(wthval)-1 do oplot, wp, wth[*,kk], color=brown, thick=vel_thick
endif

if PrgDat.cql_mode eq 3 then begin
	contour, fltarr(2,2), xr, [0,mxvt], /closed, $
		/fill, levels=[0.], color=colax, c_colors=black, $
		xrange=xr, yrange=[0., mxvt], xstyle=1, ystyle=1, $
		ytitle='v_perpendicular/v_thermal', $
		xtitle='v_parallel/v_thermal', $
		title='RF flux', /isotropic, $
		subtitle= 'rho=' + st3(cqlrho) + $
		'; max(gamma_rf)=' + string(maxrfflux, format='(e11.3)') + $
		'; n||=' + st3(n_par) + $
		'; B_loc=' + st3(b_loc) + ' T' 		

	if plot_res gt 0 then $
		oplot, wp, wpe, color=red, thick=res_thick
	oplot, w_par, tpb, color=green, thick=res_thick
	for kk=0, n_elements(wthval)-1 do $
		oplot, wp, wth[*,kk], color=6, thick=vel_thick

	velovect, -gam_rf_par_a, gam_rf_perp_a, reverse(w_par_arrow), w_perp_arrow, $
		len=arrow_length,  /noerase, color=white, thick=arrow_thick, $
		xstyle=5, ystyle=5, /isotropic, xrange=xr, yrange=[0., mxvt]
endif

if PrgDat.cql_mode eq 4 then begin	; contour rf flux
	contour, gam_rf_a, reverse(w_par_arrow), w_perp_arrow, color=colax, c_colors=blue, $
		/isotropic, xrange=xr, yrange=[0.,mxvt], xtitle='v_parallel/v_thermal', $
		ytitle='v_perpendicular/v_thermal', title='Contour RF Flux', $
		levels=reverse(max(gam_rf_a)/findgen(10));, $
		subtitle= 'rho=' + st3(cqlrho) + $
		'; max(gamma_rf)=' + string(maxrfflux, format='(e11.3)') + $
		'; n||=' + st3(n_par) + $
		'; B_loc=' + st3(b_loc) + ' T' 		

	if plot_res gt 0 then $
		oplot, wp, wpe, color=red, thick=res_thick
	oplot, w_par, tpb, color=green, thick=res_thick
	for kk=0, n_elements(wthval)-1 do $
		oplot, wp, wth[*,kk], color=brown, thick=vel_thick
endif

; ********** total flux ***********
if PrgDat.cql_mode eq 5 then begin	; total flux

	contour, fltarr(2,2), xr, [0,mxvt], /closed, $
		/fill, color=colax, c_colors=black, levels=[0.], $
		xrange=xr, yrange=[0,mxvt], /isotropic, $
		xtitle='v_parallel/v_thermal', ytitle='v_perpendicular/v_thermal', $
		title='Total flux', xstyle=1, ystyle=1, $
		subtitle= 'rho=' + st3(cqlrho) + $
		'; max(gamma_tot)=' + string(maxflux, format='(e11.3)') + $
		'; n||=' + st3(n_par) + $
		'; B_loc=' + st3(b_loc) + ' T'
	
	if plot_res gt 0 then oplot, wp, wpe, color=red, thick=res_thick
	oplot, w_par, tpb, color=green, thick=res_thick
	for kk=0, n_elements(wthval)-1 do oplot, wp, wth[*,kk], color=6, thick=vel_thick

	velovect, -gam_tot_par_a, gam_tot_perp_a, reverse(w_par_arrow), w_perp_arrow, $
		len=arrow_length,  /noerase, color=white, thick=arrow_thick, $
		xstyle=5, ystyle=5, /isotropic, xrange=xr, yrange=[0., mxvt]
endif

if PrgDat.cql_mode eq 6 then begin	; contour total flux
	contour, gam_tot_a, reverse(w_par_arrow), w_perp_arrow, color=black, c_colors=blue, $
		/isotropic, xrange=xr, yrange=[0.,mxvt], xtitle='v_parallel/v_thermal', $
		ytitle='v_perpendicular/v_thermal', title='Contour Total Flux', $
		levels=reverse(max(gam_rf_a)/findgen(20)), $
				subtitle= 'rho=' + st3(cqlrho) + $
		'; max(gamma_rf)=' + string(maxrfflux, format='(e11.3)') + $
		'; n||=' + st3(n_par) + $
		'; B_loc=' + st3(b_loc) + ' T' 		

	if plot_res gt 0 then $
		oplot, wp, wpe, color=red, thick=res_thick
	oplot, w_par, tpb, color=green, thick=res_thick
	for kk=0, n_elements(wthval)-1 do $
		oplot, wp, wth[*,kk], color=brown, thick=vel_thick
endif

if PrgDat.cql_mode ne 0 then begin
	xyouts, xr[0], mxvt+0.25, charsize=0.75, PrgDat.shottime, color=colax	
	xyouts, xr[0], mxvt+0.10, systime(), color=colax, charsize=0.75
	xyouts, xr[1]-2.0, mxvt+0.10, current, color=colax, charsize=0.65
endif

; take care of printing ps file
if PrgDat.cql_print eq 1 then begin
	if n_elements(dest) eq 0 or dest eq 0 then begin
		filenm=dialog_pickfile(path=PrgDat.cql_psfiledir)
		if filenm eq '' then begin
			PrgDat.cql_print=0
			return
		endif
		PrgDat.cql_psfiledir=strmid(filenm,0,strpos(filenm, '/', /reverse_search)+1)
		dest=1
		set_plot, 'ps'
		colax=1
		device, /color, ysize=9., yoffset=1., /inches, filename=filenm
		!p.thick=2.
		!p.charthick=2.
		goto, plotit
	endif else begin
		device, /close
		print, ' '
		print, '  *** Wrote file '+filenm
		print, ' '
		set_plot, 'x'
		PrgDat.cql_print=0
		colax=1
		dest=0
		!p.thick=1.
		!p.charthick=1.
	endelse
endif

done:
wset, old_window
cd, olddir
end
