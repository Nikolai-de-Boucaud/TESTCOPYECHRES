;calculate the rays on the wall, if there is at least one gyrotron 
;hit the OUTER wall, it calls plot_rayonwall to make the plot

;;;;;;;;;;
FUNCTION make_overlap_vector, rmin, rmax, dr,overlap=overlap,nvec=nvec
;from    /u/vanzee/XI/WALL_HIST/make_overlap_vector.pro
;makes an overlapped vector with bins from rmin to rmax spaced 
;(1-overlap)*dr apart
;0-<1 ;fractional overlap of histogram bins. 0=no overlap
IF NOT keyword_set(overlap) THEN overlap=0.  

rcen=rmin
rcens=[!null]
ppr=0L
rshif=(1.-overlap)*dr

while (rcen lt rmax) do begin    
	rcens=[rcens,rcen]
	rcen=rcen+rshif
	ppr=ppr+1
endwhile
nvec=n_elements(rcens)

return,rcens
END

;;;;;;;;;;
PRO run_rayonwall,irf
Common ECHCOM
Common REFLECTION_CHECK, hit_wall, hit_port, stop_loop
Common ECH_PORTS, RP1_240, RP1_255, RP1_270, RP1_285, RP2_300
Common AUTOMATION, running_auto_mode

igyro=irf
ngyro=n_elements(irf)
pl_ray=intarr(ngyro)

;define parameters for EC power calculation / plotting
phrange=[185., 325.] ;D3D LHCS in degree
thrange=[-90., 120.]  ;D3D LHCS in degree
Rcenter=1.72  		 ;D3D machine R0 in meter
dl=0.05  			 ;delta distance [m] for historgramming
dphi=dl/2.49/!dtor   ;delta toroidal angle for histogramming, 
         			 ;0.23 gives 1cm resolution using R=2.495m
factor=0.5			 ;to adjust the resolution in poloidal angle
dth=factor*atan(dl,2.49-rcenter)*180./!pi  ;this gives 1.86degree resolution
ovlap=0.5            ;0-<1 ;fractional overlap of histogram bins. 0=no overlap
phhis=make_overlap_vector(min(phrange),max(phrange),dphi,overlap=ovlap,nvec=nph)
thhis=make_overlap_vector(min(thrange),max(thrange),dth,overlap=ovlap,nvec=nth)
ds=dl*factor*dl*1E4   ;size [cm^2] of each bin
gammascale=1.  ;(1=linear contour scaling, typically use 0.3-1)

; get the ray numbers used for the TORAY calculation
; and then initialize the rayend structure
fname=PrgDat.gdir+'toray.nc_sys'+strtrim(igyro[0]+1,2)

if file_test(fname) then begin
	t=readnc(fname)
	nray=t.nray
	rayend_data=replicate({ $
		system_number:0, $		; echres system number
		gyroname:'', $
		TankNo:0, $ 			; ECH system (tank) number
		portLR:0, $ 			; for labelling; 1: L or -1: R port
		rfpow:fltarr(nray), $	; power [Watt] left in each ray
		phi_end:fltarr(nray), $	; phi [deg]
		z_end:fltarr(nray), $	; z [m]
		R_end:fltarr(nray), $	; R [m]
		phhis:phhis, $			; toroidal angle [deg] for contour plot
		thhis:thhis, $			; poloidal angle [deg] for contour plot 
		rf_bin:dblarr(nph,nth) $	; power intensity [W/cm^2]		
		}, ngyro)		
endif

all_rayend_phi = []
all_rayend_theta = []

for i=0, ngyro-1 do begin

	kk=igyro[i]	; system number
	sufx='_sys'+strtrim(kk+1,2)
	fnam=PrgDat.gdir+'toray.nc'+sufx

	if file_test(fnam) then begin
		t=readnc(fnam)
		nray=t.nray		
	endif
		
	reflpowfrac=fltarr(t.nray)   ;;;reflected power fraction
	nelt=intarr(t.nray)
	FOR jj=0, t.nray-1 DO BEGIN
	    nelt[jj]=t.nrayelt[jj]
		reflpowfrac[jj]=t.delpwr[nelt[jj]-1,jj]/t.delpwr[0,jj]
	ENDFOR
	
;	indmrp=where(reflpowfrac GT 0.1,mrp)	;only consider cases with f_abs < 90%
	indmrp=where(reflpowfrac GT 0.02,mrp)	;consider cases with f_abs < 98%
	rp=reflpowfrac(indmrp)
	raypow0=t.delpwr[0,0]*1E-7 ;injected RF power (Watt) per ray
				   ;toray output power is in erg/s [1erg/s=1E-7Watt]
	phi_end=fltarr(mrp)
	z_end=fltarr(mrp)
	R_end=fltarr(mrp)
	raypow=fltarr(mrp)
	portLR=0
	
	for ii=0, mrp-1 DO BEGIN	
		;grab the last and the fifth last points from TORAY (R[cm],z[cm],phi,[radian])
		;two pts in TORAY are separated by ~0.5mm, but sometimes last two pts are the same
		;the following setting set the *two* points about 2cm apart
		ind1=nelt[indmrp[ii]]-5
		ind2=nelt[indmrp[ii]]-1

		R1=t.wr[ind1, indmrp[ii]] & z1=t.wz[ind1, indmrp[ii]] & tphi1=t.wphi[ind1, indmrp[ii]]
		R2=t.wr[ind2, indmrp[ii]] & z2=t.wz[ind2, indmrp[ii]] & tphi2=t.wphi[ind2, indmrp[ii]]
	
		;convert to D3D LH coordinate system and from radian to degree	
		;angrid2 is the launch angle wrt R-direction in degree	
		if t.angrid2[indmrp[ii]] lt 180. then sign=0. else sign=360.
		port=strsplit(ECHSys.Tank[kk].AntPort,' ', /extract)
		IF STRMATCH(port[0],'*.4') THEN portLR=1 ELSE portLR=-1 ;ECH laucher on left has larger 
			; port angle and ending with .4 while launcher on right has port angle ending with .6
		
		phi_port=float(port[0])
		phi1=phi_port-(tphi1/!dtor -sign)
		phi2=phi_port-(tphi2/!dtor -sign)

		;makeline takes R,z in m and phi in degree, /left_hand_coords so output is in D3D LHCS
		IF R1 LE R2 THEN BEGIN		
			exray=makeline_2pts(R1/100.,z1/100.,phi1,R2/100.,z2/100.,phi2,0.01,$
				/left_hand_coords,minr=(t.wr[ind1, indmrp[ii]]-0.05)/100.)
		ENDIF ELSE BEGIN
			exray=makeline_2pts(R1/100.,z1/100.,phi1,R2/100.,z2/100.,phi2,0.01,$
				/left_hand_coords,maxr=(t.wr[ind1, indmrp[ii]]+0.05)/100.)
		ENDELSE
		
		inot=pnpoly_d3d_wall(exray.r,exray.z,exray.phi)
		ind=where(inot LT 1, tmp)
		end_ind=min(ind)

		phi_end[ii]=exray.phi(end_ind)
		IF phi_end[ii] LT 0 THEN phi_end[ii] +=360. ;to correct azi angle very close to 180deg
		IF phi_end[ii] GT 360. THEN phi_end[ii] -=360.
		z_end[ii]=exray.z(end_ind)
		R_end[ii]=exray.r(end_ind)
		;unabsorbed RF power in each ray
		raypow[ii]=reform(t.delpwr[nelt[indmrp[ii]]-1,indmrp[ii]])*1E-7 

;	str='Ray#'+strtrim(indmrp[ii],1)+', unabsorbed power:'+strtrim(rp[ii]*100.,1)+'%'
;	PRINT, str+', PHI_RAY_END ='+string(phi_end[ii],format='(f7.2)')+ $
;		', Z_RAY_END ='+string(z_end[ii],format='(f6.2)')
		
	endfor ;;;loop of rays (ii loop)

	;CALC. POLOIDAL ANGLE
	th_end=atan(z_end,R_end-Rcenter)*180./!pi
	weightsd3d=rp	;unabsorbed power percentage
	surfhist=fltarr(nph,nth)  ;will have weights collected

	FOR jj=0,nph-1 do begin
		FOR j=0,nth-1 do begin 
			wlin=where((abs(th_end-thhis(j)) lt dth/2.) and $
					(abs(phi_end-phhis(jj)) lt dphi/2.) ,nct)
 			if nct gt 0 then begin
 				surfhist(jj,j)=total(weightsd3d(wlin))  ;will total weights in cell
 			endif 
		ENDFOR
	ENDFOR
	hf_bin=dblarr(nph,nth)
	hf_bin=surfhist*raypow0/ds

	all_rayend_phi = [all_rayend_phi, phi_end]
	all_rayend_theta = [all_rayend_theta, th_end]
	
	IF max(hf_bin) GT 0 THEN pl_ray[i]=1
	
	;save the ray end data
	rayend_data[i].system_number=kk+1 
	rayend_data[i].gyroname=ECHSys.Tank[kk].gyroname 
	rayend_data[i].TankNo=ECHSys.Tank[kk].TankNo
	rayend_data[i].portLR=portLR
	rayend_data[i].rfpow(indmrp)=raypow 
	rayend_data[i].phi_end(indmrp)=phi_end 
	rayend_data[i].z_end(indmrp)=z_end 
	rayend_data[i].R_end(indmrp)=R_end
	rayend_data[i].rf_bin=hf_bin


	
endfor  ;;;loop of gyro systems (i loop)

plgyroi=where(pl_ray GT 0, plgyron) 
print, "AUTOMATION BOOLL"
print, running_auto_mode
IF plgyron GT 0 THEN BEGIN 
	; rays hit wall so change bool value to true
	hit_wall=1B
	; run plot_rayonwall
	plot_rayonwall,rayend_data[plgyroi],plgyroi 

	; section made by Niko de Boucaud deboucaudn@fusion.gat.com
	; check if a trace hit a port and if so mark trace and make port_hit true

	; print, 'theta:'
	; print, strtrim(all_rayend_theta)

	; print, 'phi:'
	; print, strtrim(all_rayend_phi)

	; 240 R+1 port
	Rcent=172  		 ;D3D machine R0 in cm
	z=RP1_240.z
	R=RP1_240.R
	;CALC. POLOIDAL ANGLE
	theta=atan(z,R-Rcent)*180./!pi
	RP1_240_phi = RP1_240.phi
	RP1_240_theta = theta

	; 255 R+1 port
	Rcent=172  		 ;D3D machine R0 in cm
	z=RP1_255.z
	R=RP1_255.R
	;CALC. POLOIDAL ANGLE
	theta=atan(z,R-Rcent)*180./!pi
	RP1_255_phi = RP1_255.phi
	RP1_255_theta = theta

	; 270 R+1 port
	Rcent=172  		 ;D3D machine R0 in cm
	z=RP1_270.z
	R=RP1_270.R
	;CALC. POLOIDAL ANGLE
	theta=atan(z,R-Rcent)*180./!pi
	RP1_270_phi = RP1_270.phi
	RP1_270_theta = theta
	
	; 285 R+1 port
	Rcent=172  		 ;D3D machine R0 in cm
	z=RP1_285.z
	R=RP1_285.R
	;CALC. POLOIDAL ANGLE
	theta=atan(z,R-Rcent)*180./!pi
	RP1_285_phi = RP1_285.phi
	RP1_285_theta = theta
	
	; 300 R+2 port
	Rcent=172  		 ;D3D machine R0 in cm
	z=RP2_300.z
	R=RP2_300.R
	;CALC. POLOIDAL ANGLE
	theta=atan(z,R-Rcent)*180./!pi
	RP2_300_phi = RP2_300.phi
	RP2_300_theta = theta

	; PLOT PORT HIT VARIABLES
	impact_sym=2 ; asterisk
	impact_symsize=2 
	impact_color=6 ; orange
	for ind=0, n_elements(all_rayend_phi)-1 do begin
		; check 240 R+1 port
		if (all_rayend_phi[ind] LE max(RP1_240_phi)) && (all_rayend_phi[ind] GE min(RP1_240_phi)) then begin
			if (all_rayend_theta[ind] LE max(RP1_240_theta)) && (all_rayend_theta[ind] GE min(RP1_240_theta)) then begin
				oplot, [all_rayend_phi[ind]], [all_rayend_theta[ind]],  psym=impact_sym, symsize=impact_symsize, color=impact_color
				print, "HIT 240 R+1"
				hit_port=1B
			endif
		endif

		; check 255 R+1 port
		if (all_rayend_phi[ind] LE max(RP1_255_phi)) && (all_rayend_phi[ind] GE min(RP1_255_phi)) then begin
			if (all_rayend_theta[ind] LE max(RP1_255_theta)) && (all_rayend_theta[ind] GE min(RP1_255_theta)) then begin
				oplot, [all_rayend_phi[ind]], [all_rayend_theta[ind]],  psym=impact_sym, symsize=impact_symsize, color=impact_color
				print, "HIT 255 R+1"
				hit_port=1B
			endif
		endif

		; check 270 R+1 port
		if (all_rayend_phi[ind] LE max(RP1_270_phi)) && (all_rayend_phi[ind] GE min(RP1_270_phi)) then begin
			if (all_rayend_theta[ind] LE max(RP1_270_theta)) && (all_rayend_theta[ind] GE min(RP1_270_theta)) then begin
				oplot, [all_rayend_phi[ind]], [all_rayend_theta[ind]],  psym=impact_sym, symsize=impact_symsize, color=impact_color
				print, "HIT 270 R+1"
				hit_port=1B
			endif
		endif
		
		; check 285 R+1 port
		if (all_rayend_phi[ind] LE max(RP1_285_phi)) && (all_rayend_phi[ind] GE min(RP1_285_phi)) then begin
			if (all_rayend_theta[ind] LE max(RP1_285_theta)) && (all_rayend_theta[ind] GE min(RP1_285_theta)) then begin
				oplot, [all_rayend_phi[ind]], [all_rayend_theta[ind]],  psym=7, symsize=impact_symsize, color=impact_color
				print, "HIT 285 R+1"
				hit_port=1B
			endif
		endif
		
		; check 300 R+2 port
		if (all_rayend_phi[ind] LE max(RP2_300_phi)) && (all_rayend_phi[ind] GE min(RP2_300_phi)) then begin
			if (all_rayend_theta[ind] LE max(RP2_300_theta)) && (all_rayend_theta[ind] GE min(RP2_300_theta)) then begin
				oplot, [all_rayend_phi[ind]], [all_rayend_theta[ind]],  psym=impact_sym, symsize=impact_symsize, color=impact_color
				print, "HIT 300 R+2"
				hit_port=1B
			endif
		endif
		; end of niko's function

	endfor

ENDIF ELSE BEGIN
	IF running_auto_mode eq 0 then begin
	dummy=dialog_message('Absorption is <98%, but unabsorbed power on the wall is out of plotting range!')
	ENDIF
	return
ENDELSE
return
    
END    
