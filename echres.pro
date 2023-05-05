;************************************************************************;+
; NAME:
;	ECHRES
;
; PURPOSE:
; Plots ECH resonances on an EFIT flux plot. 
; Supports running the TORAY-GA ray-tracing code and 
; the CQL3D Fokker-Planck code.
;
; R. Prater, 28 January 2012
; Xi Chen,  starting from 17 June 2014
;
; CATEGORY:
;
; CALLING SEQUENCE:
;	ECHRES, adata, gdata
;
; INPUTS:
; 	adata		A structure of eqdsk A
; 	gdata		A structure of eqdsk G
;
; KEYWORD PARAMETERS:
; 	bid		The tlb widget ID of that returned by this function.
; 	color		Color index array of size 3.  Defaults are set 
;			if none or less than 3 colors are passed in.
; 	verbose		If set, information is printed to the screen.
;  	callback	The name of the procedure that is to be called by 
;			the eches event handler.
;	parameter	The parameter used by callback procedure. Either a
;			single parameter, or a structure or an array. Used
;			with callback keyword only.
;	cancel_event	Event handler when the done button is clicked.
;			Default to its own event handler if not set.
;	_extra		Any keywords acceptable to the top level base widget,
;			e.g., group_leader
;
; OUTPUTS:
;
; COMMON BLOCKS:
;	ECHCOM, ECHSys, PrgDat, fce0, fuh0, frhco0, gstruct, ffstruct
;	The ECHCOM is used by routines in this module only
;	!!! Defined in reg.pro !!!
;
; HISTORY:color
; Created	originally around 1994 by Cary Forest
;
;    04-28-98	Q. Peng
;	Adopted for the new efit_viewer.  Changes made are the following:  
; 	. Passing in data A, G in instead of reading them within the routine.  
; 	. Necessary unit conversion for densv2 since A is returned in 
;	  MKS units.
;	. Removed common blocks and procedures included from the 4d libraries. 
;   	  The only common block left is the echcom used by the module itself 
;   	  to maintain the status.  If needed, it may be replaced by uvalues 
;	  carried by widget though.  
;	. The eqplot-equvalent to put on the equilibrium is replaced with a 
;	  routine call back.  Both the procedure name and its parameters 
;	  are passed in.
;	. Coordinates are set before the routine is called, thus eliminating 
;	  the use of @scale_eq and any position, ranges paramaters.
;	. Colors are passed in as an array, thus eliminating @discolor.
;	. Added the verbose keyword to control whether printing information 
;	  to the screen.
;	. Added a done button to cancel the window. It uses the cancel_event 
;	  function passed in if the keyword is set or its own if not. 
;	. Used the uvalue of the top base widget to carry the added 
;	  information.
;
;************************************************************************

function dot, VA, VB
return, total(VA*VB)
end

function mag, VC
return, sqrt(dot(VC,VC))
end

;***********************************************************
pro echres_plot, color=color

Common ECHCOM	; defined in reg.pro
Common WIDGETIDS, WidIDs
Common DensWidIds ;allows ECHRES to determine if dens limit calculator GUI is active -PN 04/26/23

on_error, 2 ; Return to caller, when debugging, set to 0, otherwise 2

forward_function bfield_pol

; toray runs only on 257x257 and smaller equilibria
if gstruct.mw gt 257 or gstruct.mh gt 257 then begin
	res=dialog_message('TORAY can not run on equilibria greater than 257x257; aborting.')
	return
endif

fce=fce0[0:gstruct.mw-1, 0:gstruct.mh-1]	; define sub-arrays of appropriate size
frhco=frhco0[0:gstruct.mw-1, 0:gstruct.mh-1]
fuh=fuh0[0:gstruct.mw-1, 0:gstruct.mh-1]

if PrgDat.Status.OutputsValid then goto, just_plot_it	; avoid doing loop when unnecessary

; first check whether it's a new equilibrium
if PrgDat.new_shot then begin

	; toray runs only on 257x257 and smaller equilibria
	if gstruct.mw gt 257 or gstruct.mh gt 257 then begin
		res=dialog_message('TORAY can not run on equilibria greater than 257x257; aborting.')
		return
	endif
	
	; first clear the ECHSystem and Results data for the new shot
	Clear_Com, save_profs=PrgDat.save_profiles, save_inputs=PrgDat.save_inputs

	; then calculate new fce table
	print, '  *** Calculating new f_ce table'

	fce0[*,*]=0.	; clear the arrays in ECHCOM
	frhco0[*,*]=0.
	fuh0[*,*]=0.
	fce=fltarr(gstruct.mw, gstruct.mh)	; define arrays of appropriate size
	frhco=fce
	fuh=fce
	bf=bfield_pol(gstruct)
	fce=28.*bf.tot
	fce0[0:gstruct.mw-1, 0:gstruct.mh-1]=fce
	
	ffstruct=fluxfun_ech(gstruct)
	
	PrgDat.Source=gstruct.source
	strgshot=strtrim(gstruct.shot,2)
	gshot=long(strgshot.substring(0,5))
	PrgDat.Shot=gshot
	PrgDat.config_shot=gshot
	
	PrgDat.new_shot=0
	PrgDat.Time=gstruct.time
	PrgDat.shottime=string(gshot, format='(i6.6)') + '.'+ $
		string(gstruct.Time, format='(i5.5)')
		
	if not PrgDat.save_inputs then begin
		PrgDat.Status.recalc_rays[*]=1
		PrgDat.Status.PlotRays[*]=1
	endif
	if PrgDat.Prefs.nlinputfile eq '' then begin
		PrgDat.n_freq=0
		PrgDat.unique_freq[*]=0.
		PrgDat.gdir=''
		PrgDat.gfile=''
		if not PrgDat.save_profiles then begin
			widget_control, WidIDs.ProfSource, set_value=0
			profs=PrgDat.profiles
			bt_orig=profs.bt_orig
			clear_struct, profs
			Prgdat.profiles=profs
			PrgDat.Profiles.bt_orig=bt_orig
		endif
		PrgDat.Profiles.Bt0=gstruct.bcentr
	endif
	
	widget_control, WidIDs.CentralDensity, set_value=PrgDat.Profiles.CentralDensity
	widget_control, WidIDs.CentralTe, set_value=PrgDat.Profiles.CentralTe
	widget_control, WidIDs.Bt0, set_value=PrgDat.Profiles.Bt0

	; this code to handle case of MDS gfile
	gdir=PrgDat.Prefs.scratch_path+strtrim(PrgDat.shot,2)+'/'
	if not file_test(gdir) then file_mkdir, gdir
	PrgDat.gdir=gdir

	if  ( (strpos(gstruct.source, 'MDS') ge 0) and $   ; it's an MDS source gfile
		( (PrgDat.save_profiles ne 1) or (PrgDat.Prefs.nlinputfile ne '') ) ) $
		then begin
			gfilname='g'+PrgDat.shottime
			gsrc = gdir + gfilname
			if not file_test(gsrc) then writeg_file, gstruct, file=gsrc
	endif else gsrc=gstruct.source	; if not MDS equilibrium

	if not file_test(gsrc) then begin
		result=dialog_message(['Write/read of gfile unsuccessful:', gsrc]) 
		return
	endif

	PrgDat.gfile=gsrc
	PrgDat.gdir=gdir	

endif

if ECHSys.NumTanks le 0 then goto, skip_tanks

;********** For each system in use, do the following: ******************
for sys=0, ECHSys.NumTanks - 1 do begin

	if not (PrgDat.Status.recalc_rays[sys] and PrgDat.Status.PlotRays[sys]) $
		then goto, nextantenna		; don't calc polariation etc or plot ray

	if ECHSys.InputTable[sys].polarang le 0. then goto, nextantenna

	ECHSys.InputTable[sys].GyroName=ECHSys.Tank[sys].GyroName

	widget_control, WidIDs.UsePresent,get_value=present
	If present AND (PrgDat.config_shot LT PrgDat.First_Shot_Newest_Config) THEN PrgDat.config_shot=PrgDat.First_Shot_Newest_Config+1
	
	if PrgDat.Prefs.nlinputfile eq '' then $
		update_echsys, PrgDat.config_shot, ECHSys.InputTable[sys].GyroName

	; make vacuum rays vray
	dl=0.035	; separation between points on a ray, in m
	vray=makeline_lite(ECHSys.Antenna[sys].R_launch, ECHSys.Antenna[sys].Z_launch, $
		ECHSys.InputTable[sys].PolarAng*!dtor, ECHSys.InputTable[sys].AziAng*!dtor, $
		dl, max(gstruct.r), min(gstruct.r)+0.15, max(gstruct.z), min(gstruct.z))
	nrp=n_elements(vray.r)
	inl=min(where(vray.r eq ECHSys.Antenna[sys].r_launch))
	if inl eq -1 then begin ; if mirror outside eqdsk boundary
		print, '  !!! mirror outside eqdsk boundary for system '+strtrim(sys+1,2)
		vray.r=[ECHSys.Antenna[sys].r_launch, vray.r[0:nrp-2]]	; add mirror point
		vray.z=[ECHSys.Antenna[sys].z_launch, vray.z[0:nrp-2]]
		inl=0
	endif
	r_launch=ECHSys.Antenna[sys].r_launch
	z_launch=ECHSys.Antenna[sys].z_launch

	if nrp*dl gt PrgDat.Status.RayLength then begin ; trim ray length
		numraypoints=fix(PrgDat.Status.RayLength/dl)-1
		if inl le 1 then inl=1
		if numraypoints lt n_elements(vray.r) then begin
			zs=vray.z[inl-1:numraypoints]
			rs=vray.r[inl-1:numraypoints]
		endif 
	endif else begin
		if inl gt 0 then begin
			rs=vray.r[inl-1:*]
			zs=vray.z[inl-1:*]
		endif else if inl eq 0 then begin
			rs=vray.r
			zs=vray.z
		endif
	endelse

	; find (r,z) where ray crosses separatrix
	rhoi=rho_rz(gstruct,rs,zs,psival, /norm)
	ind=min(where(rhoi lt 1.0))
	if ind le 0 then begin
		result=dialog_message('echres: Rays from sys' + strtrim(string(sys+1),2) + $
			' miss the plasma!')
		goto, nextantenna
	endif

	; home in using linear interpolation
	frac=(rhoi[ind-1]-1.0)/(rhoi[ind-1]-rhoi[ind])
	r_edge=rs(ind-1)-frac*(rs(ind-1)-rs(ind))
	z_edge=zs(ind-1)-frac*(zs(ind-1)-zs(ind))
;	rs=[r_launch, r_edge, rs[ind:*]]
;	zs=[z_launch, z_edge, zs[ind:*]]
	rs=[r_launch, r_edge, rs]
	zs=[z_launch, z_edge, zs]
	
	; store the vacuum ray data for plotting
	ECHSys.Rays[sys].r[*]=0.
	ECHSys.Rays[sys].z[*]=0.
	ECHSys.Rays[sys].q_peak_r=0.
	ECHSys.Rays[sys].q_peak_z=0.
	ECHSys.Rays[sys].r[0:n_elements(rs)-1]=rs
	ECHSys.Rays[sys].z[0:n_elements(zs)-1]=zs

	;if PrgDat.Prefs.nlinputfile ne '' then goto, nextantenna

	;************ Start polarization stuff here ***************************

	; set up waveguide angles
	ib=where(ECHSys.XLine[sys].mir_type NE 0, NumBends)
	if NumBends eq 0 then goto, nextantenna
	mir_type=ECHSys.XLine[sys].mir_type[0:NumBends-1]
	gamma=ECHSys.XLine[sys].gammasd[0:NumBends-1]*!dtor
	
	nuu=nu(ECHSys.Antenna[sys].Pol_Ang*!dtor, $		
		ECHSys.Antenna[sys].Tor_Ang*!dtor, $
		ECHSys.Antenna[sys].pol_id*!dtor)
		
	find_pol, ECHSys.XLine[sys].alpha0*!dtor, ECHSys.XLine[sys].beta0*!dtor, $
		ECHSys.InputTable[sys].pol1*!dtor, ECHSys.InputTable[sys].pol2*!dtor, $
		ECHSys.XLine[sys].gm1, ECHSys.XLine[sys].gm2, $
		mir_type, gamma, fltarr(n_elements(gamma)), nuu, alpha_a, beta_a	

	; store launched polarizations
	ECHSys.Antenna[sys].alpha_a=reform(alpha_a)/!dtor
	ECHSys.Antenna[sys].beta_a=reform(beta_a)/!dtor

	; calculate fractional power in x-mode
	pols=xmfrac(ECHSys.Antenna[sys].r_launch, $
		ECHSys.Antenna[sys].z_launch, $
		ECHSys.InputTable[sys].PolarAng, $
		ECHSys.InputTable[sys].AziAng, $
		ECHSys.Antenna[sys].pol_id, $
		ECHSys.Antenna[sys].azi_id, $
		ECHSys.Antenna[sys].alpha_a, $
		ECHSys.Antenna[sys].beta_a, $
		ECHSys.Tank[sys].freq, $
		gstruct)
		if pols.success EQ 0 then print,'out of bounds'
	if pols.success then begin
	;alpha_x and beta_x is the inclination and ellipticity at the plasma edge for PURE x-mode
		ECHSys.Antenna[sys].alpha_x=pols.x_inclination
		ECHSys.Antenna[sys].beta_x=pols.x_ellipticity
		ECHSys.InputTable[sys].xfrac=pols.x_frac*100.
	endif 

	; clear the toray calc results
	ECHSys.InputTable[sys].f_abs=0.
	ECHSys.InputTable[sys].rho_peak=0.
	ECHSys.InputTable[sys].eccd=0.
	PrgDat.Status.recalc_rays[sys]=1
	;print, ECHSys.Antenna[sys].alpha_a, pols.x_inclination, $
	;	ECHSys.Antenna[sys].beta_a, pols.x_ellipticity, pols.x_frac*100. 

	nextantenna:

endfor

widget_control, WidIDs.ConfigTable, set_value=ECHSys.Tank
widget_control, WidIDs.InputTable, set_value=ECHSys.InputTable

skip_tanks:

;**********************************************************************
; contour upper hybrid resonance and RH cutoff

; get unique EC frequencies
uniq_freq=ECHSys.Tank[uniq(ECHSys.Tank.freq, sort(ECHSys.Tank.freq))].freq
ifrq=where(uniq_freq gt 0., ufcount)
if ufcount gt 0 then uniq_freq=uniq_freq[ifrq]
PrgDat.n_freq=n_elements(uniq_freq)
PrgDat.unique_freq[*]=0.
PrgDat.unique_freq[0:PrgDat.n_freq-1]=uniq_freq

; clear fuh etc
fuh0[*,*]=0.0
frhco0[*,*]=0.0
fuh=fuh0[0:gstruct.mw-1, 0:gstruct.mh-1]
frhco=frhco0[0:gstruct.mw-1, 0:gstruct.mh-1]

; consider upper hybrid if density data has been entered
if max(PrgDat.Profiles.enein) gt 1.e10 then begin
	izmag=nearest_index(gstruct.z, gstruct.zmaxis)
	zmag=replicate(gstruct.z[izmag], gstruct.mw)
	rho_vals0=rho_rz(gstruct, gstruct.r, zmag) < 1.0	; profile at Z=0
	fuhcut=sqrt(fce[*,izmag]^2 + $
		8.064e-11*interpol(PrgDat.Profiles.enein[0:PrgDat.Profiles.njene-1], $
		PrgDat.Profiles.renein[0:PrgDat.Profiles.njene-1], rho_vals0))
	if max(fuhcut/min(prgdat.unique_freq[0:prgdat.n_freq-1])) gt 0.70 then begin
		widget_control, /hourglass
		if PrgDat.Prefs.debug then message, 'Calculating upper hybrid and RH cutoff...', /info
		density=fltarr(gstruct.mw,gstruct.mh)
		for rj=0, gstruct.mw-1 do begin	; get density at grid points
			rho_vals=rho_rz(gstruct,replicate(gstruct.r[rj], gstruct.mh),gstruct.z,/norm) < 1.1
			density[rj,*]=interpol(PrgDat.Profiles.enein[0:PrgDat.Profiles.njene-1], $
				PrgDat.Profiles.renein[0:PrgDat.Profiles.njene-1], rho_vals, /spline)
			kden=where(rho_vals gt 1.0, nkden)
			if nkden gt 0 then density[rj,kden]=0.0 ; make density outside separatrix zero
		endfor
		fuh=sqrt(8.064e-11*density + fce^2)
		frhco=(fce + sqrt(4.*8.064e-11*density + fce*fce))/2.
	endif
	fuh0[0:gstruct.mw-1, 0:gstruct.mh-1]=fuh
	frhco0[0:gstruct.mw-1, 0:gstruct.mh-1]=frhco
endif
just_plot_it:
col=2
legx=0.80
legy=0.975
legdy=0.025


;IF DENSITY CALCULATOR IS OPEN, SKIP PLOTTING RAYS/CONTOURS (tries to plot on dens calculator widget -PN 04/26/23)
if ISA(stash) eq 1 then begin
	dens_calc_open = WIDGET_INFO(stash.DensCalcBase, /VALID_ID)
endif else begin
	dens_calc_open = 0
endelse

if dens_calc_open ne 1 then begin

	; plot the rays
	for sys=0, ECHSys.NumTanks-1 do begin
		if (PrgDat.Status.PlotRays[sys] and (ECHSys.InputTable[sys].PolarAng gt 10.)) then begin 
			
			inds=where(ECHSys.rays[sys].r gt 0.)
			if inds[0] ge 0 then begin
				if PrgDat.n_freq eq 1 then lst=0 else begin
					if (ECHSys.Tank[sys].freq eq PrgDat.unique_freq[0]) then lst=0
					if (ECHSys.Tank[sys].freq eq PrgDat.unique_freq[1]) then lst=5
					if (ECHSys.Tank[sys].freq eq PrgDat.unique_freq[2]) then lst=2
				endelse
				oplot, ECHSys.rays[sys].r[inds], ECHSys.rays[sys].z[inds], thick=2.0, $
					color=col, linestyle=lst
			endif
			
			if ECHSys.rays[sys].q_peak_r gt 0. then begin
				usersym,[0., -1.0, 0., 1.0], [1.0, 0., -1.0, 0.], /fill, color=col
				plots, ECHSys.rays[sys].q_peak_r, ECHSys.rays[sys].q_peak_z, psym=8
			endif
							
			xyouts, legx, legy, ECHSys.Tank[sys].gyroname, charsize=0.7, color=col++, /normal
			legy -= legdy
			; use EFITVIEWER color table,0-white,1-black,2-red,3-blue,4-green,5-yellow,6-orange
			; 							 7-light blue, 8-magenta, 9-purple, 10-light gray
			if col eq 5 then col++
		
		endif
	endfor

	; contour the cyclotron resonances
	if PrgDat.n_freq gt 0 and max(PrgDat.unique_freq) gt 10. then begin
		legy=0.955
		legx=0.3
		imaxradius=where(gstruct.r ge 0.94*gstruct.r[gstruct.mw-1])
		iruh=where(gstruct.r ge min(gstruct.lim[0,*]) and gstruct.r le 1.04*max(gstruct.lim[0,*]))
		izuh=where(gstruct.z le max(gstruct.lim[1,*]) and gstruct.z ge min(gstruct.lim[1,*]))
		iruh0=min(iruh) & iruh1=max(iruh) & izuh0=min(izuh) & izuh1=max(izuh)
		for kk=0, PrgDat.n_freq-1 do begin
			case kk of	; get linestyle 
				0: lst=0
				1: lst=5
				2: lst=2
				else: lst=4
			endcase
			
			contour, fce0[iruh0:iruh1, izuh0:izuh1], gstruct.r[iruh], gstruct.z[izuh], $
				levels=PrgDat.unique_freq[kk]/[4.,3.,2.,1.], nlevels=4, c_label=[1,1,1,1], $
				/noerase, ystyle=1+4, xstyle=1+4, c_colors=[7], $ 
				c_charsize=1.0, thick=2.5, c_annotation=['4','3','2','1'], min_value=1., $
				c_linestyle=lst
				
			if max(fuh0) gt 0.0 then $
				contour, (fuh[iruh0:iruh1, izuh0:izuh1]/PrgDat.unique_freq[kk])-1., $
				gstruct.r[iruh],gstruct.z[izuh],levels=[0.],nlevels=1, c_annotation=['uh'], $
				/noerase,ystyle=1+4,xstyle=1+4,c_colors=[4],thick=1, c_linestyle=lst
				
			if max(frhco0) gt 0.0 then $
				contour,(frhco[iruh0:iruh1, izuh0:izuh1]/PrgDat.unique_freq[kk])-1., $
				gstruct.r[iruh],gstruct.z[izuh],levels=[0.],nlevels=1, c_annotation=['rhco'], $
				/noerase,ystyle=1+4,xstyle=1+4,c_colors=[2],thick=2, c_linestyle=lst

			stlab='f= '+string(PrgDat.unique_freq[kk], format='(f6.2)')+' GHz'
			xyouts, legx+0.16, legy, stlab, color=7, charsize=0.7, /normal
			p1=convert_coord(legx, legy+0.005, /normal, /to_data)
			p2=convert_coord(legx+0.15, legy+0.005, /normal, /to_data)
			oplot, [p1[0],p2[0]], [p1[1], p1[1]], color=7, linestyle=lst, thick=2
			legy -= legdy

		endfor
		
		if abs(PrgDat.Profiles.Bt_orig) ge 0.01 and $
			abs(PrgDat.Profiles.Bt0 - PrgDat.Profiles.Bt_orig) gt 1.e-3 then begin
			stlab = ['B_t(0) set to ' + strtrim(gstruct.bcentr,2) + $
				'  from ' + strtrim(PrgDat.Profiles.Bt_orig,2) + ' T']
			xyouts, 0.10, legy, stlab, color=8, charsize=0.6, /normal
			legy -= legdy
		endif

	endif

endif

PrgDat.Status.recalc_rays[*]=0

PrgDat.Status.OutputsValid=1	; outputs calculated

return

end

;*******************************************************************
; function get_ech_struct
; Returns a structure similar in form to that returned by get_ech
; using data from the Results structure, used in running toray

function get_ech_struct

Common ECHCOM

p=replicate({ system_number:0, $
	gyro_name:'', tank_number:0, antenna_number:0, antenna_port:'', $
	facet_angle:0.0, tilt_angle:0.0, $
	polarizer1:0.0, polarizer2:0.0, $
	launch_r:0.0, launch_z:0.0, polar_angle:0.0, azimuthal_angle:0.0, $
	incident_power:0.0, generated_power:0.0, $
	inclination:0.0, ellipticity:0.0, divergence:0.0, $
	success:1}, ECHSys.NumTanks)

; Enter the data relevant to toray
for jj=0, ECHSys.NumTanks-1 do begin
	p[jj].system_number=jj+1
	p[jj].gyro_name=ECHSys.Tank[jj].gyroname
	p[jj].antenna_number=ECHSys.Tank[jj].AntNum
	p[jj].tank_number=ECHSys.Tank[jj].TankNo
	p[jj].launch_r=ECHSys.Antenna[jj].R_launch
	p[jj].launch_z=ECHSys.Antenna[jj].Z_launch
	p[jj].polar_angle=ECHSys.InputTable[jj].PolarAng
	p[jj].azimuthal_angle=ECHSys.InputTable[jj].AziAng
	p[jj].incident_power=ECHSys.Tank[jj].Power_MW * 1.e6
	p[jj].divergence=ECHSys.Antenna[jj].divergence
endfor

return, p
end

;*******************************************************************
pro echres_event, ev	; event handler for xmanager for echres widgets
Common ECHCOM
Common REFLECTION_CHECK
Common DensWidIds ;allows ECHRES to determine if dens limit calculator GUI is active -PN 04/13/23
Common AutoWidIDs, auto_stash ;allows ECHREs to determine if auto dens GUI is active -PN 04/20/23

dtr=!PI/180.

;rad_res=101	; le 101
rho_kin=get_range(0.,1.,rad_res)

Widget_Control, ev.top, get_uvalue=info
Widget_Control, ev.id, get_uvalue=uval

case uval of

;****************** For changing config table (top table) *************

'config_table': begin	; user changed something in config table

	old_data=ECHSys.Tank	; keep old input data in case of error
	Widget_Control, ev.id, get_value=configdata

	case ev.type of
	
		0: begin	; new data entry

			if ((ev.ch NE 10) and (ev.ch NE 13)) then break ; wait for carriage return
			
			; accept entry only for gyrotron, freq, power, and xfrac			
			if ( (ev.x ne 0) and (ev.x ne 1) and (ev.x ne 7) and (ev.x ne 8) and (ev.x ne 9)) then begin
				ECHSys.Tank=old_data
				print, '  !!! This field not editable.'
			endif else ECHSys.Tank=configdata

			; show input table not from from archive
			PrgDat.Status.ArchivedData=0	; user changed something
			widget_control, info.WidIDs.Archive, set_value=PrgDat.Status.ArchivedData

			; show outputs not valid yet for new input table
			PrgDat.Status.OutputsValid=0
			;widget_control, info.WidIDs.OutsValid, set_value=PrgDat.Status.OutputsValid
			
			widget_control, info.WidIDs.ConfigTable, set_value=ECHSys.Tank
			if ev.x eq 0 then begin ; new gyrotron name
				evy=ev.y
				abb=strmid(strupcase(ECHSys.Tank[ev.y].gyroname),0,3) 
				z=echcal()
				k=where(z.gyro_prefixes eq abb)
				if k[0] lt 0 then begin
					res=dialog_message('No such gyrotron as '+abb)
					ECHSys.Tank[ev.y].gyroname=''
					widget_control, info.WidIDs.ConfigTable, set_value=ECHSys.Tank
					break
				endif
				
				if ECHSys.InputTable[ev.y].PolarAng lt 10. then begin
					ECHSys.InputTable[ev.y].PolarAng=90.
					ECHSys.InputTable[ev.y].AziAng=180.
				endif
				
				enter_ECH_data, ev.y
				widget_control, info.WidIDs.ConfigTable, set_value=ECHSys.Tank
				widget_control, info.WidIDs.InputTable, set_value=ECHSys.InputTable
				i=where(ECHSys.InputTable.PolarAng gt 10., ns)
				ECHSys.NumTanks=ns

				PrgDat.Status.Recalc_Rays[ev.y]=1
				PrgDat.Status.PlotRays[ev.y]=1
				
				goto, get_polarizer_settings
				
			endif

			for ii=0, ECHSys.NumTanks-1 do if ECHSys.Tank[ii].xfrac lt 0.5 then $
				ECHSys.Tank[ii].xfrac=0.0 else ECHSys.Tank[ii].xfrac=1.0
			
			if ev.x eq 1 then begin ; frequency change
				uniq_freq=ECHSys.Tank[uniq(ECHSys.Tank.freq, sort(ECHSys.Tank.freq))].freq
				ifrq=where(uniq_freq gt 0., count)
				if count gt 0 then uniq_freq=uniq_freq[ifrq]
				if n_elements(uniq_freq) gt 3 then begin
					print, '  !!! Maximum of 3 unique frequencies allowed.'
					ECHSys.Tank=old_data
					widget_control, info.WidIDs.ConfigTable, set_value=ECHSys.Tank
					break
				endif
				; Correct divergence for new frequency:
				ECHSys.Antenna[ev.y].Divergence=1.7*(110./ECHSys.Tank[ev.y].freq)
				PrgDat.n_freq=n_elements(uniq_freq)
				PrgDat.unique_freq[*]=0.
				PrgDat.unique_freq[0:PrgDat.n_freq-1]=uniq_freq
				goto, do_the_plot	; plot new resonances
			endif

		end
		
		else: break	; ignore any other type
		
	endcase
	
end

;****************** For changing InputTable (lower) *****************************

'input_table': begin

	old_data=ECHSys.InputTable	; keep old input data in case of error
	Widget_Control, ev.id, get_value=inputdata
	ECHSys.InputTable=inputdata

	case ev.type of

		0: begin	; new data entry

			if ((ev.ch NE 10) and (ev.ch NE 13)) then break ; wait for carriage return
			
			; accept entry only for angles, counts, and polarizers			
			if ((ev.x lt 2) or (ev.x gt 7)) then begin
				ECHSys.InputTable=old_data
				widget_control, info.WidIDs.InputTable, set_value=ECHSys.InputTable
				print, '  !!! This field not editable.'
				break
			endif else ECHSys.InputTable=inputdata
			
			if PrgDat.config_shot lt PrgDat.First_Shot_Newest_Config then begin
				IF (PrgDat.Shot lt PrgDat.Prefs.first_shot_allowed) then begin	; first shot with only P2001 style launchers
					res=dialog_message(['This version of echres works only ', $
					'with shots after ' + strtrim(PrgDat.Prefs.first_shot_allowed,2) + '. ', $
					'Do you want to swith to newest configuration? ', $
					'Otherwise please use this command: ', $
					'$IDLSOURCE/echres_old/efitviewer'],/question)
				ENDIF ELSE BEGIN
				   	IF (PrgDat.Prefs.nlinputfile ne '') OR (PrgDat.Status.asked_present EQ 1) THEN res='No' ELSE BEGIN
					   IF PrgDat.Status.asked_present EQ 0 THEN BEGIN
					      res=dialog_message(["Usually it's best to 'Use present hardware config'", $
						  'when changing aiming parameters; else the counts and port limits', $
						  'may not be right. Do you want to swith to newest configuration?'],/question)
					      PrgDat.Status.asked_present = 1
					   ENDIF
					ENDELSE
				ENDELSE
				if res eq 'Yes' then goto, use_present
			endif

			; Changed angle: get new counts, clear toray calcs
			if ev.x eq 2 or ev.x eq 3 then begin
				
				; calculate new counts: first, get revised scan and crank angles
				get_mir_angles, $
					ECHSys.Antenna[ev.y].pol_id*!dtor, $
					ECHSys.Antenna[ev.y].azi_id*!dtor, $
					ECHSys.InputTable[ev.y].PolarAng*!dtor, $
					ECHSys.InputTable[ev.y].AziAng*!dtor, $
					ECHSys.Antenna[ev.y].offset_angle*!dtor, $
					pol_normal, azi_normal, $ 		; output mirror normals (rad)
					tilt, facet, scan, crank, $ 	; output angles (deg)
					ECHSys.Antenna[ev.y].antenna_inclinationd

				; and store results
				; pass the hardcoded top launch mirror normal
				IF (strpos(ECHSys.InputTable[ev.y].gyroname, 'T') eq -1) THEN BEGIN 
					ECHSys.Antenna[ev.y].pol_ang=scan
					ECHSys.Antenna[ev.y].tor_ang=crank
				ENDIF ELSE BEGIN
					IF (strpos(ECHSys.Antenna[ev.y].DWGNO, 'Top300') eq 0) THEN BEGIN 
						ECHSys.Antenna[ev.y].pol_ang=-147.3
						ECHSys.Antenna[ev.y].tor_ang=344.6-180.
					ENDIF ELSE BEGIN
;					IF (strpos(ECHSys.Antenna[ev.y].DWGNO, 'Top90') eq 0) THEN BEGIN 
						ECHSys.Antenna[ev.y].pol_ang=-147.6
						ECHSys.Antenna[ev.y].tor_ang=326.8-180.			
					ENDELSE
				ENDELSE
				
				; get scan and crank counts
				Antenna_Counts, PrgDat.config_shot, ECHSys.Antenna[ev.y].ant_num, $
					scan, crank, $				; input angles (deg)
					scan_cts, crank_cts, $		; outputs
					/antenna_num

				ECHSys.InputTable[ev.y].PolCts=scan_cts
				ECHSys.InputTable[ev.y].TorCts=crank_cts
				
			endif

			; changed counts
			if ev.x eq 4 or ev.x eq 5 then begin

				; get new scan and crank angles
				Antenna_Angles, PrgDat.config_shot, $
					ECHSys.Antenna[ev.y].ant_num, $
					scan_angle, crank_angle, $	; outputs (deg)
					ECHSys.InputTable[ev.y].polcts, $
					ECHSys.InputTable[ev.y].torcts, $
					/antenna_num

				; get new polar and azimuthal angles for launched ray
				get_reflected_angles, $
					scan_angle, crank_angle, $
					ECHSys.Antenna[ev.y].pol_id*!dtor, $
					ECHSys.Antenna[ev.y].azi_id*!dtor, $
					ECHSys.Antenna[ev.y].offset_angle*!dtor, $
					pol_r, azi_r, $ 		; output launch angles
					ECHSys.Antenna[ev.y].antenna_inclinationd, $
					antenna_style='P2001'
				ECHSys.InputTable[ev.y].PolarAng=pol_r/!dtor
				ECHSys.InputTable[ev.y].AziAng=azi_r/!dtor

			endif

			; clear the toray calcs	
			ECHSys.InputTable[ev.y].f_abs=0.
			ECHSys.InputTable[ev.y].rho_peak=0.
			ECHSys.InputTable[ev.y].eccd=0.
			ECHSys.Rays[ev.y].r[*]=0.
			ECHSys.Rays[ev.y].z[*]=0.
			ECHSys.Rays[ev.y].q_peak_r=0.
			ECHSys.Rays[ev.y].q_peak_z=0.

			widget_control, info.WidIDs.InputTable, set_value=ECHSys.InputTable
			widget_control, info.WidIDs.Archive, set_value=0
			PrgDat.Status.recalc_rays[*]=0	
			PrgDat.Status.recalc_rays[ev.y]=1
			PrgDat.Status.OutputsValid=0
			;widget_control, info.WidIDs.OutsValid, set_value=PrgDat.Status.OutputsValid
			
			; check if the pair of counts is in-bounds
			cvw=PrgDat.counts_valid_window
			
			IF cvw GE 0 THEN BEGIN  ;;; means this window has been opened before
				IF NOT windowavailable(cvw) THEN BEGIN ;;; means this window is closed by the user
					cvw = -1
					PrgDat.counts_valid_window = -1
				ENDIF
			ENDIF	
			
;			in=test_counts_valid(PrgDat.config_shot, $ ; returns true if counts valid
;				ECHSys.InputTable[ev.y].PolCts, $
;				ECHSys.InputTable[ev.y].TorCts, $
;				ECHSys.InputTable[ev.y].gyroname, cvw, ev.y)
;			PrgDat.counts_valid_window=cvw
;			if in then begin
;				widget_control, info.WidIDs.inputtable, $
;					use_table_select=[4,ev.y,5,ev.y], $
;					foreground_color=[0,0,0]
;			endif else widget_control, info.WidIDs.inputtable, $
;				use_table_select=[4,ev.y,5,ev.y], $			
;				foreground_color=[255,0,0]




			IF (strpos(ECHSys.InputTable[ev.y].gyroname, 'T') eq 0) THEN BEGIN 
				in=1 
				IF (ECHSys.InputTable[ev.y].polarang NE 162.) or (ECHSys.InputTable[ev.y].aziang NE 241.) THEN $ 
				res=dialog_message('both top launch systems have fixed aiming (Pol=162, Azi=241)')
			ENDIF ELSE BEGIN
			in=test_counts_valid(PrgDat.config_shot, $ ; returns true if counts valid
				ECHSys.InputTable[ev.y].PolCts, $
				ECHSys.InputTable[ev.y].TorCts, $
				ECHSys.InputTable[ev.y].gyroname, cvw, ev.y)
			PrgDat.counts_valid_window=cvw
			ENDELSE
			if in then begin
				widget_control, info.WidIDs.inputtable, $
					use_table_select=[4,ev.y,5,ev.y], $
					foreground_color=[0,0,0]
			endif else widget_control, info.WidIDs.inputtable, $
				use_table_select=[4,ev.y,5,ev.y], $			
				foreground_color=[255,0,0]



		end

		else:

	endcase
	
	PrgDat.Status.recalc_rays[ev.y]=1
	
	goto, do_the_plot
	
end

;************ For resetting the polarizers ************************

'getpols': begin

	get_polarizer_settings:

	if PrgDat.Prefs.nlinputfile ne '' then break	; don't know the gamma matrix

	widget_control, /hourglass

        kk=where(PrgDat.Status.PlotRays EQ 1)
	for num_kk=0, n_elements(kk)-1 do begin	
	    jj=kk[num_kk]
		if (PrgDat.Status.PlotRays[jj] and (ECHSys.InputTable[jj].PolarAng gt 10.)) then begin
		
			; if a gyro has no data for this shot, then clear the table:
			if ECHSys.Tank[jj].TankNo le 0 or ECHSys.Tank[jj].AntNum le 0 then begin
				ECHSys.InputTable[jj].xfrac=0.0
				ECHSys.InputTable[jj].polarang=0.0
				ECHSys.InputTable[jj].aziang=0.0
				ECHSys.InputTable[jj].polcts=0
				ECHSys.InputTable[jj].torcts=0
				ECHSys.InputTable[jj].pol1=0.0
				ECHSys.InputTable[jj].pol2=0.0
				ECHSys.InputTable[jj].F_abs=0.0
				ECHSys.InputTable[jj].rho_peak=0.0
				ECHSys.InputTable[jj].eccd=0.0
				goto, next_line 
			endif
		
			if ECHSys.Tank[jj].xfrac lt 0.5 then begin
				omo=1
				alpha_0=ECHSys.Antenna[jj].alpha_x+90.
				beta_0=-ECHSys.Antenna[jj].beta_x
				if alpha_0 LT -90. then alpha_0=alpha_0+180. else $ ; get alpha bet 0 and pi
					if alpha_0 GT 90. then alpha_0=alpha_0-180. 
			endif else begin
				omo=0
				alpha_0=ECHSys.Antenna[jj].alpha_x
				beta_0=ECHSys.Antenna[jj].beta_x
			endelse

	;ECHSys.XLine[jj].alpha0 and beta0 are inclination and ellipticity angle out of gyrotron, eg. 90, 0deg
			GetPolSettings, $ 
				ECHSys.XLine[jj].alpha0*!dtor, ECHSys.XLine[jj].beta0*!dtor, $
				ECHSys.XLine[jj].gm1, ECHSys.XLine[jj].gm2, $
				ECHSys.XLine[jj].mir_type, ECHSys.Xline[jj].gammasd*!dtor, $
				ECHSys.XLine[jj].delay, alpha_0*!dtor, beta_0*!dtor, $
				ECHSys.Antenna[jj].Tor_Ang*!dtor, ECHSys.Antenna[jj].Pol_Ang*!dtor, $
				ECHSys.Antenna[jj].pol_id*!dtor, polvalues

			ind=where((polvalues[3,*] gt 0.01) and (polvalues[2,*] gt 99.9) )
			if ind[0] lt 0 then $
				ind=where((polvalues[3,*] gt 0.01) and (polvalues[2,*] gt 99.75) )
			if ind[0] lt 0 then $
				ind=where((polvalues[3,*] gt 0.01) and (polvalues[2,*] gt 99.5) )
			if ind[0] lt 0 then $
				ind=where((polvalues[3,*] gt 0.01) and (polvalues[2,*] gt 99.0) )
			if ind[0] lt 0 then $
				ind=where((polvalues[3,*] gt 0.01) and (polvalues[2,*] gt 98.5) )
			if ind[0] lt 0 then $
				ind=where((polvalues[3,*] gt 0.01) and (polvalues[2,*] gt 98.0) )
			ind=where(min(polvalues[3,ind]) eq polvalues[3,ind])
			if ind[0] ge 0 then begin
				ECHSys.InputTable[jj].Pol1=polvalues[0,ind[0]]
				ECHSys.InputTable[jj].Pol2=polvalues[1,ind[0]]
				if omo then ECHSys.InputTable[jj].xfrac=100.-polvalues[2,ind[0]] else $
					ECHSys.InputTable[jj].xfrac=polvalues[2,ind[0]]
				ECHSys.Antenna[jj].alpha_a=polvalues[4,ind[0]]
				ECHSys.Antenna[jj].beta_a=polvalues[5,ind[0]]
			endif else message, 'Could not find polarizer settings for system '+strtrim(jj+1,2)
			
		endif
		next_line:
	endfor

	PrgDat.Status.OutputsValid=0
	widget_control, info.WidIDs.ConfigTable, set_value=ECHSys.Tank
	widget_control, info.WidIDs.InputTable, set_value=ECHSys.InputTable
	PrgDat.Status.ArchivedData=0	; user changed polarizers
	widget_control, info.WidIDs.Archive, set_value=PrgDat.Status.ArchivedData
		
	goto, do_the_plot
	
end
	
;********* For getting setups ***************************

'readfile': begin	; 'Read Setup File' button

	res=dialog_message(['Read in TIMCON setup file?'], /question)
	if res eq 'Yes' then setupdata=read_setup_file(setup_dir='/fusion/d3d/d3share/timcon/setup_files/') else $
	setupdata=read_setup_file(setup_dir=PrgDat.Setup_Dir)
	if not setupdata.success then begin
		message, 'Did not get setup file', /info
		break
	endif
	
	PrgDat.Setup_Dir=setupdata.setup_dir
	
	usetable=0
	
	readfile:
	
	if setupdata.shot ge PrgDat.First_Shot_Newest_Config then begin
		PrgDat.config_shot=PrgDat.First_Shot_Newest_Config+1
		widget_control, info.WidIDs.UsePresent, set_value=1
	endif else begin
		PrgDat.config_shot=setupdata.shot
		widget_control, info.WidIDs.UsePresent, set_value=0
	endelse
	
	PrgDat.Prefs.nlinputfile=''
	PrgDat.Status.ArchivedData=0
	widget_control, info.WidIDs.Archive, set_value=PrgDat.Status.ArchivedData
	
	if not usetable then begin	; enter all data from present.setup file
	
		clear_com	; clear tables for all new input	
		configdata=resolve_setup(setupdata)
		if configdata.success then fill_com, configdata, success=success else $
			message, 'resolve_setup failed'
		if not success then message, 'fill_com failed'
		
	endif else begin	; keep pol and azi angles that are present in table already
		
		; first, look for gyrotrons in the old AND present lists
		takenold=intarr(ECHSys.NumTanks)
		takenpresent=intarr(setupdata.num_sys)

		ECHSys.Tank[*].Power_MW=0

		for i=0, setupdata.num_sys-1 do begin
			k=where(strmid(strupcase(ECHSys.Tank.gyroname), 0, 3) eq $
				strmid(strupcase(setupdata.gyroname[i]),0,3), nmatch)
				
			IF nmatch EQ 1 THEN BEGIN
				takenold[k]=1
				takenpresent[i]=1		
				enter_ECH_data, k
			ENDIF				
		ENDFOR
		
		kold=where(takenold eq 0, nold)
		kpres=where(takenpresent eq 0, npres)

		;;;replacing the unavailable old gyro with the present gyro		
		IF (npres GT 0) AND (nold GT 0) THEN BEGIN
			for i=0, nold-1 do begin
				j=where(takenpresent eq 0, nj)
				IF nj gt 0 THEN BEGIN
				   ECHSys.Tank[kold[i]].gyroname=setupdata.gyroname[j[0]]
				   enter_ECH_data, kold[i]
				   takenold[kold[i]]=1
				   takenpresent[j[0]]=1
				ENDIF
			endfor
			kold=where(takenold eq 0, nold)
			kpres=where(takenpresent eq 0, npres)
		END
				
		;;;appending the left 'new' present gyro to the end of the ECHSys	
		IF (npres GT 0) AND (nold EQ 0) THEN BEGIN
			FOR i=0, npres-1 DO BEGIN
				ECHSys.Tank[n_elements(takenold)+i].gyroname=setupdata.gyroname[kpres[i]]
				ECHSys.InputTable[n_elements(takenold)+i].gyroname=setupdata.gyroname[kpres[i]]
				ECHSys.InputTable[n_elements(takenold)+i].PolarAng=90.-2*i ;;;;hardcoded an initial value for new gyro
				ECHSys.InputTable[n_elements(takenold)+i].AziAng=180. ;;;;hardcoded an initial value for new gyro
				ECHSys.XLine[n_elements(takenold)+i].alpha0=90. ;;;;hardcoded an initial value for new gyro
				PrgDat.Status.PlotRays[n_elements(takenold)+i] = 1
				PrgDat.Status.recalc_rays[n_elements(takenold)+i] = 1
				enter_ECH_data, n_elements(takenold)+i
			ENDFOR 			
		ENDIF
	
		;;;removing the gyrotrons that are not available now		
		IF (npres EQ 0) AND (nold GT 0) THEN BEGIN 
			for i=0, nold-1 do begin
				ECHSys.Tank[kold[i]].gyroname=''
				ECHSys.Tank[kold[i]].freq=0.
				ECHSys.Tank[kold[i]].nharm=0.
				ECHSys.InputTable[kold[i]].gyroname=''
				ECHSys.InputTable[kold[i]].pol1=0
				ECHSys.InputTable[kold[i]].pol2=0
				enter_ECH_data, kold[i], /empty
				PrgDat.Status.Plotrays[kold[i]]=0
			endfor	
		ENDIF
		
		ECHSys.NUMTANKS=setupdata.NUM_SYS
	
	endelse
	
	; put data into the inputs table
	widget_control, info.WidIDs.ConfigTable, set_value=ECHSys.Tank

	; designate the outputs table as invalid
	PrgDat.Status.OutputsValid=0

	; set the likely antenna numbers for ray tracing
	widget_control, info.WidIds.RayTrace, set_value=PrgDat.Status.PlotRays
	PrgDat.Status.recalc_rays[*]=1
	
	; reset nharm for DIII-D
	ECHSys.Tank[0:ECHSys.NumTanks-1].nharm=2

	widget_control, info.WidIDs.inputtable, $
		use_table_select=[4,0,5,n_elements(ECHSys.Tank)-1], $
		foreground_color=[0,0,0] 
		

	;;***************************************
	;;check whether it's within the port limits
		for i=0, setupdata.num_sys-1 do begin
			IF (strpos(setupdata.gyroname[i], 'T') eq 0) THEN BEGIN 
				in=1 
;				IF (ECHSys.InputTable[i].polarang NE 162.) or (ECHSys.InputTable[i].aziang NE 241.) THEN $ 
;				res=dialog_message('both top launch systems have fixed aiming (Pol=162, Azi=241')
			ENDIF ELSE BEGIN
			in=test_counts_valid(PrgDat.config_shot, $ ; returns true if counts valid
				ECHSys.InputTable[i].PolCts, $
				ECHSys.InputTable[i].TorCts, $
				ECHSys.InputTable[i].gyroname, -1, i+1, /noplot)
			ENDELSE
			if in then begin
				widget_control, info.WidIDs.inputtable, $
					use_table_select=[4,i,5,i], $
					foreground_color=[0,0,0]
			endif else widget_control, info.WidIDs.inputtable, $
				use_table_select=[4,i,5,i], $			
				foreground_color=[255,0,0]
		endfor
	;;***************************************
	
	if usetable then goto, get_polarizer_settings else goto, do_the_plot
	
end

'use_present': begin	; 'Use present hardware config' button pressed

	use_present:
	
	setupdata=read_setup_file(infile=PrgDat.Prefs.present_setup_path)
	if not setupdata.success then $
		message, 'Could not read file ' + PrgDat.Prefs.present_setup_path
	PrgDat.Prefs.nlinputfile=''
	
	; if no entries in InputTable.PolarAng, then use angles from file; otherwise,
	; 	use angles from table
	if max(ECHSys.InputTable.PolarAng) gt 10.0 then usetable=1 else usetable=0
	
	goto, readfile
	
end

'readmds': begin	; 'Get MDS ECH Setup' button
	
	widget_control, /hourglass
	
	strgshot=strtrim(gstruct.shot,2)
	gshot=long(strgshot.substring(0,5))
	PrgDat.config_shot=gshot
	; print, strgshot
	; print, gshot
	; print, PrgDat.config_shot

	Clear_Com	; clear tables for new input
	PrgDat.Prefs.nlinputfile=''
	read_MDS_setup, PrgDat.Config_Shot, success=success
	
	if success then begin	; get polarization purity here and put in inputtable

		; set ArchivedData button to reflect the success of the read
		PrgDat.Status.ArchivedData=1
		widget_control, info.WidIDs.Archive, set_value=PrgDat.Status.ArchivedData
		
		; update the tables
		widget_control, info.WidIDs.ConfigTable, set_value=ECHSys.Tank
		widget_control, info.WidIDs.InputTable, set_value=ECHSys.InputTable
			
		; designate the outputs table as invalid
		PrgDat.Status.OutputsValid=0
		;widget_control, info.WidIDs.OutsValid, set_value=PrgDat.Status.OutputsValid
		widget_control, info.WidIDs.Eqb, set_value=PrgDat.shottime
			
		; set the likely antenna numbers for ray tracing
		widget_control, info.WidIds.RayTrace, set_value=PrgDat.Status.PlotRays
		if PrgDat.config_shot ge PrgDat.first_shot_newest_config then $
			widget_control, info.WidIDs.UsePresent, set_value=1 else $
			widget_control, info.WidIDs.UsePresent, set_value=0

		uniq_freq=ECHSys.Tank[uniq(ECHSys.Tank.freq, sort(ECHSys.Tank.freq))].freq
		ifrq=where(uniq_freq gt 0., count)
		if count gt 0 then uniq_freq=uniq_freq[ifrq]
		if n_elements(uniq_freq) gt 3 then begin
			print, '  !!! Maximum of 3 unique frequencies allowed.'
			return
		endif
		PrgDat.n_freq=n_elements(uniq_freq)
		PrgDat.unique_freq[*]=0.
		PrgDat.unique_freq[0:PrgDat.n_freq-1]=uniq_freq
		PrgDat.status.recalc_rays[*]=1
		widget_control, info.WidIDs.inputtable, $
			use_table_select=[4,0,5,n_elements(ECHSys.Tank)-1], $
			foreground_color=[0,0,0]
		if PrgDat.counts_valid_window ge 0 then begin
			wdelete, PrgDat.counts_valid_window
			PrgDat.counts_valid_window = -1			
		endif
		
		; reset nharm for DIII-D
		ECHSys.Tank[0:ECHSys.NumTanks-1].nharm=2

		goto, do_the_plot

	endif else begin ; MDSplus data not available

		Config_Source, info ; pick alternative if desired

	endelse

end

'sendtotimcon': begin

	PrgDat.Prefs.to_timcon=1
	goto, wrt_setup_fl

end

'writefile': begin	; write the setup file

	wrt_setup_fl:

	if PrgDat.Prefs.nlinputfile ne '' then break	; don't write file for namelist inputs

	if PrgDat.config_shot lt PrgDat.First_Shot_Newest_Config then begin
		res=dialog_message(['If this file is being saved for use', $
			'as a setup for a present or future shot,', $
			'then you should have hit the "Use present hardware config" ', $
			'button to get the correct physical configuration.', $
			'Saving this file for a future shot will not get you', $
			'the configuration you probably want.', $
			'Do you want to proceed with the save?'], /question)
		if res ne 'Yes' then break
	endif
	
	mm=where(ECHSys.InputTable.xfrac gt 0.0 and ECHSys.InputTable.xfrac lt 95., countmm)
	if countmm gt 0 then begin
		res=dialog_message(['Not all systems have high X-mode fraction.', $
			'Write setup file anyway?'], /question)
		if res ne 'Yes' then break
	endif
	
	fh=dialog_pickfile(title='Name output setup file: ', filter='*.setup', $
		path=PrgDat.setup_dir, get_path=spth)
		
	if PrgDat.Prefs.to_timcon EQ 1 then $ 
	res=dialog_message('This file will be copied to your TIMCON area on atom.')
			
	if spth.StartsWith('/home/') then $
		res=dialog_message('For sharing on iris, save setup file to public accessible area, i.e., /fusion/projects/results/echres/username/')
		
	if fh ne '' then begin
		PrgDat.setup_dir=spth
		write_setup_file, fh, PrgDat.config_shot, ECHSys.InputTable, $
			user=PrgDat.Prefs.user, success=success, write_timcon=PrgDat.Prefs.to_timcon
		PrgDat.Prefs.to_timcon=0
		if not success then $
			res=dialog_message('Trouble writing the setup file')
		goto, do_the_plot
	endif
	
end

'namelistinput': begin	; read ECH input data from a namelist file

	PrgDat.Status.OutputsValid=0
	nlpath=''
	fh=dialog_pickfile(title='namelist file:', filter='*.ech', $
		path=PrgDat.nl_dir, get_path=nlpath)
	if fh[0] eq '' then begin
		print, '  !!! No namelist file selected'
		break
	endif
	PrgDat.nl_dir=nlpath
	PrgDat.Prefs.nlinputfile=fh[0]
	PrgDat.Status.RayLength=5.0
	
	inp=read_nl2(PrgDat.Prefs.nlinputfile)
	print, '  *** Reading file '+PrgDat.Prefs.nlinputfile
	eci=inp.echinputs
	ecitags=tag_names(eci)

	; clear the structures
	clear_struct, ECHSys
	clear_struct, PrgDat.Profiles
	ECHSys.Rays[*].R[*]=0.
	ECHSys.Rays[*].Z[*]=0.
	ECHSys.Rays[*].q_peak_r=0.
	ECHSys.Rays[*].q_peak_z=0.

	; enter profile data from the namelist
	k=where(ecitags eq 'NJENE')
	if k[0] lt 0 then message, 'No value for NJENE', /continue 
	k=where(ecitags eq 'RENEIN')
	if k[0] lt 0 then message, 'No value for RENEIN'
	k=where(ecitags eq 'ENEIN')
	if k[0] lt 0 then message, 'No value for ENEIN'
	k=where(ecitags eq 'NJTE')
	if k[0] lt 0 then message, 'No value for NJTE'
	k=where(ecitags eq 'RTEIN')
	if k[0] lt 0 then message, 'No value for RTEIN'
	k=where(ecitags eq 'TEIN')
	if k[0] lt 0 then message, 'No value for TEIN' 
	
	PrgDat.Profiles.renein[0:eci.njene-1]=eci.renein[0:eci.njene-1]
	PrgDat.Profiles.enein[0:eci.njene-1]=eci.enein[0:eci.njene-1]
	PrgDat.Profiles.rtein[0:eci.njte-1]=eci.rtein[0:eci.njte-1]
	PrgDat.Profiles.tein[0:eci.njte-1]=eci.tein[0:eci.njte-1]
	PrgDat.Profiles.rtiin[0:eci.njti-1]=eci.rtiin[0:eci.njti-1]
	PrgDat.Profiles.tiin[0:eci.njti-1]=eci.tiin[0:eci.njti-1]
	PrgDat.Profiles.rzeffin[0:eci.njzef-1]=eci.rzeffin[0:eci.njzef-1]
	PrgDat.Profiles.zeffin[0:eci.njzef-1]=eci.zeffin[0:eci.njzef-1]
	PrgDat.Profiles.njene=eci.njene
	PrgDat.Profiles.njte=eci.njte
	PrgDat.Profiles.njti=eci.njti
	PrgDat.Profiles.njzef=eci.njzef
	PrgDat.Profiles.CentralDensity=eci.enein[0]
	PrgDat.Profiles.CentralTe=eci.tein[0]
	PrgDat.Profiles.source='knots'
	
	PrgDat.Status.PlotRays[*]=0
	PrgDat.Status.PlotRays[0:eci.num_systems-1]=1

	IF (strpos(eci.gyrotron,'''') NE -1) THEN ECHSys.Tank[0:eci.num_systems-1].gyroname = strsplit(eci.gyrotron,'''',/extract) $
		 ELSE ECHSys.Tank[0:eci.num_systems-1].gyroname = eci.gyrotron
	ECHSys.tank[0:eci.num_systems-1].freq = eci.freq_ghz
	ECHSys.NumTanks=eci.num_systems
	ECHSys.NumAntennas=eci.num_systems
	
;	k=where(ecitags eq 'USE_ALPHABETA')
;	if k[0] ge 0 then begin
;		uab=strupcase(eci.use_alphabeta)
;		if uab eq '.T.' or uab eq '.TRUE.' or uab eq 'T' or uab eq 'TRUE' then $
;			PrgDat.Prefs.use_alphabeta=1 else PrgDat.Prefs.use_alphabeta=0 
;	endif

	for jj=0, eci.num_systems-1 do begin
		ECHSys.Antenna[jj].R_launch=eci.r_launch_m[jj]
		ECHSys.Antenna[jj].Z_launch=eci.z_launch_m[jj]
		if PrgDat.Prefs.use_alphabeta then begin
			alphabeta_to_polazi, eci.polar_deg[jj], eci.azimuthal_deg[jj], pol, azi
			ECHSys.InputTable[jj].PolarAng=pol
			ECHSys.InputTable[jj].AziAng=azi
		endif else begin
			ECHSys.InputTable[jj].PolarAng=eci.polar_deg[jj]
			ECHSys.InputTable[jj].AziAng=eci.azimuthal_deg[jj]
		endelse
		ECHSys.Tank[jj].Power_MW=eci.power_mw[jj]
		ECHSys.Antenna[jj].divergence=eci.hlwec_deg[jj]
		if eci.wrfo[jj] gt 0.5 then wrfx=0. else wrfx=1.
		ECHSys.Tank[jj].xfrac=wrfx
		ECHSys.Tank[jj].nharm=eci.nharm[jj]
	endfor
	
	; enter the general preferences 
	enter_prefs, PrgDat.Prefs.nlinputfile, PrgDat, 'ECHRES'

	; set the other profile widgets
	widget_control, info.WidIDs.ConfigTable, set_value=ECHSys.Tank
	widget_control, info.WidIDs.InputTable, set_value=ECHSys.InputTable
	widget_control, info.WidIDs.CentralDensity, set_value=PrgDat.Profiles.enein[0]
	widget_control, info.WidIDs.CentralTe, set_value=PrgDat.Profiles.tein[0]
	widget_control, info.WidIDs.Bt0, set_value=PrgDat.Profiles.Bt0
	widget_control, info.WidIDs.RayTrace, set_value=PrgDat.Status.PlotRays
	widget_control, info.WidIDs.ProfSource, set_value=4

	k=where(ecitags eq 'GAUSZONES')	
	if k[0] ge 0 then case eci.gauszones of
		1: widget_control, info.WidIDs.GaussZones, set_value=0
		3: widget_control, info.WidIDs.GaussZones, set_value=1
		4: widget_control, info.WidIDs.GaussZones, set_value=2
		else: begin
			widget_control, info.WidIDs.GaussZones, set_value=3
			print, '  !!! Set values of gausszones only to 1, 3, 4, or 7'
		endelse
	endcase
	
	PrgDat.status.recalc_rays[*]=0
	PrgDat.status.recalc_rays[0:eci.num_systems-1]=1

	PrgDat.new_shot=1
	PrgDat.save_profiles=1
	PrgDat.save_inputs=1

	goto, do_the_plot
	
end

;**************** For changing historical/present *******************

'present_config': begin ; 'Waveguide Configuration: Historical | Present' button

	if ev.select eq 0 then break
	widget_control, info.WidIDs.UsePresent, get_value=pc
	
	; if config_shot already greater than FSNC, do nothing
	if ((PrgDat.shot ge PrgDat.First_Shot_Newest_Config) or $
		(PrgDat.config_shot ge PrgDat.First_Shot_Newest_Config)) then $
		widget_control, info.WidIDs.UsePresent, set_value=1 else $
		widget_control, info.WidIDs.UsePresent, set_value=0
	
end

;********* For getting profiles ***************************

'get_profiles': begin

if ev.select eq 1 then begin
	widget_control, ev.id, get_value=profsource
	
	case profsource of
	
		0: begin	; none	
			print, '  *** Clearing profiles'
			PrgDat.Profiles.Source=''
			enein=fltarr(rad_res)
			tein=enein
			zeffin=enein
		end

		1: begin	; get zipfits

			print, '  *** Getting zipfits...'
		
			; first enter Te knots from MDS+ zip fits
			PrgDat.Profiles.Source='zipfits'
			
			te_str=get_fitted_profile(PrgDat.shot, PrgDat.time, 'etemp', ierr, /single_slice)
			if (ierr ne 0) then begin
				message, 'T_e zipfit not available.', /info
				enein=fltarr(rad_res)
				tein=enein
				zeffin=enein
				widget_control, info.WidIDs.ProfSource, set_value=0
				break
			endif

			if te_str.redchisq gt 10. then $
				print, '  !!! T_e profile has redchisq ' + $
					strtrim(string(te_str.redchisq, format='(f10.1)' ),2)

			ilto=where(te_str.rho le 1.0)
			rho=te_str.rho[ilto]
			rtein=rho_kin
			tein=spline(te_str.rho, te_str.data[ilto], rho_kin)
			njte=rad_res
		
			; fix Te profile if necessary 
			teinstr=fixte(rtein, tein,/echres)
			tein=teinstr.te
;			if teinstr.success then begin
;				tein=teinstr.te
;				if teinstr.success eq 3 then print, '  *** FixTe: Added 4 eV to Te(rho>0.92)' 
;			endif else begin
;				if teinstr.success eq 2 then $
;					message, 'Te < 10 eV inside rho=0.92; please fix Te(rho).', /info else $
;				if teinstr.success eq 4 then $
;					message, 'Te < 10 eV in more than one internal location; please fix Te(rho).', $
;						/info
;				enein=fltarr(rad_res)
;				tein=enein
;				zeffin=enein
;				widget_control, info.WidIDs.ProfSource, set_value=0
;				break
;			endelse

			; now enter density values
			ierr=0
			ne_str=get_fitted_profile(PrgDat.shot, PrgDat.time, 'edensn', ierr, /single_slice)
			IF (ierr ne 0) THEN BEGIN 
				ne_str=get_fitted_profile(PrgDat.shot, PrgDat.time, 'edens', ierr, /single_slice)
				print, '  !!! No CO2 normalization'
			ENDIF
			if (ierr ne 0) then begin
				message, 'n_e zipfit not available', /info
				enein=fltarr(rad_res)
				tein=enein
				zeffin=enein
				widget_control, info.WidIDs.ProfSource, set_value=0
				break
			endif

			if ne_str.redchisq gt 12.5 then $
				print, '  !!! n_e profile has redchisq ' + $
					strtrim(string(ne_str.redchisq, format='(f10.1)' ),2)	
			renein=rho_kin
			enein=spline(ne_str.rho, ne_str.data, rho_kin)*1.e13
			njene=rad_res
	
			; now enter zeff knots
			; too many problems with zeff from get_fitted profs
			rzeffin=rho_kin
			zeffin=fltarr(rad_res)
			zeffin[*]=1.75
			njzef=rad_res
				
		end
		
		2: begin	; get gaprofiles files
			
			print, '  *** Getting gaprofiles files'
						
			PrgDat.Profiles.Source='gaprofiles'
			
			; get profile directory
			if strmid(PrgDat.prof_dir, strlen(PrgDat.prof_dir)-1,1) ne '/' then $
				PrgDat.prof_dir += '/'
			
			; get density file
			dnefile=PrgDat.prof_dir+'dne'+PrgDat.shottime
			if not file_test(dnefile) then begin
				dnefile=dialog_pickfile(title='Pick dne file:', filter='dne*', $
					path=PrgDat.prof_dir, get_path=dfile_path)
				PrgDat.prof_dir=dfile_path
			endif
		
			if dnefile EQ '' then begin
				message, 'Did not get dne file', /info
				enein=fltarr(rad_res)
				tein=enein
				zeffin=enein
				widget_control, info.WidIDs.ProfSource, set_value=0
				break
			endif

			restore, dnefile
			renein=rho_kin
			njene=rad_res
			enein=spline(ne_str.rho_dens, ne_str.dens, renein)*1.e13

			; get dte file
			dtefile=PrgDat.prof_dir+'dte'+PrgDat.shottime
			if not file_test(dtefile) then $
				dtefile=dialog_pickfile(title='Pick dte file: ', filter='dte*', $
					path=PrgDat.prof_dir, /must_exist)
			restore, dtefile
			rtein=rho_kin
			njte=rad_res
			tein=spline(te_str.rho_te, te_str.te, rtein)
		
			; get zeff file
			rzeffin=rho_kin
			njzef=rad_res
			dimpfile=PrgDat.prof_dir+'dimp'+PrgDat.shottime+'_Carbon'
			if not file_test(dimpfile) then $
				dimpfile=dialog_pickfile(title='Optional dimp file: ', filter='dimp*', $
				path=PrgDat.prof_dir)
			if dimpfile eq '' then zeffin=replicate(1.75, rad_res) else begin
				restore, dimpfile
				zeffin=spline(impdens_str.rho_imp, impdens_str.total_zeff, rzeffin)
				ij=where(zeffin gt 4.)
				if ij[0] ge 0 then zeffin[ij]=4.0
			endelse

		end
		
		3: begin	; request input on where in MDS to get profile data
			print, '  !!! User MDSplus fits not available yet.'
			PrgDat.Profiles.Source=''
			enein=fltarr(rad_res)
			tein=enein
			zeffin=enein
		end
		
		4: begin	; namelist provides kinetic profiles--not selectable
			;if PrgDat.Prefs.nlinputfile eq '' then $
			;	widget_control, info.WidIDs.ProfSource, set_value=0 $
			;	else widget_control, info.WidIDs.ProfSource, set_value=4
			;break
			prof_nl=dialog_pickfile(title='Profile namelist file')
			
			if (prof_nl ne '' and file_test(prof_nl)) then begin
				profs=read_nl2(prof_nl)
				prof_tags=tag_names(profs.profile_data)
				ip=where(prof_tags eq 'ENEIN')
				if ip[0] lt 0 then begin
					print, '  !!! profile namelist must include ENEIN'
					goto, skip_prof_entry
				endif
				ip=where(prof_tags eq 'TEIN')
				if ip[0] lt 0 then begin
					print, '  !!! profile namelist must include TEIN'
					goto, skip_prof_entry
				endif
				enein=spline(profs.profile_data.renein[0:profs.profile_data.njene-1], $
					profs.profile_data.enein[0:profs.profile_data.njene-1], rho_kin)
				tein=spline(profs.profile_data.rtein[0:profs.profile_data.njte-1], $
					profs.profile_data.tein[0:profs.profile_data.njte-1], rho_kin)
				ip=where(prof_tags eq 'ZEFFIN')
				if ip[0] lt 0 then begin
					print, '  *** Using constant value of 2.0 for Zeff'
					zeffin = replicate(2.0, n_elements(rho_kin))
				endif else $
					zeffin=spline(profs.profile_data.rzeffin[0:profs.profile_data.njzef-1], $
						profs.profile_data.zeffin[0:profs.profile_data.njzef-1], rho_kin)
			endif else begin
				res=dialog_message('Profile namelist file must exist; no changes')
				PrgDat.Profiles.Source=''
				goto, skip_prof_entry
			endelse	

		end
		
	endcase
	
	; check that the outer part of enein is monotonically decreasing, 
	; so that toray doesn't make ray go out instead of in
	for ik=rad_res-4, rad_res-2 do $
		if enein[ik+1] ge enein[ik] then enein[ik+1]=0.9*enein[ik]	
	
	; enter the data into com
	PrgDat.Profiles.njte=rad_res
	PrgDat.Profiles.njene=rad_res
	PrgDat.Profiles.njzef=rad_res
	PrgDat.Profiles.renein[*]=rho_kin
	PrgDat.Profiles.rtein[*]=rho_kin
	PrgDat.Profiles.rzeffin[*]=rho_kin
	PrgDat.Profiles.enein[*]=0.
	PrgDat.Profiles.tein[*]=0.
	PrgDat.Profiles.zeffin[*]=0.
	PrgDat.Profiles.enein[0:rad_res-1]=enein
	PrgDat.Profiles.tein[0:rad_res-1]=tein
	PrgDat.Profiles.zeffin[0:rad_res-1]=zeffin
	PrgDat.Profiles.CentralDensity=PrgDat.Profiles.enein[0]
	PrgDat.Profiles.InputDensity=PrgDat.Profiles.CentralDensity
	PrgDat.Profiles.CentralTe=PrgDat.Profiles.tein[0]
	PrgDat.Profiles.InputTe=PrgDat.Profiles.CentralTe
	PrgDat.Profiles.ShotCentralDensity=enein[0]
	PrgDat.Profiles.ShotCentralTe=tein[0]
	PrgDat.Status.OutputsValid=0	; check if cutoffs should be calculated
	
	skip_prof_entry:
	widget_control, info.WidIDs.CentralDensity, set_value=PrgDat.Profiles.CentralDensity
	widget_control, info.WidIDs.CentralTe, set_value=PrgDat.Profiles.CentralTe
	
	goto, do_the_plot

endif
		
end

'CentralDensity': begin	; user changed central density

	if PrgDat.Profiles.CentralDensity lt 1. then begin
		; change density only if it is already set to nonzero value
		widget_control, info.WidIDs.CentralDensity, set_value=0.0
		result=dialog_message('Get profiles before changing central density.')
	endif else begin
		widget_control, ev.id, get_value=CDen
		PrgDat.Profiles.InputDensity=CDen
		PrgDat.Profiles.enein=PrgDat.Profiles.enein * $	; adjust knot values
			(PrgDat.Profiles.InputDensity/PrgDat.Profiles.CentralDensity)
		PrgDat.Profiles.CentralDensity=PrgDat.Profiles.enein[0]
		PrgDat.Status.OutputsValid=0
		;widget_control, info.WidIDs.OutsValid, set_value=PrgDat.Status.OutputsValid
		PrgDat.Status.ArchivedData=0
		widget_control, info.WidIDs.Archive, set_value=PrgDat.Status.ArchivedData
		goto, do_the_plot	; in case fuh needs to be contoured
	endelse
	
end

'CentralTe': begin	; user changed central density

	if PrgDat.Profiles.CentralTe eq 0. then begin
		; change Te only if it is already set to nonzero value
		widget_control, info.WidIDs.CentralTe, set_value=0.0
		result=dialog_message('Hit Profiles first to get kinetic profiles.')
	endif else begin
		widget_control, ev.id, get_value=CTe
		PrgDat.Profiles.InputTe=CTe
		PrgDat.Profiles.tein=PrgDat.Profiles.tein * $	; adjust knot values
			(PrgDat.Profiles.InputTe/PrgDat.Profiles.CentralTe)
		if PrgDat.Prefs.debug then message, /info, '  Multiplying Te profile by ' + $
			strtrim(PrgDat.Profiles.InputTe,2) + ' / ' + strtrim(PrgDat.Profiles.CentralTe,2)
		PrgDat.Profiles.CentralTe=PrgDat.Profiles.tein[0]
		PrgDat.Status.OutputsValid=0
		PrgDat.Status.ArchivedData=0
		widget_control, info.WidIDs.Archive, set_value=PrgDat.Status.ArchivedData
		goto, do_the_plot
	endelse

end

'Bt0': begin

	widget_control, ev.id, get_value=CBt0
	PrgDat.Profiles.Bt0=CBt0
	gstruct=scale_equilibrium(gstruct, PrgDat.Profiles.Bt0/gstruct.bcentr)
	PrgDat.gfile=PrgDat.gdir+'g'+PrgDat.shottime 
	if strmid(PrgDat.gfile, strlen(PrgDat.gfile)-6, 6) ne '_newBt' then $
		PrgDat.gfile += '_newBt'
	writeg_file, gstruct, file=PrgDat.gfile
	gstruct.source=PrgDat.gfile
	PrgDat.Status.ArchivedData=0
	widget_control, info.WidIDs.Archive, set_value=PrgDat.Status.ArchivedData
	PrgDat.new_shot=1	; need to recompute cold resonances
;	widget_control, info.WidIDs.UsePresent,get_value=present
;	If present THEN PrgDat.config_shot=PrgDat.First_Shot_Newest_Config
	widget_control, info.WidIDs.UsePresent,get_value=present
	If present AND (PrgDat.config_shot LT PrgDat.First_Shot_Newest_Config) THEN PrgDat.config_shot=PrgDat.First_Shot_Newest_Config+1
	PrgDat.Status.OutputsValid=0
	PrgDat.save_profiles=1
	PrgDat.save_inputs=1
	PrgDat.status.recalc_rays[*]=1

	goto, do_the_plot
	
end	

'plot_profiles': begin
	ppdirectory=PrgDat.gdir
	widget_control, info.WidIDs.ProfSource, get_value=psource	
	plot_profiles, ppdirectory, psource
end

;********** For GUI info/control **********************************

'archive': begin
	widget_control, ev.id, get_value=dont_care	; want it non-editable
	widget_control, info.WidIDs.Inputs_Archived, set_value=PrgDat.Status.ArchivedData
end

'RayTrace': begin
	widget_control, ev.id, get_value=trace
	i=where(ECHSys.InputTable.PolarAng lt 10.)
	if i[0] ge 0 then trace[i]=0
	PrgDat.Status.PlotRays=trace
	widget_control, ev.id, set_value=PrgDat.Status.PlotRays
	PrgDat.Status.OutputsValid=0
	goto, do_the_plot
end

'PlotAll': begin
	i=where(ECHSys.InputTable.PolarAng gt 10.)
	PrgDat.Status.PlotRays[*]=0
	if i[0] ge 0 then PrgDat.Status.PlotRays[i]=1
	widget_control, info.WidIDs.RayTrace, set_value=PrgDat.Status.PlotRays
	goto, do_the_plot
end

'PlotOne': begin
	PrgDat.Status.PlotRays[*]=0
	PrgDat.Status.PlotRays[0]=1
	widget_control, info.WidIDs.RayTrace, set_value=PrgDat.Status.PlotRays
	goto, do_the_plot
end

'plotit': begin
	
	do_the_plot:

	;IF DENSITY CALCULATOR IS OPEN, SKIP RE-PLOT (refreshes ECHRES panel back to initial settings -PN 04/13/23)
	if ISA(stash) eq 1 then begin
		dens_calc_open = WIDGET_INFO(stash.DensCalcBase, /VALID_ID)
	endif else begin
		dens_calc_open = 0
	endelse

	if dens_calc_open ne 1 then begin
		; Replot the equilibrium with the new change if there is a callback routine.
		if info.routine ne '' then begin
			if Ptr_Valid(info.ptr_para) then $
				call_procedure, info.routine, *info.ptr_para else $
				call_procedure, info.routine
		endif
	endif

	; put the calculated values into the output table:
	widget_control, info.WidIDs.InputTable, set_value=ECHSys.InputTable
	widget_control, info.WidIDs.Eqb, set_value=PrgDat.shottime
	widget_control, info.WidIDs.CentralDensity, set_value=PrgDat.Profiles.CentralDensity
	widget_control, info.WidIDs.CentralTe, set_value=PrgDat.Profiles.CentralTe
	widget_control, info.WidIDs.Bt0, set_value=PrgDat.Profiles.Bt0
	PrgDat.Status.OutputsValid=1
	;widget_control, info.WidIds.OutsValid, set_value=PrgDat.Status.OutputsValid
	
	if PrgDat.show_toray_window then begin
		PrgDat.show_toray_window=0
		wshow, PrgDat.toray_window
	endif

end

'auto_aim': begin
	cd, PrgDat.gdir
	auto_aimer, info.WidIDs
end

'closewindow': widget_control, ev.top, /destroy

'help': window_text_gaplot, PrgDat.Prefs.program_help_path

'wdebug': if PrgDat.Prefs.debug eq 0 then PrgDat.Prefs.debug=1B else PrgDat.Prefs.debug=0B

'wstop': stop

'dump_echsys': begin
	svfile=PrgDat.gdir+'ECHSys_'+strtrim(gstruct.time,2)+'.sav'
	save, ECHSys, file=svfile
	print, '  *** Wrote ECHSys data to save file '+svfile
end

'WriteInone': begin 	; Write global inone file from InputTable and profiles

	if PrgDat.Prefs.nlinputfile ne '' then begin
		print, '  !!! Cannot write inone file if input is via a namelist file'
		break
	endif
	
	widget_control, /hourglass
	
	;if PrgDat.Profiles.enein[0] eq 0. then begin
	;	res=dialog_message('Get kinetic profiles first')
	;	goto, skip_inone_global
	;endif	
  
	if PrgDat.Profiles.enein[0] eq 0. then begin
		res=dialog_message(['Write inone file without kinetic profiles?'], /question)
		IF res eq 'No' THEN BEGIN
			res=dialog_message('Get kinetic profiles first')			
			goto, skip_inone_global
		ENDIF
	endif	
	
	PrgDat.write_global_inone=1

	; Read template file
	fh=file_test(PrgDat.inone_template)
	if not fh then begin
		res=dialog_message('Did not get inone template file: ' + PrgDat.inone_template)
		goto, skip_inone_global
	endif
	nlg=read_nl2(PrgDat.inone_template)
	tgs=tag_names(nlg)
	inl=where(tgs eq 'NODATA', ninl)
	if ninl gt 0 then begin
		res=dialog_message('Did not correctly read inone template: ' + $
			PrgDat.inone_template)
		goto, skip_inone_global
	endif
	
	; header
	nlg.header[1:*]=''
	nlg.header[n_elements(nlg.header)-1]='----------------------------------------'
	igh=1
	
	; ***** enter the ECH data *****
	; inone used only for onetwo, and onetwo runs only on 64-bit computers 
	; set toray_path in inone appropriately
	nlg.namelis2.toray_path="'"+PrgDat.toray_path+"'"
	
	nlg.namelis2.rfon[*]=1.e6
	nlg.namelis2.rftime[*]=-1.
	nlg.namelis2.rfpow[*]=0.
	nsys=ECHSys.NumTanks
	n_rho_knots=rad_res    ;changed by xc on April 9 2016, orginally hardcoded to 101
	instantECpower=''
	
	iec=where(ECHSys.Tank.power_MW gt 0., niec)
	if niec gt 0 then begin

		nlg.namelis2.rfpow[0:nsys-1]=ECHSys.Tank[0:nsys-1].power_MW*1.e6
		nlg.namelis2.freq[0:nsys-1]=ECHSys.Tank[0:nsys-1].freq*1.e9
;		nlg.namelis2.wrfo[0:nsys-1]=1.-ECHSys.Tank[0:nsys-1].xfrac
		nlg.namelis2.wrfo[0:nsys-1]=round(1.-ECHSys.InputTable[0:nsys-1].xfrac/100.)
		nlg.namelis2.hlwec[0:nsys-1]=ECHSys.Antenna[0:nsys-1].divergence
		nlg.namelis2.thetec[0:nsys-1]=ECHSys.InputTable[0:nsys-1].PolarAng
		nlg.namelis2.xec[0:nsys-1]=ECHSys.Antenna[0:nsys-1].r_launch*100.
		nlg.namelis2.zec[0:nsys-1]=ECHSys.Antenna[0:nsys-1].z_launch*100.

		; if Ip negative, set irfcur=-1 and phaiec to 360.-phaiec, in order
		;  to account for onetwo being hardwired for Ip>0
		if gstruct.cpasma ge 0. then begin
			nlg.namelis2.phaiec[0:nsys-1]=ECHSys.InputTable[0:nsys-1].AziAng
			nlg.namelis2.irfcur[*]=1.0
		endif else begin
			nlg.namelis2.phaiec[0:nsys-1]=360.-ECHSys.InputTable[0:nsys-1].AziAng
			nlg.namelis2.irfcur[*]=-1.0
		endelse
		
		; now on and pulse length times
		e=get_ech(PrgDat.shot, PrgDat.time)
		itime=nearest_index(e.time, PrgDat.time)
		dtime=e.time[1]-e.time[0]
		navtime=fix(PrgDat.Prefs.pow_avg_half_period/dtime)
		for kkk=0, e.num_systems-1 do begin
			avpow=median(e.systems[kkk].pinj, 2*navtime+1)
			instantECpower += strtrim(avpow[itime],2) + '    '
		endfor

		nti=n_elements(e.time)
		for ii=0, nsys-1 do begin
			smpow=median(e.systems[ii].pinj, 21); avg over +/-1.05 msec for digitizing rate of 0.05 sec
			igad=where(smpow gt 50000., count)
			if count gt 100 then begin	; on for at least 5 msec
				ton=e.time[igad[0]]/1000.	; want seconds not msec
				if igad[count-1] lt (nti-1) then toff=e.time[igad[count-1]]/1000. else toff=e.time[nti-1]/1000.
				nlg.namelis2.rfon[ii]=ton
				nlg.namelis2.rftime[ii]=toff-ton
				avpow=total(smpow[igad])/float(count)	; power averaged over pulse length
				nlg.namelis2.rfpow[ii]=avpow	; entering average power, not instantaneous power
			endif else begin
				nlg.namelis2.rfon[ii]=1.e6
				nlg.namelis2.rftime[ii]=-1.0
				nlg.namelis2.rfpow[ii]=0.0
			endelse
		endfor
		
	endif
	
	; ***** enter the kinetic profiles *****
	
	; electron density
	nlg.namelis1.enein[*]=0.
	nlg.namelis1.renein[*]=0.
	nlg.namelis1.njene=PrgDat.Profiles.njene
	nlg.namelis1.enein[0:PrgDat.Profiles.njene-1]= $
		PrgDat.Profiles.enein[0:PrgDat.Profiles.njene-1]
	nlg.namelis1.renein[0:PrgDat.Profiles.njene-1]= $
		PrgDat.Profiles.renein[0:PrgDat.Profiles.njene-1]
	
	; electron temperature
	nlg.namelis1.tein[*]=0.
	nlg.namelis1.rtein[*]=0.
	nlg.namelis1.njte=PrgDat.Profiles.njte
	nlg.namelis1.tein[0:PrgDat.Profiles.njte-1]= $
		PrgDat.Profiles.tein[0:PrgDat.Profiles.njte-1]
	nlg.namelis1.rtein[0:PrgDat.Profiles.njte-1]= $
		PrgDat.Profiles.rtein[0:PrgDat.Profiles.njte-1]
	
	; zeff
	nlg.namelis1.zeffin[*]=0.
	nlg.namelis1.rzeffin[*]=0.
	nlg.namelis1.njzef=PrgDat.Profiles.njzef
	nlg.namelis1.zeffin[0:PrgDat.Profiles.njzef-1]= $
		PrgDat.Profiles.zeffin[0:PrgDat.Profiles.njzef-1]
	nlg.namelis1.rzeffin[0:PrgDat.Profiles.njzef-1]= $
		PrgDat.Profiles.rzeffin[0:PrgDat.Profiles.njzef-1]
		
	case 1 of
	
		(PrgDat.Profiles.source eq 'zipfits'): begin

			; ion temperature
			ierr=0
			ti_str=get_fitted_profile(PrgDat.shot, PrgDat.time, 'itemp', ierr, /single_slice)
			if (ierr ne 0) then begin
				print, 'T_i zipfit not available; using T_i=T_e.'
				nlg.header[++igh]='T_i zipfit not available; using T_i=T_e'				
				njti=nlg.namelis1.njte
				tiin=nlg.namelis1.tein[0:njti-1]
				rtiin=nlg.namelis1.rtein[0:njti-1]
			endif else begin
				if ti_str.redchisq gt 10. then $
					print, '  !!! T_i profile has redchisq ' + $
					strtrim(string(ti_str.redchisq, format='(f10.1)' ),2)
				ilto=where(ti_str.rho le 1.0)
				rho=ti_str.rho[ilto]
				knot_inds=fix(n_elements(rho)/(n_rho_knots-1))*indgen(n_rho_knots)
				rtiin=rho[knot_inds]
				tiin=ti_str.data[knot_inds] + 0.005	; add 5 eV to avoid neg knots
				njti=n_elements(rtiin)
				ibad=where(tiin lt 0., count)
				if count gt 0 then begin
					res=dialog_message('Note: Ti profile has negative values.')
					print, '  !!! T_i profile has negative values.'
				endif
			endelse
			
			; angular rotation
			trot_ierr=1
			trot_str=get_fitted_profile(PrgDat.shot, PrgDat.time, 'trot', $
				trot_ierr, /single_slice)
			if trot_ierr ne 0 then begin
				print, '  !!! No toroidal rotation zipfit; trot_ierr=', trot_ierr, $
					' -- continuing with trot=0.'
				nlg.header[++igh]='No toroidal rotation zipfit; continuing with trot=0.'
				rangrot = rho_kin
				angrotin = fltarr(rad_res)
			endif else begin
				ilto=where(trot_str.rho le 1.0)
				rho=trot_str.rho[ilto]
				knot_inds=fix(n_elements(rho)/(n_rho_knots-1))*indgen(n_rho_knots)
				rangrot=rho[knot_inds]
				angrotin=trot_str.data[knot_inds]*1000.	; krad/s to rad/s
			endelse

			PrgDat.Profiles.njti = njti
			PrgDat.Profiles.rtiin[*] = 0.
			PrgDat.Profiles.tiin[*] = 0.
			PrgDat.Profiles.rtiin[0:njti-1] = rtiin[0:njti-1]
			PrgDat.Profiles.tiin[0:njti-1] = tiin[0:njti-1]
			PrgDat.Profiles.angrotin[*]=0.
			PrgDat.Profiles.rangrot[*]=0.
			if gstruct.cpasma gt 0. then $
				PrgDat.Profiles.angrotin[0:n_elements(angrotin)-1]=angrotin else $
				PrgDat.Profiles.angrotin[0:n_elements(angrotin)-1]=-1.*angrotin
			PrgDat.Profiles.rangrot[0:n_elements(rangrot)-1]=rangrot			

		end
	
		(PrgDat.Profiles.source eq 'gaprofiles'): begin
		
			; get dti file if available
			dtifile=PrgDat.prof_dir+'dti'+PrgDat.shottime
			fh=file_test(dtifile)
			if fh then begin	; found dti file
				restore, dtifile
				rtiin=ti_str.fit.knotloc
				tiin=ti_str.fit.knotval
				njti=n_elements(rtiin)
			endif else begin
				njti=nlg.namelis1.njte
				tiin=nlg.namelis1.tein[0:njti-1]
				rtiin=nlg.namelis1.rtein[0:njti-1]
				print, '  !!! echres: no dti file found, using Ti=Te'
				nlg.header[++igh]='No dti file found, using Ti=Te'
			endelse

			; get toroidal rotation file
			trotfile=PrgDat.prof_dir+'dtrot'+PrgDat.shottime
			fh=file_test(trotfile)
			if not fh then trotfile=dialog_pickfile(title='Pick dtrot file: ', filter='dtrot*', $
				path=PrgDat.prof_dir)
			fh=file_test(trotfile)
			if fh then begin	; found trot file
				restore, trotfile
				angrotin=tor_rot_str.fit.knotval
				rangrot=tor_rot_str.fit.knotloc
			endif else begin
				print, '  !!! Using toroidal rotation = 0'
				nlg.header[++igh]='Using toroidal rotation = 0'
				angrotin=fltarr(11)
				rangrot=findgen(11)/10.
			endelse

			PrgDat.Profiles.njti = njti
			PrgDat.Profiles.rtiin[*] = 0.
			PrgDat.Profiles.tiin[*] = 0.
			PrgDat.Profiles.rtiin[0:njti-1] = rtiin[0:njti-1]
			PrgDat.Profiles.tiin[0:njti-1] = tiin[0:njti-1]
			PrgDat.Profiles.angrotin[*]=0.
			PrgDat.Profiles.rangrot[*]=0.
			if gstruct.cpasma gt 0. then $
				PrgDat.Profiles.angrotin[0:n_elements(angrotin)-1]=angrotin else $
				PrgDat.Profiles.angrotin[0:n_elements(angrotin)-1]=-1.*angrotin
			PrgDat.Profiles.rangrot[0:n_elements(rangrot)-1]=rangrot			

		end

		(PrgDat.Profiles.source eq ''): begin
			print, '!!! Writing inone file without kinetic profiles'
			njti=rad_res
			tiin=fltarr(njti)
			rtiin=rho_kin
			angrotin=fltarr(njti)
			rangrot=rho_kin				
		end
		
		else: begin
			res=dialog_message('Problem getting profiles')
			goto, skip_inone_global
		end
	endcase
	
	nlg.namelis1.njti=njti
	nlg.namelis1.tiin[*]=0.
	nlg.namelis1.rtiin[*]=0.
	nlg.namelis1.tiin[0:njti-1]=tiin[0:njti-1]
	nlg.namelis1.rtiin[0:njti-1]=rtiin[0:njti-1]
	nlg.namelis1.angrotin[*]=0.
	nlg.namelis1.rangrot[*]=0.
	if gstruct.cpasma gt 0. then $
		nlg.namelis1.angrotin[0:n_elements(angrotin)-1]=angrotin else $
		nlg.namelis1.angrotin[0:n_elements(angrotin)-1]= -1.*angrotin
	nlg.namelis1.rangrot[0:n_elements(angrotin)-1]=rangrot
	
	; get radiated power
	print, '  *** Getting radiated power...'
	nqrad=rad_res   ;modified by Xi Chen on April 9, 2016 originally hardcoded to 101
	qradr=get_range(0.,1.,nqrad)
	qradin=fltarr(nqrad)

	if PrgDat.Profiles.source eq 'gaprofiles' then begin	
		pradfile=PrgDat.prof_dir+'prad'+PrgDat.shottime
		fh=file_test(pradfile)	; first look for file
		if not fh then pradfile=dialog_pickfile(title='Pick prad file: ', filter='prad*', $
			path=PrgDat.prof_dir)
		if file_test(pradfile) then begin
			restore, pradfile
			qradin=spline(prad_str.prad_rho, prad_str.prad_prof, qradr)
		endif
	endif else if (PrgDat.Profiles.source eq 'zipfits') then begin
	
		mdsopen, 'd3d', PrgDat.shot
		intprad_core_time=mdsvalue('dim_of(\spectroscopy::prad_core, 0)', status=success, /quiet)
		if success then begin
			prad_core_rho=mdsvalue('dim_of(\spectroscopy::bolfit_corprof,0)')
			prad_core_time=mdsvalue('dim_of(\spectroscopy::bolfit_corprof,1)')
			prad_core=mdsvalue('\spectroscopy::bolfit_corprof')
			ipr=nearest_index(prad_core_time, PrgDat.time, diff=diff)
			if diff gt 10. then print, '     Diff = ' + strtrim(diff,2) + ' msec'
			ipr=ipr[0]
			qradin=spline(prad_core_rho[*,ipr], prad_core[*,ipr], qradr) > 0
		endif
	endif

	if max(abs(qradin)) lt 1.e-6 then begin 
		print, '  *** No Prad profile file or zipfit this shot.time; continuing with Prad=0'
		nlg.header[++igh]='No Prad file or zipfit this shot.time; continuing with Prad=0'
	endif
	nlg.namelis2.qradin[*]=0.
	nlg.namelis2.qradr[*]=0.
	nlg.namelis2.qradin[0:nqrad-1]=qradin
	nlg.namelis2.qradr[0:nqrad-1]=qradr
	nlg.namelis2.nqrad=nqrad
	
	; ***** Neutral beam power *****
	print, '  *** Getting NBI data...'
	nb=nbi12_8sources(PrgDat.shot, PrgDat.time)
	if nb.err ne 0 then begin
		print, '  !!! No NBI data returned for this shot for global inone file.'
		goto, skip_inone_global
	endif 

	nlg.namelis2.sfrac1=nb.nbi.sfrac1
	nlg.namelis2.ebkev=nb.nbi.ebkev
	nlg.namelis2.bptor=nb.nbi.bptor
	nlg.namelis2.angleh=nb.nbi.angleh
	nlg.namelis2.anglev=-nb.nbi.anglev	; for nfreya, tilt downward has - sign
	nlg.namelis2.rpivot=nb.nbi.rpivot
	nlg.namelis2.zpivot=nb.nbi.zpivot
	nlg.namelis2.blenp$3=nb.nbi.blenp[2]
	nlg.namelis2.blenp$4=nb.nbi.blenp[3]
	nlg.namelis2.iterate_beam='.false.'
	for i=0,7 do nlg.namelis2.fbcur[3*i:3*i+2]=nb.nbi.fbcur[*,i]

	if gstruct.cpasma lt 0. then nlg.namelis2.angleh *= -1.

	; onetwo uses timing of first beam for all beams. Make sure it's on, even at low power.
	; If other beams are off at time then turn power down
	nlg.namelis2.beamon=nb.nbi.beamon
	nlg.namelis2.btime=nb.nbi.btime
	i=where(((nlg.namelis2.beamon gt PrgDat.time/1000.) or $
		(nlg.namelis2.beamon + nlg.namelis2.btime lt PrgDat.time/1000.)), ni)
	if ni gt 0 then nlg.namelis2.bptor[i]=1.
	i=where(nlg.namelis2.bptor gt 1., ni)
	if ni eq 0 then begin
		nlg.namelis2.beamon[0]=99.0
		nlg.namelis2.btime[0]=1.
	endif else begin
		nlg.namelis2.beamon[0]=0.0
		nlg.namelis2.btime[0]=99.
	endelse
	
	; keep onetwo happy
	i=where(nlg.namelis2.bptor eq 0.0, count)
	if count gt 0 then nlg.namelis2.bptor[i]=1. ; 1 Watt better than no Watt
	
	; notify user if H is in a beam
	i=where(nb.nbi.abeam ne 2., ni)
	if ni gt 0 then begin
		print, '  !!! Some beams are not in deuterium. '
		print, '       Changes are needed in inone to reflect this.'
	endif

	nlg.namelis2.ishot=PrgDat.shot
	nlg.namelis2.itime=PrgDat.time
	nlg.namelis1.bctime=PrgDat.time/1000.
	nlg.namelis1.time0=PrgDat.time/1000.
	nlg.namelis1.timmax=PrgDat.time/1000.
	nlg.namelis3.eqdskin="'"+PrgDat.gfile+"'"

	; Write the header data
	nlg.header[igh++]='! inone template: ' + PrgDat.inone_template
	nlg.header[igh++]='Gyrotron systems (in order of inone entries): '
	for kk=0, nsys-1 do if ECHSys.Tank[kk].gyroname ne '' then nlg.header[igh++] = $
		ECHSys.Tank[kk].gyroname + ' in tank ' + $
		strtrim(ECHSys.Tank[kk].TankNo,2) + ' on antenna ' + $
		strtrim(ECHSys.Tank[kk].AntNum,2)
	nlg.header[igh++]='  !!! ECH powers in namelis2 are powers AVERAGED OVER THE PULSE LENGTH !!!'
	nlg.header[igh++]='To use instantaneous power at '+strtrim(PrgDat.time,2)+' msec, substitute the following line:'
	nlg.header[igh++]=' RFPOW =  ' + instantECpower
	nlg.header[igh++]='(These instantaneous powers at ' + strtrim(nlg.namelis2.itime,2) + $
		' msec are averaged over +/- ' + $
		strtrim(PrgDat.Prefs.pow_avg_half_period,2) + ' msec)'
	if gstruct.cpasma lt 0. then begin
		nlg.header[igh++]=''
		nlg.header[igh++]='!!! This eqdsk has Ip<0: Since ONETWO has Ip>0, we compensated: '
		nlg.header[igh++]='  For ECH, set IRFCUR = -1.0  and PHAIEC -> 360 - PHAIEC'
		nlg.header[igh++]='  For NBI, set ANGLEH = -ANGLEH and ANGROTIN = -ANGROTIN'
	endif

	; write inone_global
	file=PrgDat.gdir+'inone'
	write_nl2, nlg, file, delim='&', /slashterm
	file_copy, file, file+'_global', /overwrite ; for backward compatibility
	print, '  *** Wrote inone file '+file

	skip_inone_global:

end


; ************* For toray and cql3d ******************************

'Num_Rays': begin
	widget_control, ev.id, get_value=gzones
	case gzones of
		0: PrgDat.Prefs.gauss_zones=1	;2: 6rays, 3: 18rays, 4: 30rays
		1: PrgDat.Prefs.gauss_zones=3	;5: 50rays, 6: 74rays, 7:100rays, 9: 180rays
		2: PrgDat.Prefs.gauss_zones=4	
		3: PrgDat.Prefs.gauss_zones=7
		else: PrgDat.Prefs.gauss_zones=4
	endcase
end


'Idamp_val': begin
	widget_control, ev.id, get_value=idampv
	case idampv of
		0: PrgDat.Prefs.idamp=2	
		1: PrgDat.Prefs.idamp=8	
		else: PrgDat.Prefs.idamp=8
	endcase
end


'run_Toray': begin

	widget_control, /hourglass

	;close the 'TORAY Plot Window' launched from previous run
	pltorayid=LookupManagedWidget('plot_toray')
	IF pltorayid NE 0L THEN widget_control,pltorayid,/destroy
	
	; kill cql3d window and clear data, if open
	if xregistered('plot_cql_echres') then begin
		widget_control, PrgDat.cql_windowID, /destroy
		PrgDat.cql_pwr[*]=0.
		PrgDat.cql_cur[*]=0.
		PrgDat.cql_rya[*]=0.
		PrgDat.cql_surf=10
		PrgDat.cql_totpwr=0.
		PrgDat.cql_totcur=0.
		PrgDat.cql_mode=0
		PrgDat.cql_window=-1
		PrgDat.cql_b_fudge_factor=1.0
		PrgDat.cql_plotresonance=1
		PrgDat.cql_quadrant=0
		PrgDat.cql_done=0
	endif

	;close the opened 'Unabsorbed EC Power on the Wall' window when'run_Toray' is clicked
	raywallid=LookupManagedWidget('plot_rayonwall')
	IF raywallid NE 0L THEN widget_control,raywallid,/destroy

	; first check density and setup
	if PrgDat.Profiles.enein[0] lt 1. then begin
		res=dialog_message('Need to obtain kinetic profiles before running toray')
		break
	endif
	if max(ECHSys.Tank.freq) eq 0 then begin
		res=dialog_message('Need to obtain ECH setup before running toray.')
		break	
	endif

	; call toray
	tordata=run_toray_echres(/save_data)
	if not tordata.success then begin
		res=dialog_message('Toray failed to run.')
		break
	endif

	; store some data
	inds=tordata.toray_data.system_number-1
	ECHSys.InputTable[*].f_abs=0.
	ECHSys.InputTable[*].eccd=0.
	ECHSys.InputTable[*].rho_peak=0.
	ECHSys.InputTable[inds].f_abs=tordata.toray_data.f_abs
	ECHSys.InputTable[inds].rho_peak=tordata.toray_data.rho_peak

	ECHSys.InputTable[inds].eccd= $
		tordata.toray_data.eccd*ECHSys.Tank[inds].Power_MW*1000.
	widget_control, info.WidIDs.InputTable, set_value=ECHSys.InputTable
	
	plot_toray
	
	PrgDat.show_toray_window=1

	goto, do_the_plot

end

'find_dens_lim': begin	; find density where absorption less than 95%

	if PrgDat.gfile eq '' then begin
		res=dialog_message('Pick equilibrium first')
		break
	endif
	if ECHSys.NumTanks eq 0 then begin
		res=dialog_message('Get ECH setup first')
		return
	endif
	if PrgDat.Profiles.enein[0] lt 1. then begin
		res=dialog_message('Pick kinetic profiles first')
		break
	endif

	print, ''
	print, '  *** Getting density limit for 98% absorption...'
	widget_control, /hourglass
	t00=systime(1)
	maxtries=15

	trial_densities=fltarr(maxtries)
	sys_to_run=PrgDat.Status.Plotrays
	nsys_to_run=n_elements(sys_to_run)
	calc=where(sys_to_run gt 0, nsys_to_calc)
	
	fabs=fltarr(nsys_to_run, maxtries)
	startne=6.0e13
	step=1.0 & direction=1.0 & itnum=0 & focus=0B
;;;	IF nsys_to_calc EQ 1 THEN focus=1B

	for iii=0, maxtries-1 do begin	; iii is tries
	
		IF (iii eq 0) THEN BEGIN 
		    trial_densities[iii]=startne 
		ENDIF ELSE BEGIN
		    trial_densities[iii] = trial_densities[iii-1]+step*direction*1.e13
		    tried_trial_densities = trial_densities [0:iii-1]
		    tmp = where (tried_trial_densities EQ trial_densities[iii], tried)
		    IF (tried GT 0) THEN BEGIN
		    	step = step*0.75
;;;			print,'density step = ', step
;;;			print, 'trial_densities = ',trial_densities
		    	trial_densities[iii] = trial_densities[iii-1]+step*direction*1.e13
		    ENDIF			
		ENDELSE
					
		; start trials
		print, '  *** Trial '+strtrim(iii+1,2)+ $
			', central density='+strtrim(trial_densities[iii],2)

		dens_mult=(trial_densities[iii]/PrgDat.Profiles.CentralDensity)

		v=run_toray_echres(dens_mult=dens_mult, number_gzones=4, $
			sys_to_run=sys_to_run, /no_plotting)

		if v.success then begin
			for jjj=0, nsys_to_run-1 do begin	; jjj is system
				if sys_to_run[jjj] then begin
					fnam='toray.nc_sys'+strtrim(jjj+1,2)
					if file_test(fnam) then begin
						t=readnc(fnam)
						if t.filename eq '' then print, '  !!! File '+fnam+'not read'
						fabs[jjj,iii]=t.tpowde[t.ledge-2]
					endif else print, '  !!! No file '+fnam
				endif
			endfor

			if not focus then begin	; lowest desity system not determined yet

				; want at least one gyro 0.02 < fabs < 0.98 to pick focus gyro
				ilow=where(fabs[calc,iii] lt 0.02, nilow)
				if nilow eq nsys_to_calc then begin ; reduce density
					if direction lt 0.0 then step=0.5 else direction=-1.0
					nilow=0.
					goto, next_try
				endif
				ihigh=where(fabs[calc,iii] gt 0.98, nihigh)
				if nihigh eq nsys_to_calc then begin ; increase density
					if direction gt 0.0 then step=0.5 else direction=1.0
					nihigh=0.
					goto, next_try
				endif
						
				fabsmin=min(fabs[calc,iii])
				tmpind=where(fabs[calc,iii] eq fabsmin, tmp)	
				If tmp GT 1 THEN BEGIN
					print,'more than one gyrotron have the same aiming, only evaluating one of them'
					sysj=calc[tmpind[0]]
				ENDIF ELSE sysj=calc[tmpind]	; sysj is the lucky gyrotron
				gyro=ECHSys.Tank[sysj].GyroName
				print, '  *** Focusing on gyrotron '+ gyro
				print, ''
				step=step/2.
				focus=1B
				sys_to_run[*]=0
				sys_to_run[sysj]=1
				if fabs[sysj,iii] gt 0.98 then direction=1. else direction=-1.0
				
			endif else begin
				if abs(fabs[sysj,iii]-0.98) lt 0.02 then goto, done_denlim
				if itnum++ lt 4 then begin
					step *= 0.7
					if fabs[sysj,iii] gt 0.98 then direction=1. else direction=-1.0
				endif else goto, done_denlim
				
			endelse
			
		endif
		next_try:
	endfor

	done_denlim:
	
	IF sysj eq !null THEN BEGIN
		print, '  *** Reach maximum # of density trials'	
	ENDIF ELSE BEGIN
		print, '  *** Done with density trials'
		print, '  ***    Elapsed time '+strtrim(systime(1)-t00, 2)
		plot_density_limit, fabs[sysj,*], trial_densities, gyro
	ENDELSE

end

'run_cql': begin

	cd, PrgDat.gdir

	writeg_file, gstruct, file='eqdsk'
	systorun=where(PrgDat.status.plotrays gt 0, nsyson)

	; check that only one frequency is called for, as cql requires
	if nsyson ge 2 then begin
		tankon=where(PrgDat.status.plotrays gt 0)
		for i=1, nsyson-1 do begin
			if ECHSys.Tank[tankon[i]].freq ne $
				ECHSys.Tank[tankon[0]].freq then begin
					res=dialog_message(['All gyrotrons must have the same', $
					'frequency for CQL3D to run. Aborting CQL3D.'])
				return
			endif
		endfor
	endif
	
	fhcqldir=file_search('cqldir*', /test_directory, count=ncqldir)
	if ncqldir eq 0 then cqldir=PrgDat.gdir+'cqldir00' else begin
		cnum=fix(strmid(fhcqldir[n_elements(fhcqldir)-1],6,2))+1
		cqldir=PrgDat.gdir+'cqldir'+string(cnum, format='(i2.2)')
	endelse
	file_mkdir, cqldir
	print, '  *** Starting cql in directory '+cqldir
	run_cql_echres, nsyson, PrgDat.Profiles.BT0, PrgDat.toray_window, $
		PrgDat.use64bits, cqldir, cqltemplate=PrgDat.cqlinput_template

	cd, PrgDat.gdir

end

'run_auto_dens': begin

	widget_control, /hourglass
	
	if PrgDat.gfile eq '' then begin
		res=dialog_message('Pick equilibrium first')
		break
	endif
	if ECHSys.NumTanks eq 0 then begin
		res=dialog_message('Get ECH setup first')
		return
	endif
	if PrgDat.Profiles.enein[0] lt 1. then begin
		res=dialog_message('Pick kinetic profiles first')
		break
	endif

	;close the 'TORAY Plot Window' launched from previous run
	pltorayid=LookupManagedWidget('plot_toray')
	IF pltorayid NE 0L THEN widget_control,pltorayid,/destroy
	
	; kill cql3d window and clear data, if open
	if xregistered('plot_cql_echres') then begin
		widget_control, PrgDat.cql_windowID, /destroy
		PrgDat.cql_pwr[*]=0.
		PrgDat.cql_cur[*]=0.
		PrgDat.cql_rya[*]=0.
		PrgDat.cql_surf=10
		PrgDat.cql_totpwr=0.
		PrgDat.cql_totcur=0.
		PrgDat.cql_mode=0
		PrgDat.cql_window=-1
		PrgDat.cql_b_fudge_factor=1.0
		PrgDat.cql_plotresonance=1
		PrgDat.cql_quadrant=0
		PrgDat.cql_done=0
	endif

	;close the opened 'Unabsorbed EC Power on the Wall' window when'run_Toray' is clicked
	raywallid=LookupManagedWidget('plot_rayonwall')
	IF raywallid NE 0L THEN widget_control,raywallid,/destroy

	;Close auto dens gui if it is already open
	if ISA(auto_stash) eq 1 then begin
		if WIDGET_INFO(auto_stash.AutoBase, /VALID_ID) eq 1 then begin
			WIDGET_CONTROL, auto_stash.AutoBase, /Destroy
		endif
	endif

	; call auto_density_limit
	auto_density_limit
end

'calc_dens_limit': begin

	;Open dens limit calculator GUI
	dens_limit_calculator

	;Autofill shot text box with current EFIT shot
	WIDGET_CONTROL, stash.ShotNum, set_value = LONG(PrgDat.config_shot)

	;Hit "Plot Data" button
	WIDGET_CONTROL, stash.PlotDensity, SEND_EVENT={id:0L, top:0L, handler:0l}
end

else:

endcase
		
return

end ; end echres_event.pro



;*******************************************************************
FUNCTION echres_cancel_event, ev	; Exit the window
  Widget_Control,ev.top,/Destroy
  return,0
END

PRO killed_widget_echres, id	; clean up the heap
  if not Widget_Info(id,/Valid) then Return
  Widget_Control,id,Get_UValue=info,/No_Copy
  Ptr_Free,info.ptr_para
END

;*******************************************************************

FUNCTION echres, adata, gdata, bid=bid, color=color, verbose=verbose, $
	 callback=callback, parameter=parameter,cancel_event=cancel_event,$
	_extra=extra

on_error, 2 ; Return to caller
Common ECHCOM	; defined in reg.pro
Common WIDGETIDS, WidIDs
Common ECHRES_INPUTS, bid_ECHres, color_ECHres, verbose_ECHres, callback_ECHres, parameter_ECHres, cancel_event_ECHres
Common DENS_LIM_AUTO, echres_uval ;used by run_ech_timeslice.pro to access echres widgets for dens limit calculator -PN 04/13/23
Common AUTOMATION, running_auto_mode

running_auto_mode = 0B

if gdata.mh gt 257 or gdata.mw gt 257 then begin
	res=dialog_message('echres cannot run with equilibria larger than 257x257')
	return, bid
endif 

bid_ECHres = bid
color_ECHres = color
verbose_ECHres = verbose
callback_ECHres = callback
parameter_ECHres = parameter
cancel_event_ECHres = cancel_event


if xregistered('echres') then begin

	strgshot=strtrim(gdata.shot,2)
	gshot=long(strgshot.substring(0,5))

	if (gshot ne PrgDat.Shot) or (gdata.time ne PrgDat.time) or (gdata.source ne PrgDat.Source) then begin

		PrgDat.Shot=gshot		
		PrgDat.Time=gdata.time
		PrgDat.Source=gdata.source		
		PrgDat.Status.OutputsValid=0
		PrgDat.Status.ArchivedData=0
		PrgDat.Status.asked_present=0
		PrgDat.new_shot=1
		PrgDat.save_inputs=0
		PrgDat.save_profiles=0
		PrgDat.Profiles.Bt_orig=gdata.bcentr
		PrgDat.Profiles.Bt0=gdata.bcentr
		PrgDat.n_freq=0
		PrgDat.unique_freq[*]=0.
		gstruct=gdata
		widget_control, WidIDs.Eqb, set_value=string(gshot, format='(i6.6)') + $
			'.' + string(gdata.time, format='(i5.5)')
;		widget_control, WidIDs.Eqb, set_value=string(gdata.shot, format='(i6.6)') + $
;			'.' + string(gdata.time, format='(i5.5)')
		;close the opened 'Unabsorbed EC Power on the Wall' window when inputtable is reloaded
		raywallid=LookupManagedWidget('plot_rayonwall')
		IF raywallid NE 0L THEN widget_control,raywallid,/destroy
		;close the opened 'TORAY Plot' window when inputtable is reloaded
		pltorayid=LookupManagedWidget('plot_toray')
		IF pltorayid NE 0L THEN widget_control,pltorayid,/destroy			
	endif else begin
		gdata=gstruct				; update the g struct for other efitiviewer diagnostics
	endelse
	if not adata.error then begin
		adata.d.bcentr=gstruct.bcentr	; update the a display
		PrgDat.Profiles.line_av_density=adata.d.densv2/1.e19
	endif
	echres_plot, color=color
	return, bid
	
endif else begin

	env_user=strlowcase(getenv('USER'))
	if env_user eq '' then begin
		result=dialog_message('Environment variable USER must be set to proceed')
		return, bid
	endif
	if file_test('/fusion/projects/results/echres/chenxi/elog') then $
		spawn, 'echo ' + env_user +  '     ' + string(systime()) + ' >> /fusion/projects/results/echres/chenxi/elog'		
endelse

gstruct=gdata	; for getting g data to echres_event handler
ffstruct=fluxfun_ech(gstruct)
		
; Handle keywords
if not keyword_set(callback) then callback=''
if keyword_set(parameter) then ptr_para=Ptr_New(parameter) $
	else ptr_para=Ptr_New()
if not keyword_set(bid) then bid = 0
if not keyword_set(cancel_event) then cancel_event='echres_cancel_event'

color_setup, /reverse

;cd, current=start_dir
start_dir=getenv('PWD')
setup_dir=start_dir
SrcCommand=''

; toray runs only on 257x257 and smaller equilibria
if gstruct.mw gt 257 or gstruct.mh gt 257 then $
	res=dialog_message('TORAY can not run on equilibria larger than 257x257.')

; get global parameters
z=echcal()
print, ''
print, '      Last calibration and config changes at shot '+strtrim(z.first_shot_newest_config, 2)

; arrays to hold data for plotting
rays=replicate({r:fltarr(z.Max_Number_Raypts), z:fltarr(z.Max_Number_Raypts), $
	q_peak_r:0.0, q_peak_z:0.0, j_peak_r:0.0, j_peak_z:0.0 }, $
	z.MaxNumberOfTanks)

fce0=fltarr(257,257)	; cyclotron frequency, maximum array size 257x257
fuh0=fce0				; upper hybrid
frhco0=fce0				; right hand cutoff
fce=fce0[0:gdata.mw-1, 0:gdata.mh-1]	; array sub-size for this eqdsk
fuh=fce
frhco=fce
rad_res=101	; XC,4/10/2016, set to 101 instead of 201 because inone_linux8

; ************** Structures *******************************************

; ECHSys.Tank is the first (configuration) table, mostly non-editable 
Tank = Replicate({ConfigInputs, $
	gyroname:'', $		; gyrotron text name
	freq:110.0, $ 		; frequency (GHz)
	Antenna:'', $		; antenna type (eg, 'P2001_M1'
	AntPort:'', $		; port name (eg, '271.4 R+1')
	AntNum:0, $ 		; port number 
	TankNo:0, $ 		; tank number+1 (1, 2, ...)
	PowerSupply:'', $	; power supply name
	Power_MW:0.0, $ 	; historical power (MW)
	xfrac:1, $ 			; fraction X desired (0 or 1)
	nharm:2 $
	}, z.MaxNumberOfTanks)

; ECHSys.InputTable is Table 2, with editable input values
InputTable=Replicate({ECHInputs, $
	gyroname:'', $
	xfrac:0.0, $ 		; calculated x-mode fraction
	PolarAng:0.0, $ 	; polar angle of launched ray bundle (deg)
	AziAng:0.0, $		; azimuthal angle of launch (deg)
	PolCts:0L, $		; poloidal counts for antenna encoder
	TorCts:0L, $		; toroidal counts
	pol1:0.0, $			; setting of first polarizer
	pol2:0.0, $			; setting of second polarizer
	f_abs:0.0, $		; fraction absorbed, from toray
	rho_peak:0.0, $ 	; peak rho of power absorption, from toray
	eccd:0.0 $			; total ECCD (kA), from toray
	}, z.MaxNumberOfTanks)

; ECHSys.Antenna contains data regarding the specific antenna
Antenna = Replicate( {AntennaSys, $
	ant_num:0, $		; antenna number 
	pol_ang:0.0, $		; 'poloidal (=scan) angle' of steering mirror normal (deg)
	tor_ang:0.0, $		; 'toroidal (=crank) angle' of steering mirror normal (deg)
	z_launch:0.0, $ 	; launch point above midplane (cm)
	r_launch:0.0, $		; major radius of launch point (cm)
	pol_id:0.0, $		; polar angle of ray incident on steering mirror (deg)
	azi_id:0.0, $		; azimuthal angle of ray incident on steering mirror (deg) 
	offset_angle:0.0, $ ; toroidal angle between mirror and port midplane (deg)
	DwgNo:'', $ 		; eg, 'P2001_M1'
	antenna_inclinationd:0.0, $ ; inclination of entire antenna (deg)
	divergence:1.7, $	; angular divergence (HWHP) of beam
	alpha_a:0.0, $		; applied inclination
	alpha_x:0.0, $		; x-mode inclination
	beta_a:0.0, $		; applied ellipticity
	beta_x:0.0 $		; x-mode ellipticity
	}, z.MaxNumberOfTanks)

; ECHSys.XLine contains data about the transmission line geometry
XLine = Replicate({XLine, $
	gammasd:fltarr(z.MaxNumMiters), $ 	; angle of each bend relative to plane of previous bend
	mir_type:intarr(z.MaxNumMiters), $ 	; 1=pol1, 2=pol2, 4=plane miter bend
	delay:fltarr(z.MaxNumMiters), $
	alpha0:0.0, $		; inclination of plane of E at gyrotron output
	beta0:0.0, $		; ellipticity of E at gyrotron output
	gm1:fltarr(6), $	; polarizer calibration constants for 1st polarizer
	gm2:fltarr(6) $ 	; same for second polarizer
	}, z.MaxNumberofTanks)

ECHSys={NumTanks:z.MaxNumberOfTanks, NumAntennas:z.NumberOfAntennas, $
	Tank:Tank, InputTable:InputTable, Antenna:Antenna, $
	XLine:Xline, rays:rays}	

Status={ $
	ArchivedData:0, $	; 0 if input data are as archived, otherwise 1
	OutputsValid:0, $	; 1 if output table is calculated, otherwise 0
	PlotRays:intarr(ECHSys.NumTanks), $ ; which ray bundles to trace
	recalc_rays:replicate(1,ECHSys.NumTanks), $	; set to 1 to recalculate rays
	RayLength:5.0, $ 	; maximum ray length (m)
	asked_present:0}	; indicator of whether the user has been asked the preference of using present setup or not

Profiles={renein:fltarr(rad_res), $    ;modified by Xi Chen on April 9, 2016 originally all hardcoded to 101
	enein:fltarr(rad_res), $ 
	rtein:fltarr(rad_res), $
	tein:fltarr(rad_res), $
	rtiin:fltarr(rad_res), $
	tiin:fltarr(rad_res), $
	rangrot:fltarr(rad_res), $
	angrotin:fltarr(rad_res), $
	rzeffin:fltarr(rad_res), $
	zeffin:fltarr(rad_res), $
	njene:0, $
	njte:0, $
	njti:0, $
	njzef:0, $
	CentralDensity:0.0, $
	InputDensity:0.0, $
	CentralTe:0.0, $
	InputTe:0.0, $
	Bt0:gdata.bcentr, $
	Bt_orig:gdata.bcentr, $
	ShotCentralDensity:0.0, $	; from zipfits or gaprofiles or namelist
	ShotCentralTe:0.0, $	; from zipfits or gaprofiles or namelist
	line_av_density:0.0, $	; a.d.densv2
	density:0.0, $	; pointname DENSITY
	denv1:0.0, $	; value of denv1 pointname CO2 interferometer
	denv2:0.0, $
	denv3:0.0, $
	denr0:0.0, $
	source:''}

Prefs={ $
	resolution:201L, $	; radial resolution (51, 101, or 201
	gauss_zones:4L, $	; number of radial zones in ray bundle
	ostype:'', $		; operating system type
	user:env_user, $		; user name
	idamp:8L, $ 		; value for idamp in toray
	ds:0.5, $			; value for step size ds and dsmin in toray
	smax:250.0, $		; max ray length (cm)
	dsmin:0.20, $
	pow_avg_half_period:1., $	; averaging time (msec) for ECH power
	nlinputfile:'', $	; namelist input file name
	present_setup_path:'', $	; present gyrotron set
	program_help_path:'', $ 
	first_shot_allowed:123575L, $	; for DIII-D, to avoid pre-P2001 antennas
	tokamak:'DIII-D', $
	scratch_path:'.', $
	use_alphabeta:0, $  ; interpret polar and azi angles in namelist as alpha and beta
	to_timcon:0, $		; flag for sending setup to timcon
	debug:0B}

strgshot=strtrim(gdata.shot,2)
gshot=long(strgshot.substring(0,5))

PrgDat={Status:Status, Profiles:Profiles, Prefs:Prefs, $
	Shot:gshot, $
	Time:gdata.time, $
	Source:gdata.source, $
	shottime:'', $
	toray_window:-1, $
	show_toray_window:0, $
	plot_window:-1, $
	kinetic_plot_window:-1, $
	counts_valid_window:-1, $
	survey_window:-1, $
	density_limit_window:-1, $
	rayonwall_window:-1, $
	gfile:'', $
	new_shot:1, $
	config_shot:gshot, $
	first_shot_newest_config:z.First_Shot_Newest_Config, $
	unique_freq:[110.0,0.0,0.0], $	; maximum of three frequencies allowed (GHz)
	n_freq:1, $ 					; number of unique nonzero frequencies
	start_dir:start_dir, $
	setup_dir:setup_dir, $
	prof_dir:start_dir, $
	nl_dir:start_dir, $
	gdir:'', $
	inone_template:'', $ 			; inone_linux8
	inc_powers:1, $					; 0 for 1 MW nominal powers, 1 for actual powers
	toray_sav_file:'', $			; for plot_toray
	toray_plot_expand:0, $			; =0 for rho=[0,1], =1 for expanded scale
	toray_path:'/fusion/projects/codes/toray/toray/xtoray', $	; path to toray executable
	torayin_template:'', $			; path to template toray.in file
	cql3d_path:'', $				; path to cql3d executable
	cqlinput_template:'', $ 		; path to template cqlinput file
	cql_done:0B, $					; set if cql calcs are valid this shottime
	cql_dir:'', $					; directory containing valid cql results
	cql_pwr:fltarr(32), $			; electron power density from cql (W/cm3)
	cql_cur:fltarr(32), $			; current density from cql (A/cm2)
	cql_rya:fltarr(32), $			; minor radius from cql
	cql_totpwr:0., $				; total absorbed power from cql (W)
	cql_totcur:0., $				; total driven current from cql (A)
	cql_surf:10, $					; surface to plot
	cql_mode:0, $					; plot mode (dist fun, flux, ...)
	cql_window:-1, $				; cql draw window
	cql_windowID:-1, $				; cql window widgetID
	cql_b_fudge_factor:1.0, $		; for overplotting resonance curve
	cql_plotresonance:1, $			; 0, 1, or 2 resonances
	cql_quadrant:0, $				; 0 to plot both quadrants, 1 for lh, 2 for rh
	cql_sys:0, $					; system for plotting resonance
	cql_print:0, $					; 1s to print
	cql_psfiledir:'.', $			; directory for ps files from cql
 	save_profiles:0, $				; save kinetic profiles Te and ne
	save_inputs:0, $				; preserve the input table
	write_global_inone:0, $			; set to write inone_global	
	printer_name:'', $
	print_file:'', $
	survey_print_file:'', $
	use64bits:'y', $
	paths_to_files:'', $			; file containing paths to toray, etc
	ctr:bytarr(256), ctg:bytarr(256), ctb:bytarr(256) }	; color table values

PrgDat.plot_window=!D.WINDOW
tvlct, ctr, ctg, ctb, /get	; to pass to plot_toray window
PrgDat.ctr=ctr & PrgDat.ctg=ctg & PrgDat.ctb=ctb
PrgDat.printer_name=getenv('PRINTER')

if not adata.error then if n_elements(adata.d.densv2) eq 1 then $
	PrgDat.Profiles.line_av_density=adata.d.densv2/1.e19

	
; set some default values for prefs
PrgDat.Prefs.idamp=8
PrgDat.Prefs.ds=0.5
PrgDat.Prefs.dsmin=0.2
PrgDat.Prefs.pow_avg_half_period=0.1
PrgDat.Status.RayLength=5.
PrgDat.Prefs.first_shot_allowed=123575
PrgDat.Prefs.tokamak='DIII-D'
PrgDat.paths_to_files='$ECHRES_PATH/overlay_paths'	; can overwrite in .echres_prefs

;*****************************************************
; fill PrgDat values from file ./.echres_prefs, if it exists
; if so, overwrite the data stored in PrgDat

IF env_user eq 'chenxi' THEN BEGIN
   print, '  *** Checking for my file ./.echres_prefs'
   if file_test('./.echres_prefs') then enter_prefs, './.echres_prefs', PrgDat
ENDIF ELSE BEGIN
   print, '  *** Checking for file ~/.echres_prefs'
   if file_test('~/.echres_prefs') then enter_prefs, '~/.echres_prefs', PrgDat
ENDELSE

; Could overwrite some paths using the paths file
print, '  *** Checking for file overlay_paths'
if PrgDat.paths_to_files ne '' then if file_test(PrgDat.paths_to_files) then $
	enter_prefs, PrgDat.paths_to_files, PrgDat

; set up path for files: either ./ or /cluster-scratch/user/echres/  Add shot later.
sfh=file_test('/cluster-scratch')
if sfh then gdir='/cluster-scratch' else gdir=PrgDat.Prefs.scratch_path

gdir += '/'+PrgDat.Prefs.user
fh=file_test(gdir)
if not fh then file_mkdir, gdir
gdir += '/echres/'
fh=file_test(gdir)
if not fh then file_mkdir, gdir

PrgDat.Prefs.scratch_path=gdir
PrgDat.gdir=gdir

; ****************** Labels for tables ******************
; config table
row_labs=strtrim((indgen(ECHSys.NumTanks)+1),2)

wBase=widget_base(/column, title='ECHRES')

; input widgets
wBaseArch=widget_base(wBase, /row)
wArchiveMDS =Widget_Button(wBaseArch, value='Get MDS ECH Setup', uvalue='readmds')
wArchiveSetupFile=Widget_Button(wBaseArch, value='Read Setup file', uvalue='readfile')
wArchiveSetup=Widget_Button(wBaseArch, value='Use present hardware config', uvalue='use_present')
wArchiveFile=Widget_Button(wBaseArch, value='Namelist Input', uvalue='namelistinput') 
wCentralBt=cw_field(wBaseArch, /floating, /return_events, /row, $
	title='g.bcentr:', value=PrgDat.Profiles.Bt0, $
	uvalue='Bt0', xsize=8)
wEquil=cw_field(wBaseArch, title='Eq: ', xsize=12, $
	value=string(PrgDat.shot, format='(i6.6)') + $
	'.' + string(PrgDat.time, format='(i5.5)'), /noedit)

; config table (upper table)
col_labs_config=['Gyrotron', 'Freq(GHz)', 'Launcher', 'Port', 'Port Num', 'System', $
	'ECHPS', 'Power(MW)', 'Xfrac', 'nharm']
nconfig=n_elements(col_labs_config)
a1=[  1,       2,       1,      1,      1,       1,     1,       2,       1,         1  ]
f1=['(a9)', '(f8.2)', '(a9)', '(a9)', '(i2)', '(i2)', '(a9)', '(f6.3)', '(f3.1)', '(i2)']
align1=intarr(nconfig, z.MaxNumberOfTanks)
formin1=strarr(nconfig, z.MaxNumberOfTanks)
for jj=0, z.MaxNumberOfTanks-1 do begin
	align1[*,jj]=a1
	formin1[*,jj]=f1
endfor
column_width=[10,8,8,8,6,6,8,8,8,8]*!d.x_ch_size
wConfigTable=widget_table(wbase, /editable, /row_major, units=0, $
	column_labels=col_labs_config, row_labels=row_labs, column_width=column_width, $
	value=ECHSys.Tank, uvalue='config_table', format=formin1, alignment=align1)

; widgets for output control
RTrace=indgen(ECHSys.NumTanks)
RTraceLab=strtrim(RTrace+1, 2)
RTraceVal=intarr(ECHSys.NumTanks)
wBaseRays=widget_base(wBase, /row)
wVacPlot = Widget_Button(wBaseRays, value='  Plot rays ', uvalue='plotit')
wRayTrace=cw_bgroup(wBaseRays, RTraceLab, /nonexclusive, /row, $
	label_left='from systems: ', button_uvalue=RTrace, $
	uvalue='RayTrace', set_value=Status.PlotRays)
wRayTraceNone=widget_button(wBaseRays, value='Plot one ', uvalue='PlotOne')
wRayTraceAll=widget_button(wBaseRays,  value='Plot all ', uvalue='PlotAll')
wArchive=cw_bgroup(wBaseRays, /exclusive, ['No','Yes   '], /row, $
	label_left='Inputs as archived: ', button_uvalue=[0,1], /no_release, $
	uvalue='archive', set_value=0)

wBase5=widget_base(wBase, /row)
wUsePresent=cw_bgroup(wBase5, ['Historical', 'Present'], /row, $
	label_left='Hardware config:', button_uvalue=[0,1], $
	uvalue='present_config', set_value=0, /exclusive, /no_release)
wGetPols=Widget_Button(wBase5,      value='   Get polarizers   ', uvalue='getpols')
wArchiveWFile=Widget_Button(wBase5, value='  Save setup file   ', uvalue='writefile')
wSendToTimcon=Widget_Button(wBase5, value='Save setup, send to TIMCON', uvalue='sendtotimcon')

wBase4=widget_base(wBase, /row)
;wOutsValid=cw_bgroup(wBase4, /exclusive, ['No','Yes   '], /row, $
;	label_left='Outputs valid: ', button_uvalue=[0,1], $
;	uvalue='outsvalid', set_value=0)

; Input table (lower table)
col_labs_inputs=['Gyrotron', 'Xfrac(%)', 'Polar', 'Azimuthal', 'Pol Cts', $
	'Tor Cts', 'Pol 1', 'Pol 2', 'F_abs', 'rho_abs', 'ECCD(kA)']
ninputs=n_elements(col_labs_inputs)
column_width2=[10,8,8,7,6,6,6,6,7,7,7]*!d.x_ch_size
a2=[1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]
f2=['(a11)', '(f9.3)', '(f9.3)', '(f9.3)', '(i6)', '(i6)', '(f6.1)', '(f6.1)', $
	'(f9.3)', '(f9.3)', '(f9.3)']
align2=intarr(ninputs, z.MaxNumberOfTanks)
formin2=strarr(ninputs, z.MaxNumberOfTanks)
for jj=0, z.MaxNumberOfTanks-1 do begin
	align2[*,jj]=a2
	formin2[*,jj]=f2
endfor
wInputsTable=widget_table(wbase, /editable, /row_major, units=0, $
	column_labels=col_labs_inputs, row_labels=row_labs, column_width=column_width2, $
	value=ECHSys.InputTable, uvalue='input_table', format=formin2, alignment=align2) 	

; profiles
wBaseProfs=widget_base(wBase, /row)
wProfSource=cw_bgroup(wBaseProfs, /exclusive, $
	['None', 'Zipfits', 'gaprofiles', 'User fits in MDS','namelist'], $
	/row, label_left='Get kinetic profiles:', button_uvalue=[0,1,2,3,4], $
	set_value=0, uvalue='get_profiles')
wPlotProfs=Widget_Button(wBaseProfs,  value='   Plot profiles    ', uvalue='plot_profiles')

wBaseMisc=widget_base(wBase, /row)
wCentralDensity=cw_field(wBaseMisc, /floating, /return_events, /row, $
	title=' n_e(0)(/cm3):', value=PrgDat.Profiles.CentralDensity, $
	uvalue='CentralDensity', xsize=12)
wCentralTe=cw_field(wBaseMisc, /floating, /return_events, /row, $
	title=' T_e(0)(keV):', value=PrgDat.Profiles.CentralTe, $
	uvalue='CentralTe', xsize=12)
wGetDensLim=Widget_Button(wBaseMisc,value=' Density at 98% abs ', uvalue='find_dens_lim')
wRunAutoDens=Widget_Button(wBaseMisc,value=' Auto refl dens ', uvalue='run_auto_dens')
wWriteInone=Widget_Button(wBaseMisc, value='  Write inone file  ', uvalue='WriteInone')

; toray buttons
wBaseToray=Widget_Base(wBase, /Row)
wNumRays=cw_bgroup(wBaseToray, /exclusive, ['1', '18', '30', '100'], /row, $
	label_left=' Number Rays: ', button_uvalue=[0,1,2,3], $
	uvalue='Num_Rays', set_value=2)
wIdamp=cw_bgroup(wBaseToray, /exclusive, ['2', '8'], /row, $
	label_left=' Idamp: ', button_uvalue=[0,1], uvalue='Idamp_val', set_value=1)
wRunToray=Widget_Button(wBaseToray,  value='  Run TORAY   ', uvalue='run_Toray')
wRunCql=Widget_Button(wBaseToray,    value='     CQL3D     ', uvalue='run_cql')
wCalcDensLimit=Widget_Button(wBaseToray,value=' ECH Dens Limit Calculator ', uvalue='calc_dens_limit')

; bottom button widgets
wBaseEnd = Widget_Base(wBase, /Row)	
wHelp=Widget_Button(wBaseEnd,        value='    Program Help    ', uvalue='help')
wAutoAim=Widget_Button(wBaseEnd,     value='     Auto_Aimer     ', uvalue='auto_aim')
wCancel = Widget_Button(wBaseEnd,    value='     EXIT     ', uvalue='closewindow')

if env_user eq 'prater' or env_user eq 'chenxi' then $
	wStop = Widget_Button(wBaseEnd,  value='        STOP        ', uvalue='wstop')
if env_user eq 'prater' or env_user eq 'cengher' or env_user eq 'lohr' or env_user eq 'chenxi' $
	then wDump = widget_button(wBaseEnd,      value='     DUMP      ', uvalue='dump_echsys')

; IDs of widgets which get data back from echres_plot or other prgs
WidIDs={ConfigTable:wConfigTable, InputTable:wInputsTable, $
	Archive:wArchive, RayTrace:wRayTrace,  $
	Eqb:wEquil, ProfSource:wProfSource, GaussZones:wNumRays, $
	CentralDensity:wCentralDensity, CentralTe:wCentralTe, $
	Bt0:wCentralBt, Inputs_Archived:wArchive, UsePresent:wUsePresent, Base:wBase, $
	ArchiveMDS:wArchiveMDS, ArchiveSetupFile:wArchiveSetupFile}

uvalue={color:color, routine:callback, ptr_para:ptr_para, WidIDs:WidIDs}

;ASSIGN GLOBAL VARIABLE 'echres_uvalue' TO UVALUE SO THAT DENS LIMIT CALCULATOR CAN CONTROL ECHRES WIDGETS -PN 04/13/23
echres_uval = uvalue

widget_control, wBase, /realize, set_uvalue=uvalue
echres_plot, color=color

xmanager, 'echres', wBase

end

