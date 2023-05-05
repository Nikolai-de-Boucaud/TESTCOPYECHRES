function run_toray_echres, save_data=save_data, _EXTRA=extr, $
	dens_mult=dens_mult, $  	; factor multiplying the density profile
	no_plotting=no_plotting, $	; avoid the plotting and stdout data
	number_gzones=number_gzones, $ ; number of rays to use
	sys_to_run=sys_to_run		; array of which systems to run toray for
	
; all input data come from the common block ECHCOM
; if save_data is set, toray save file is written

common ECHCOM	; contains the input data

ledge=PrgDat.Prefs.resolution-1	; radial resolution (toray is at bin midpoints)

if n_elements(sys_to_run) eq 0 then sys_to_run=PrgDat.Status.Plotrays

igyros=where((sys_to_run and $
	(ECHSys.InputTable.PolarAng gt 10.)), ngyros)
if ngyros lt 1 then begin
	print, '  !!! No systems turned on!'
	return, {success:0, error_msg:'No systems on'}
endif

if not keyword_set(no_plotting) then begin

	; clear the stale rays
	ECHSys.Rays[*].r[2:*]=0.
	ECHSys.Rays[*].z[2:*]=0.
	ECHSys.Rays[*].q_peak_r=0.
	ECHSys.Rays[*].q_peak_z=0.
	ECHSys.Rays[*].j_peak_r=0.
	ECHSys.Rays[*].j_peak_z=0.

	toray_data=replicate({ $
		system_number:0, $		; echres system number
		gyroname:'', $
		TankNo:0, $ 			; ECH system (tank) number
		power:0.0, $
		rho:fltarr(ledge), $	; toray (200 pt) rho grid (bin centers)
		q_e:fltarr(ledge), $	; power density profile (W/cm2)
		integrated_power:fltarr(ledge), $	; integrated power profile (W)
		j_eccd:fltarr(ledge), $ ; current density profile (A/cm2)
		eccd:0.0, $ 			; total ECCD (A)
		nharm:0, $				; for calc of ECCD with toray
		f_abs:0.0, $			; total fractional absorption
		rho_peak:0.0, $			; rho of peak absorption of central ray
		r_peak:0.0, $			; R of peak abs central ray (m)
		z_peak:0.0, $			; (m)
		phi_peak:0.0, $ 		; (deg)
		npar_peak:0.0, $
		eps_peak:0.0, $
		theta_p_peak:0.0, $
		yy_peak:0.0, $
		ne_peak:0.0, $
		Te_peak:0.0, $
		beta_e:0.0, $
		zeff_peak:0.0, $
		cd_eff:0.0 $
		}, ngyros)

endif
	
pushd, PrgDat.gdir	; cd to /cluster-scratch/user/echres/shotnumber/

; clear the stale files
patt=['ecrh*', 'psiin', 'log*', 'gafit.in', 'mhddat', 'toray*', $
	'echin*', 'inone*', 'echout', 'g0.0*', 'toray.in*', $
	'profstat.dat', 'rjpdh.out']
fh=file_search(patt, count=count)
if count gt 0 then file_delete, fh, /allow_nonexistent

; write the g file, for posterity
if not file_test('g'+PrgDat.shottime+'*') then file_copy, PrgDat.gfile, '.'

; write psiin
write_psiin_echres, status=status
if not status then begin
	print, '  !!! write_psiin_echres failed.'
	return, {success:0, error_msg:'psiin failed'}
endif
pin=read_psiin()

; cycle through the systems
for i=0, ngyros-1 do begin

	j=igyros[i]
	
	;if i eq 0 then igaf=1 else igaf=0		; write mhddat only once
	; some segmentation problem running toray on an old mhddat file
	igaf=1
	
	power=ECHSys.Tank[j].power_MW*1.e6		; power (W)
	if power eq 0. then power=1.e6 			; nominal 1 MW

	; write toray.in
	if number_gzones eq !null then gszones=PrgDat.Prefs.gauss_zones else $
		gszones=number_gzones
		
	write_torayin, infile=PrgDat.torayin_template, data=torayindata, $
		header='written by echres', ecpower=power, $
		gauszone=gszones, igafit=igaf, status=status, $
		nharm=ECHSys.Tank[i].nharm, smax=PrgDat.Prefs.smax
	; if status then file_copy, 'toray.in', 'toray.in_sys'+strtrim(j+1,2) $
	if not status then begin
		print, '  !!! write_torayin failed'
		return, {success:0, error_msg:'write_torayin failed'}
	endif

	; write echin and echin_sys#
	write_echin_echres, pin, j, _EXTRA=extra, dens_mult=dens_mult

	tm=systime(1)
	print, '  *** Starting toray for system '+strtrim(j+1,2)
	spawn, PrgDat.toray_path + ' > logfile_sys'+strtrim(j+1,2)
	print, systime(1)-tm, format='(''  ***   Toray done; time='',f5.1,'' sec'')'

	; first, restore nc file and check it
	if not file_test('toray.nc') then begin
		msgtorf=['run_toray_echres: toray failed.', 'Check logfile: ' + $
			PrgDat.gdir+'/logfile_sys'+strtrim(j+1,2)]
		result=dialog_message(msgtorf)
		return, {success:0, error_msg:'toray.nc file not found'}
	endif

	file_move, 'toray.nc', 'toray.nc_sys'+strtrim(j+1,2)

endfor
file_delete, ['profstat.dat', 'echin', 'rjpdh.out'], /allow_nonexistent 
print, '  *** Done with toray'
print, ''

if keyword_set(no_plotting) then return, {success:1}

; clear the rays structure
notplot=where(PrgDat.Status.PlotRays eq 0, nop)
if nop gt 0 then begin
	ECHSys.Rays[notplot].r[2:*]=0.
	ECHSys.Rays[notplot].z[2:*]=0.
endif
ECHSys.Rays[igyros].r[2:*]=0.	; keep 1st two points (antenna and edge)
ECHSys.Rays[igyros].z[2:*]=0.
ECHSys.Rays[*].q_peak_r=0.
ECHSys.Rays[*].q_peak_z=0.
ECHSys.Rays[*].j_peak_r=0.
ECHSys.Rays[*].j_peak_z=0.
ECHSys.InputTable[*].f_abs=0.
ECHSys.InputTable[*].rho_peak=0.
ECHSys.InputTable[*].eccd=0.

titags=tag_names(torayindata)
n=where(titags eq 'GAUSZONE')
if n[0] ge 0 then gauszone=torayindata.gauszone else gauszone=4
n=where(titags eq 'DS')
if n[0] ge 0 then ds=torayindata.ds else ds=0.5
n=where(titags eq 'DSMIN')
if n[0] ge 0 then dsmin=torayindata.dsmin else dsmin=0.5
n=where(titags eq 'MODELC')
if n[0] ge 0 then modelc=torayindata.modelc else modelc=5


; get the q=2 surface
rq2=rq(gstruct, 2.0, f=ffstruct)
if rq2.rho_q[0] gt 0.01 then begin	; there's a q=2 surface
	psiq2=interpp(ffstruct.rhonorm, ffstruct.psi, rq2.rho_q[0])
	psiq2norm=(psiq2-ffstruct.psi[0])/(ffstruct.psi[100]-ffstruct.psi[0])
	contour_psi, gstruct, 720, psiq2norm, psiq2norm, 2*gstruct.mw, 2*gstruct.mh, $
		psi, thetag, rg, /do_one
	if n_elements(rg) gt 1 then begin
		Rmajq2=gstruct.rmaxis+rg*cos(thetag)
		Zq2=gstruct.zmaxis+rg*sin(thetag)
		iq2=where(Zq2 gt 0.)
		if iq2[0] ge 0 then begin
			Zq2=Zq2[iq2]
			Rmajq2=Rmajq2[iq2]
		endif
	endif
endif

rq32=rq(gstruct, 1.5, f=ffstruct)
if rq32.rho_q[0] gt 0.01 then begin	; there's a q=3/2 surface
	psiq32=interpp(ffstruct.rhonorm, ffstruct.psi, rq32.rho_q[0])
	psiq32norm=(psiq32-ffstruct.psi[0])/(ffstruct.psi[100]-ffstruct.psi[0])
	contour_psi, gstruct, 720, psiq32norm, psiq32norm, 2*gstruct.mw, 2*gstruct.mh, $
		psi, thetag, rg, /do_one
	if n_elements(rg) gt 1 then begin
		Rmajq32=gstruct.rmaxis+rg*cos(thetag)
		Zq32=gstruct.zmaxis+rg*sin(thetag)
		iq32=where(Zq32 gt 0.)
		if iq32[0] ge 0 then begin
			Zq32=Zq32[iq32]
			Rmajq32=Rmajq32[iq32]
		endif
	endif
endif

; now read and return the toray data
;print, 'Sys Gyrotron  Antenna     Pol_Cts   Tor_Cts   rho_j0   R_j0     Z_j0     Z_q2     Z_q32'	
print, 'Sys Gyrotron      rho_j0    R_j0      Z_j0      Z_q2      Z_q32  Azi_launch Azi_abs'	

for i=0, ngyros-1 do begin

	kk=igyros[i]	; system number
	sufx='_sys'+strtrim(kk+1,2)

	; first, restore toray.nc file and check it
	if not file_test('toray.nc'+sufx) then begin
		result=dialog_message('run_toray_echres: no toray.nc'+sufx)
		return, {success:0, error_msg:'No toray.nc file'}
	endif
	;tnc=readnc('toray.nc'+sufx)
	tnc=add_gyroname('toray.nc'+sufx, ECHSys.Tank[kk].gyroname)	; add gyroname to toray.nc
	if (tnc.filename eq '') then begin
		result=dialog_message('run_toray_echres: invalid toray.nc'+sufx)
		return, {success:0, error_msg:'invalid toray.nc'+sufx}
	endif
	nrpts_rays=n_elements(ECHSys.rays[0].r)-2	; maximum number of ray points allowed
	nrpts_toray=max(tnc.nrayelt)	 ; nrayelt is the number of ray elements for each ray
	nel=min([nrpts_rays, nrpts_toray])	; maximum number of points to keep along rays

	; enter data from nc file into data structures	
	; first, the ray data	
	ECHSys.Rays[kk].r[2:nel+1]=float(tnc.wr[0:nel-1,0])/100.	; return m not cm
	ECHSys.Rays[kk].z[2:nel+1]=float(tnc.wz[0:nel-1,0])/100.

	; enter the toray_data data (everything per W)
	toray_data[i].system_number=kk+1
	toray_data[i].TankNo=ECHSys.Tank[kk].TankNo
	toray_data[i].gyroname=ECHSys.Tank[kk].gyroname
	toray_data[i].power=ECHSys.Tank[kk].power_MW*1.e6
	toray_data[i].rho=tnc.xmrho
	toray_data[i].q_e=tnc.weecrh	;  power density per incident watt, W/cm^3
	toray_data[i].j_eccd=tnc.currf
	toray_data[i].integrated_power=tnc.tpowde
	toray_data[i].f_abs=max(tnc.tpowde)
	toray_data[i].eccd=tnc.tidept[tnc.ledge-2]	;integrated current per incident watt (Amp)
	ECHSys.InputTable[kk].eccd=tnc.tidept[tnc.ledge-2]*ECHSys.Tank[kk].power_MW*1.e6
	ECHSys.InputTable[kk].f_abs=max(tnc.tpowde)*100.

	; get data corresponding to peak of absorption
	pk=get_peak_abs(tnc, gstruct)
	if not pk.success then begin
		print, '  !!! run_toray_echres: get_peak_abs failed for '+sufx
	endif else begin
		toray_data[i].rho_peak=pk.rho
		ECHSys.rays[kk].q_peak_r=pk.r
		ECHSys.rays[kk].q_peak_z=pk.z
		ECHSys.InputTable[kk].rho_peak=pk.rho
		toray_data[i].r_peak=pk.r
		toray_data[i].z_peak=pk.z
		toray_data[i].phi_peak=pk.phi/!dtor
		toray_data[i].npar_peak=pk.n_par
		toray_data[i].eps_peak=pk.eps
		toray_data[i].theta_p_peak=pk.theta_pol
		toray_data[i].yy_peak=pk.yy
		toray_data[i].ne_peak=pk.n_e
		toray_data[i].Te_peak=pk.T_e
		toray_data[i].beta_e=pk.beta_e
		toray_data[i].Zeff_peak=pk.Zeff
		if pk.T_e gt 0. and power gt 0. then toray_data[i].cd_eff = $
			33.*(pk.n_e/1.e14)*toray_data[i].eccd*gstruct.rmaxis/pk.T_e
		
		;print, '  *** System ',strtrim(kk+1,2)
		;print, '  ***    Peak (R,Z), rho of q_e: ', pk.r, pk.z, $
		;	rho_rz(gstruct, pk.r, pk.z, /norm)

		;;;dimensional CD efficiency often used in the literature, -XC 05172017
		;print, 'cd_eff=',toray_data[i].cd_eff
		;print, 'pk.T_e (keV)=',pk.T_e
		;print, 'cd_eff (w/o 33/Te)=', toray_data[i].cd_eff*pk.T_e/33.

		; New 20140617, for Francesca
		for kin=0, tnc.nray-1 do begin
			in=where(abs(tnc.curds[*,kin]) gt 1.0e-7, count)
			if count gt 0 then begin
				frac_pow_j=tnc.delpwr[in[0]-4,kin]/tnc.delpwr[0,kin]
				if frac_pow_j lt 0.98 then $
					print, '    *** Frac power for Sys ' + strtrim(i+1,2) + $
					' where ECCD starts: ' + string(frac_pow_j, format='(f6.3)')
				goto, done_w_fracpowj
			endif
		endfor
		done_w_fracpowj:

		; get peak of j for central ray
		curds=abs(tnc.curds[*,0])
		itnc=where(curds eq max(curds))
		if itnc[0] ge 0 then begin
			jpeakr=tnc.wr[itnc[0],0]/100.
			jpeakz=tnc.wz[itnc[0],0]/100.
			jpeakrho=rho_rz(gstruct, jpeakr, jpeakz, /norm)
		endif else begin
			jpeakr=0.
			jpeakz=0.
			jpeakrho=0.
		endelse
		
		; get z of rational q=2, 3/2 at major radius jpeakr
		if n_elements(Rmajq2) gt 0 then begin
			iq2=where(Rmajq2 le jpeakr)
			if iq2[0] ge 0 then Zq2_0=Zq2[iq2[0]] else Zq2_0=0.
 		endif else Zq2_0=0.

		if n_elements(Rmajq32) gt 0 then begin
			iq32=where(Rmajq32 le jpeakr)
			if iq32[0] ge 0 then Zq32_0=Zq32[iq32[0]] else Zq32_0=0.
 		endif else Zq32_0=0.
		
		;print, kk+1, ECHSys.Tank[kk].gyroname, ECHSys.Tank[kk].Antenna, $
		;	ECHSys.InputTable[kk].PolCts, ECHSys.InputTable[kk].TorCts, $
		;	jpeakrho, jpeakr, jpeakz, zq2_0, zq32_0, format='(i2, 2a10, 2I10, 5f9.3)'
		
		launch_azi=float(strmid(echsys.tank[kk].antport,0,5))
		peak_azi=launch_azi- (pk.phi + tnc.wphi[0,0])/!dtor
		if peak_azi gt 360. then peak_azi -= 360. 
		if peak_azi lt 0. then peak_azi += 360.
		print, kk+1, ECHSys.Tank[kk].gyroname, jpeakrho, jpeakr, jpeakz, zq2_0, zq32_0, $
			launch_azi, peak_azi, format='(i3, a10, 7f10.3)'
		
		;if PrgDat.Prefs.user eq 'prater' then begin
		;	jmodel=snell(ECHSys.Antenna[i].r_launch, ECHSys.Antenna[i].z_launch, $
		;		ECHSys.InputTable[i].polarang, ECHSys.InputTable[i].aziang, $
		;		PrgDat.Profiles.line_av_density, 0.3*PrgDat.Profiles.tein[0], $
		;		ECHSys.Tank[i].freq, ECHSys.Tank[i].nharm, gstruct)
		;
		;	if jmodel.status then begin
		;		ECHSys.Rays[kk].j_peak_r=jmodel.r
		;		ECHSys.Rays[kk].j_peak_z=jmodel.z
		;		jmodelrho=rho_rz(gstruct, jmodel.r, jmodel.z, /norm)
		;		print, 'Diff from Snell:', jpeakrho-jmodelrho, $
		;			jpeakr-jmodel.r, jpeakz-jmodel.z, format='(a42, 3f9.3)'
		;	endif	
		;endif
		
	endelse

endfor  ;;;loop of gyro systems (i loop)
print, ''

; check if the total power for any gyrotron is less than 98% absorbed
irf=where(sys_to_run AND (NOT STRCMP(ECHSys.InputTable.GYRONAME,'')) $
		 AND (ECHSys.InputTable.f_abs LT 98.), nrf)
if nrf GT 0 then begin
	print, '  *** calculating the EC power on the wall for system(s) with F_abs < 98% ! '
	run_rayonwall,irf
endif

if keyword_set(save_data) then begin	; save file with toray data
	toray_version=''
	if file_test('logfile'+sufx) then begin
		openr, lun, 'logfile'+sufx, /get_lun
		for n=0, 10 do begin
			readf, toray_version
			if strpos(toray_version, 'toray') ne -1 then break
		endfor
		close, lun
		free_lun, lun
	endif else toray_version='unknown'

	toray_run_data={toray_version:toray_version, toray_path:PrgDat.toray_path, $
		freq:tnc.freqcy, nharm:tnc.nharm, idamp:tnc.idamp, $
		nray:tnc.nray, gauszone:gauszone, $
		ds:ds, dsmin:dsmin, modelc:modelc}

	save_file_name=PrgDat.gdir+'toray'+PrgDat.shottime+'.sav'
	ishot=PrgDat.shot
	itime=PrgDat.time
	save, ishot, itime, toray_data, $
		toray_run_data, filename=save_file_name
	
	PrgDat.toray_sav_file=save_file_name
	
endif

return, {success:1, toray_data:toray_data}

end
