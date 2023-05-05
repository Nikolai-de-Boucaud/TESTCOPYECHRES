function survey_steering_angles, obj, rho_target, $
	Bt0, Bt0max, Bt0min, eccdsign, $
	gauszones, polmin, polmax, npol, azimin, azimax, nazi, nbt
	
; sys0 is the system number, starting at zero
; obj=0,1,2 for no ECCD, max ECCD, or max j_ECCD
; rho_target is target value of rho for ECCD or ECH in case of
;	no ECCD
; eccdsign is +1. or -1.
; pol and azi are the polar and azimuthal angles, in degrees

; Should already be in /cluster-scratch/user/echres/shotnumber/
;	and toray should have been run

common ECHCOM
on_error, 2

; Get initial values
cd, PrgDat.gdir
fh=file_search('echin_sys*', count=count)
if count lt 1 then begin
	res=dialog_message('No '+echin_file+' file; run toray first.')
	return, {success:0}
endif else ein=read_echin(fh[0])

ft=file_search('toray.in', count=count)
if count eq 0 then begin
	res=dialog_message('No toray.in file; run toray first.')
	return, {success:0}
endif

; already ran gafit
; Note: toray has segmentation fault on venus if igatfit ne 1
write_torayin, infile='toray.in', igafit=1, gauszone=gauszones

g=gstruct
if g.bcentr lt 0. then bsign=-1. else bsign=1.
nbt=1
Bt=g.bcentr

; set range of parameters
pol=get_range(polmin, polmax, npol)
azi=get_range(azimin, azimax, nazi)
rho=get_range(0.,1.,201)

print, '  *** Starting auto_aimer'
print, '  *** polar_min='+strtrim(polmin,2)+'; polar_max='+strtrim(polmax,2)
print, '  *** azimuthal_min='+strtrim(azimin,2)+'; azimuthal_max='+strtrim(azimax,2)

fht=file_search('toray.nc_sys*', count=count)
if count lt 1 then begin
	res=dialog_message('No toray.nc_sys* file; run toray first.')
	return, {success:0}
endif
t=readnc(fht[0])
if t.filename eq '' then begin
	message, 'Toray failed initial run. Aborting.'
	return, {success:0}
endif
freq=t.freqcy/1.e9
p=get_peak_abs(t,g)
if not p.success then begin
	message, 'Get_peak_abs failed initial run.', /continue
	return, {success:0}
endif
clear_struct, p
pp=create_struct('pol', 0., 'azi', 0., 'Bt', 0., p) 
echdat=replicate(pp, nazi, npol)
power=1.e6
sufx=string(PrgDat.shot, format='(i6.6)')+'.'+string(PrgDat.time, format='(i5.5)')

e={success:0, echdat:echdat, pol:pol, npol:npol, azi:azi, nazi:nazi, $
			bt:bt, nbt:nbt, g:g, rho_target:rho_target, obj:obj, freq:freq, $
			file:'survey_ech_'+sufx+'.sav'}

print, '  *** Case Bt='+strtrim(bt,2)

; now run through the angles, running toray
for iazi=0, nazi-1 do begin	; poloidal angle scan
	for ipol=0, npol-1 do begin	; toroidal angle scan
		print, '    ipol='+strtrim(ipol,2)+'; iazi='+strtrim(iazi,2)+';  pol, azi = ' $
			+ strtrim(pol[ipol],2) + ', ' + strtrim(azi[iazi],2)
			
		; run toray
		success=write_echin(ein, 'echin', phai=azi[iazi], thet=pol[ipol])
		spawn, PrgDat.toray_path + ' > logf'
		file_copy, 'echin', 'inech_'+strtrim(iazi,2) + '_' + strtrim(ipol,2),/overwrite

		; first check that rays actually intersect the plasma
		b=''
		openr, lun, 'logf', /get_lun
		for i=0, 20 do begin
			if not EOF(lun) then readf, lun, b
			ip=strpos(b, 'ISTOP')
			if ip ne -1 then break
		endfor
		close, lun & free_lun, lun
		istop=strmid(b,18,1)
		iflag=strmid(b,29,1)
		;if (((istop ne '7') or (istop ne '2')) and (iflag ne '2')) then begin
		;	print, '        Ray 0 has ISTOP='+strtrim(istop,2) + ' and IFLAG=' $
		;		+ strtrim(iflag,2) + '; angle skipped.'
		;	goto, skip_angle
		;endif
			
		; read and store the data from toray
		t=readnc('toray.nc')
		if t.filename eq '' then begin
			print, '        toray.nc file not written; angle skipped.'
			goto, skip_angle
		endif
		p=get_peak_abs(t, g)
		if not p.success then begin
			print, '        less than 2% absorption; angle skipped.'
			goto, skip_angle
		endif
		p.qe0 *= power
		p.j0 *= power
		p.ECCD *= power
		pp=create_struct('pol', pol[ipol], 'azi', azi[iazi], 'Bt', PrgDat.Profiles.Bt0, p) 
		e.echdat[iazi,ipol]=pp
		skip_angle:
	endfor
	save, e, filename='survey_ech_'+sufx+'.sav' ; save results along the way
endfor	

print, '******************************'
print, ''
e.success=1
return, e
end
