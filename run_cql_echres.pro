pro run_cql_echres, nsys, bt0, toray_window, use64bits, cqldir, $
	cqltemplate=cqltemplate
common ECHCOM

print, 'Starting run_cql_echres...'
PrgDat.cql_done=0B

;if PrgDat.Profiles.njti eq 0 then begin
;	message, 'Get inone global file for Ti data before running CQL3D',/info
;endif

fh=file_search('toray.nc_sys*', /fully_qualify_path, count=count)
if count ne nsys then begin
	message, 'Wrong number of toray.nc_sys* files:' + $
		'Found '+strtrim(count,2)+'; expecting '+strtrim(nsys,2), /info
	return
endif
file_copy, fh, cqldir, /overwrite

fe=file_search('eqdsk', /fully_qualify_path, count=count)
if count ne 1 then message, 'No eqdsk file; aborting.'
file_copy, 'eqdsk', cqldir, /overwrite

cd, cqldir

t=combine_toraync(fh, outfile='combined_toraync.nc')
;if t.nray gt 150 then begin
;	res=dialog_message(['Too many rays: '+strtrim(t.nray, 2)+';', 'maximum is 150'])
;	return
;endif
if not keyword_set(cqltemplate) then cqltemplate='$ECHRES_PATH/cqlinput_template' 
fc=file_search(cqltemplate, count=count)
if count ne 1 then begin
	res=dialog_message('Could not find cqlinput template file '+cqltemplate)
	return
endif

; get the rya values for cqlinput
rya=get_rya_toraync('combined_toraync.nc', 20, toray_window)

; get administrative data
mnemonic=strtrim(string(PrgDat.shot),2)
ech_on='.true.'
efield_on='.false.'
nc_rf_flux='.true.'	; default is with flux nc files
nc_co_flux='.true.'
nc_tot_flux='.true.'

; get sign of Bt right for rf module
if bt0 lt 0 then bt_sign=-1.0 else bt_sign=1.0

nknots=11
rdata=get_range(0., 1., nknots)
enein=spline(PrgDat.Profiles.renein[0:PrgDat.Profiles.njene-1], $
	PrgDat.Profiles.enein[0:PrgDat.Profiles.njene-1], rdata)
renein=rdata
tein=spline(PrgDat.Profiles.rtein[0:PrgDat.Profiles.njte-1], $
	PrgDat.Profiles.tein[0:PrgDat.Profiles.njte-1], rdata)
rtein=rdata
ip=where(PrgDat.Profiles.tiin gt 0., nip)
if nip le 1 then begin
	print, '  !!! Using Ti=Te since no Ti data available; get inone file if Ti wanted'
	tiin=tein
	rtiin=rdata
endif else tiin=spline(PrgDat.Profiles.rtiin[0:PrgDat.Profiles.njti-1], $
	PrgDat.Profiles.tiin[0:PrgDat.Profiles.njti-1], rdata)
rtiin=rdata
zeffin=spline(PrgDat.Profiles.rzeffin[0:PrgDat.Profiles.njzef-1], $
	PrgDat.Profiles.zeffin[0:PrgDat.Profiles.njzef-1], rdata)
rzeffin=rdata

rfread='netcdf'
rffile='combined_toraync.nc'

; write cqlinput file
success=write_cqlinput( $
	mnemonic=mnemonic, $
	in_file=PrgDat.cqlinput_template, $
	rya=rya, $
	enein=enein, $
	tein=tein, $
	tiin=tiin, $
	zeffin=zeffin, $
	rffile=rffile, $
	rfread=rfread, $
	elec_field=e_field, $
	bt_sign=bt_sign, $
	ech_on=ech_on, $
	efield_on=efield_on, $
	nc_rf_flux=nc_rf_flux, $
	nc_co_flux=nc_co_flux, $
	nc_tot_flux=nc_tot_flux )
if success then print, '  *** Wrote cqlinput file ' + cqldir + $
	'/cqlinput *****' else $
	message, '  !!!  Writing cqlinput file failed; aborting.'
print, '  *** cql3d command is: '+PrgDat.cql3d_path+' > cqlog'

cqlinputfile=cqldir+'/cqlinput'

msg=[ $
	'If the TORAY Plot Window is open, the 24 values of rho', $
	'where the Fokker-Planck equation will be solved',$
	'appear as symbols in the q_e plot.', $
	'To change these locations, edit the rya values in', $
	cqlinputfile, $
	'', $
	'         Continue now?']
for ji=0, n_elements(msg)-2 do print, msg[ji]
res=dialog_message(msg, /question)
if res ne 'Yes' then return

; spawn or stay connected?
res=dialog_message(['Do you want to run CQL3D now as a connected process', $
	'(ties up echres for ~10 minutes)?,', $
	'or click NO to spawn CQL3D as a disconnected process'], /question)
	
pos1=strpos(strupcase(getenv('HOST')), 'SATURN')
pos2=strpos(strupcase(getenv('HOST')), 'IRIS')
if (pos1 LT 0) and (pos2 LT 0) then begin
	print, '  *** this version only works on Saturn or Iris clusters'
	return
endif

print, '  *** running cql3d in directory '+cqldir

if res ne 'Yes' then begin
	spawn, PrgDat.cql3d_path + ' > cqlog &' 
	return
endif else begin
	widget_control, /hourglass
	oldtime=systime(1)
	spawn, PrgDat.cql3d_path + ' > cqlog'
	print, '  *** CQL3D completed in '+strtrim((systime(1)-oldtime)/60.)+ ' min'

	PrgDat.cql_done=1B
	PrgDat.cql_dir=cqldir
	PrgDat.cql_pwr[*]=0.
	PrgDat.cql_cur[*]=0.
	PrgDat.cql_rya[*]=0.
	c=readnc(cqldir+'/'+strtrim(PrgDat.shot,2)+'.nc')
	if c.filename eq '' then begin
		print, ''
		print, '  !!! Restore of CQL main nc file failed'
		print, ''
		return
	endif
	
	ncalc=n_elements(c.time)
	PrgDat.cql_rya[0:c.lrz-1]=c.rya
	Prgdat.cql_pwr[0:c.lrz-1]=c.rfpwr[*,0,ncalc-1]
	gf=readg(PrgDat.gfile, mode='file')
	f=fluxfun_ech(gf)

	fcql=interpol(f.fcql, f.rho/f.rho[100], c.rya) 
	cql_cur=fcql*c.curtor[*,ncalc-1]/(c.btor0/10000.)

	PrgDat.cql_cur[0:c.lrz-1]=cql_cur
	PrgDat.cql_totcur=c.ccurtor(c.lrz-1, ncalc-1)
	
	d=readnc(cqldir+'/'+strtrim(PrgDat.shot,2)+'_rf.nc')
	rmpwr=0.
	for i=0, d.nray-1 do rmpwr += d.delpwr[d.nrayelt[i]-1,i]
	PrgDat.cql_totpwr=(total(d.delpwr[0,*])-rmpwr)/1.e7
	
	plot_toray
	
	plot_cql_echres, cqldir, mnemonic

endelse

return

end

