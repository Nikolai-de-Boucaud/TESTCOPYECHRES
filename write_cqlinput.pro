; write_cqlinput
; inputs cqlinput master file, modifies common quantities, and
; writes a file cqlinput
; returns 1 for success or 0 for failure

function write_cqlinput, $
	eqdskin=eqdskin, $
	mnemonic=mnemonic, $
	in_file=in_file, $
	rya=rya, $
	enein=enein, $
	renein=renein, $	; use only if enein is not specified on 11 equal spaced rho values
	tein=tein, $
	rtein=rtein, $		; use only if tein is not specified on 11 equal spaced rho values
	tiin=tiin, $
	rtiin=rtiin, $		; use only if tiin is not specified on 11 equal spaced rho values
	zeffin=zeffin, $
	rzeffin=rzeffin, $	; use only if zeffin is not specified on 11 equal spaced rho values
	elec_field=elec_field, $
	bt_sign=bt_sign, $
	enorm=enorm, $
	nharm1=nharm1, $
	nharms=nharms, $
	outfile=outfile, $
	ech_on=ech_on, $
	efield_on=efield_on, $
	nc_rf_flux=nc_rf_flux, $
	nc_co_flux=nc_co_flux, $
	nc_tot_flux=nc_tot_flux, $
	rffile=rffile, $
	rfread=rfread

ap='''

if keyword_set(in_file) then cqlinput_template=in_file $
	else cqlinput_template='$ECHRES_PATH/cqlinput_template'
	
fh=findfile(cqlinput_template, count=count)
if count gt 0 then c=read_nl2(cqlinput_template) else begin
	print, '  !!! write_cqlinput: aborting: cannot find cqlinput_template file '+cqlinput_template
	return, 0
endelse

if keyword_set(mnemonic) then c.setup.mnemonic=ap+mnemonic+ap

if keyword_set(eqdskin) then c.eqsetup.eqdskin=ap+eqdskin+ap

if keyword_set(enorm) then c.setup$.enorm=enorm

if keyword_set(nharm1) then c.rfsetup.nharm1=nharm1

if keyword_set(nharms) then c.rfsetup.nharms=nharms

if keyword_set(rffile) then c.rfsetup.rffile=ap+rffile+ap $
	else c.rfsetup.rffile=ap+'genray.nc'+ap

if keyword_set(rfread) then c.rfsetup.rfread=ap+rfread+ap $
	else c.rfsetup.rfread=ap+'netcdf'+ap

if keyword_set(rya) then begin
	if n_elements(rya) gt c.setup.lrz then begin
		print, '  !!! write_cqlinput: too many rya; trimming array size.'
		rya=rya[0:c.setup.lrz-1]
	endif
	c.setup.lrz=n_elements(rya)
	c.setup$.rya$1[*]=0.
	c.setup$.rya$1=rya
endif

rho=get_range(0.,1., 11)

if keyword_set(enein) then begin
	if n_elements(enein) ne 11 then begin
		if keyword_set(renein) then begin
			if n_elements(renein) eq n_elements(enein) then begin
				enein=spline(renein, enein, rho)
				renein=rho
			endif else begin
				print, '  !!! write_cqlinput: enein and renein have different number of elements'
				return, 0
			endelse
		endif
	endif
	c.setup$.enein=enein
endif
	
if keyword_set(tein) then begin
	if n_elements(tein) ne 11 then begin
		if keyword_set(rtein) then begin
			if n_elements(rtein) eq n_elements(tein) then begin
				tein=spline(rtein, tein, rho)
				rtein=rho
			endif else begin
				print, '  !!! write_cqlinput: tein and rtein have different number of elements'
				return, 0
			endelse
		endif
	endif
	c.setup$.tein=tein
endif
	
if keyword_set(tiin) then begin
	if n_elements(tiin) ne 11 then begin
		if keyword_set(rtiin) then begin
			if n_elements(rtiin) eq n_elements(tiin) then begin
				tiin=spline(rtiin, tiin, rho)
				rtiin=rho
			endif else begin
				print, '  !!! write_cqlinput: tiin and rtiin have different number of elements'
				return, 0
			endelse
		endif
	endif
	c.setup$.tiin=tiin
endif

if keyword_set(zeffin) then begin
	if n_elements(zeffin) ne 11 then begin
		if keyword_set(rzeffin) then begin
			if n_elements(rzeffin) eq n_elements(zeffin) then begin
				zeffin=spline(rzeffin, zeffin, rho)
				rzeffin=rho
			endif else begin
				print, '  !!! write_cqlinput: enein and renein have different number of elements'
				return, 0
			endelse
		endif
	endif
	c.setup$.zeffin=zeffin
endif

if keyword_set(elec_field) then begin
	if n_elements(elec_field) ne 11 then begin
		print, '  !!! write_cqlinput: wrong number of elecin(1), aborting.'
		return, 0
	endif else c.setup$.elecin=elec_field
endif

if keyword_set(bt_sign) then c.eqsetup.bsign=bt_sign

if keyword_set(outfile) then outf=outfile else outf='cqlinput'

if keyword_set(ech_on) then $
	if ech_on eq '.false.' then c.rfsetup.urfmod=ap+'disabled'+ap $
	else c.rfsetup.urfmod=ap+'enabled'+ap

if keyword_set(efield_on) then $
	if efield_on eq '.false.' then c.setup$.nonel=10000 $
	else c.setup$.nonel=0

if keyword_set(nc_rf_flux) then $
	if nc_rf_flux eq '.true.' then begin
		c.setup$.netcdfvecrf=ap+'x-theta'+ap
		c.setup$.netcdfvecc=ap+'x-theta'+ap
	endif else c.setup$.netcdfvecrf=ap+'disabled'+ap

if keyword_set(nc_co_flux) then $
	if nc_co_flux eq '.true.' then begin
		c.setup$.netcdfvecc=ap+'x-theta'+ap 
		c.setup$.netcdfvecc=ap+'x-theta'+ap
	endif else c.setup$.netcdfvecc=ap+'disabled'+ap

if keyword_set(nc_tot_flux) then $
	if nc_tot_flux eq '.true.' then begin
		c.setup$.netcdfvecal=ap+'x-theta'+ap
		c.setup$.netcdfvecc=ap+'x-theta'+ap
	endif else c.setup$.netcdfvecal=ap+'disabled'+ap

write_nl2, c, outf, delim='&'
return, 1
end
