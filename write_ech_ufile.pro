; write_echdat.pro
; Obtains data from DIII-D MDSplus using get_ech() and writes a file
;	(default name 'echdat_shotnumber.nc') containing time-dependent power and other 
;	data for use with transport codes like ONETWO and TRANSP
; R. Prater, 10/28/2009

; 7/18/2012 - added ALL data needed to run toray (including toray.in values)
; 7/19/2012 - changed all floats to double for compatibility with onetwo compilation
; 7/24/2012 - added ability to use namelist file for inputs

@$ECHRES_PATH/get_ech
@$ECHRES_PATH/nearest_index
@$ECHRES_PATH/get_range
@$ECHRES_PATH/readnc
@$ECHRES_PATH/read_nl2
@$ECHRES_PATH/read_echin

pro write_ech_ufile, shot, dtmin=dtmin, dtavg=dtavg, file=file, $
	nharm=nharm, gauszone=gauszone, modelc=modelc, netcdfdat=netcdfdat, $
	irfcur=irfcur, ratwec=ratwec, idamp=idamp, igafit=igafit, $
	ech_namelist=ech_namelist, status=status, error_message=error_message
	; dtmin is approximate time in msec between fast samples, 
	;	default=0.01 msec
	; dtavg is full smoothing time in msec for power data 
	;	(p/m dtavg/2), default is 0.0
	; file is output nc file name, default is echin.nc
	; set the following to either a scalar or an array of size nsys
	;	nharm (default is 2)
	;	gauszone (default is 4)
	;	modelc (default is 5)
	;	netcdfdat (default is 1, to write netcdf file; 0 to not write it)
	;	irfcur (default is 1.0)
	;	ratwec (default is 1.0)
	;	idamp (default is 8)
	;	igafit (default is 1, to run gafit; otherwise, set to 0)
	; status returns false if an error, otherwise true 
	;
	;
	; If ech_namelist is set to point to a file, 
	;	then a namelist overwrites all other entries.
	;	the namelist file is like:
;********************************************************
;
;namelist for entering ech data into echin.nc file
;&ECHDATA
; MACHINE='FDF'
; TMIN=0.		! timebase starts (msec)
; TMAX=10.	! timebase ends (msec)
; NTIMES=501	! number points in timebase
; !
; ! From/for inone
; !
; TORAY_VERSION =  1.90000
; TORAY_PATH = '/task/imd/toray/xtoray'
; RFON =   3.00080  3.03185  3.00080  3.00135  3.00055  3.00125  1.00000e+06 1.00000e+06
; RFTIME =   2.79725  1.88685  2.79725  2.79680  2.79735  2.79720  -1.00000 -1.00000
; IRFCUR = 8*1
; FREQ = 8*1.10000e+11
; WRFO = 8*0.00000
; HLWEC = 8*1.70000
; RATWEC = 8*1.00000
; RFPOW =   475669.  51996.1  533710.  301483.  526338.  453911.  0.00000 0.00000
; THETEC =   97.3652  108.290  109.799  96.6361  111.438  109.102  0.00000 0.00000
; PHAIEC =   202.729  201.135  200.585  187.220  200.228  199.657  180.000 180.000
; XEC =   239.990  239.990  239.990  239.990  239.990  239.990  239.400  239.400
; ZEC =   67.9400  67.9400  67.9400  67.9400  67.9400  67.9400  69.6100  69.6100
; NRAY = 8*30
; IDAMP = 8*8
; !
; ! From/for toray.in
; !
; IGAFIT = 8*1
; NHARM = 8*1
; GAUSZONE = 8*4
; NETCDFDAT = 8*1
; MODELC = 8*5
; /
;
;******************************************************
;

status=0B
error_message=''

;catch, error_stat
;if error_stat ne 0 then begin
;		error_message=!ERROR_STATE.MSG
;		print, error_message
;        catch, /cancel
;        return
;endif

if not keyword_set(dtmin) 	then dtmin=0.2	; msec between fast samples
if not keyword_set(file) 	then file='echin.nc'
if not keyword_set(dtavg) 	then dtavg=0.0 	; msec total avg time
if not keyword_set(nharm) 	then nharm0=2 else nharm0=nharm
if not keyword_set(gauszone) then gauszone0=4 else gauszone0=gauszone
if not keyword_set(modelc)  then modelc0=5 else modelc0=modelc
if not keyword_set(netcdfdat)  then netcdfdat0=1 else netcdfdat0=netcdfdat
if not keyword_set(irfcur)  then irfcur0=1.0 else irfcur0=irfcur
if not keyword_set(ratwec)  then ratwec0=1.0 else ratwec0=ratwec
if not keyword_set(idamp) 	then idamp0=8 else idamp0=idamp

;if not keyword_set(igafit)  then igafit0=1 else igatfit0=igafit
; segmentation problem with gafit/toray unless igafit=1
igafit0=1

if n_elements(ech_namelist) eq 0 then ech_namelist='' else begin
	efh=file_search(ech_namelist, /full)
	if efh[0] eq '' then begin
		message, 'No namelist file found: '+efh[0], /info
		error_message='No namelist file found'
		return
	endif
endelse

;*************** for namelist data entry ************************
if ech_namelist ne '' then begin
	nl=read_nl2(efh[0])
	ntimes=nl.echdata.ntimes
	times=get_range(nl.echdata.tmin, nl.echdata.tmax, ntimes)
	dtmin=(nl.echdata.tmax - nl.echdata.tmin)/float(ntimes)
	nsys=n_elements(nl.echdata.rftime)
	dt_fast=0.
	t_avg=0
	gyrotron=strarr(nsys)
	sysnum=intarr(nsys)
	portname=strarr(nsys)
	portnum=intarr(nsys)
	antname=strarr(nsys)
	antnum=intarr(nsys)
	freq=nl.echdata.freq
	dispersion=nl.echdata.hlwec
	ratwec=nl.echdata.ratwec
	idamp=nl.echdata.idamp
	nharm=nl.echdata.nharm
	modelc=nl.echdata.modelc
	irfcur=nl.echdata.irfcur
	netcdfdat=nl.echdata.netcdfdat
	igafit=nl.echdata.igafit
	gauszone=nl.echdata.gauszone
	
	; time-dependent quantities
	launchr=fltarr(ntimes, nsys)
	launchz=fltarr(ntimes, nsys)
	polarang=fltarr(ntimes, nsys)
	aziang=fltarr(ntimes, nsys)
	omode_frac=fltarr(ntimes, nsys)
	for j=0L, ntimes-1 do begin
		launchr[j,*]=nl.echdata.xec
		launchz[j,*]=nl.echdata.zec
		polarang[j,*]=nl.echdata.thetec
		aziang[j,*]=nl.echdata.phaiec
		omode_frac[j,*]=nl.echdata.wrfo
	endfor
	
	pwr=fltarr(ntimes, nsys)
	for i=0, nsys-1 do begin
		k=where((times ge nl.echdata.rfon[i]) and $
			(times le (nl.echdata.rfon[i]+nl.echdata.rftime[i])), nk)
		if nk gt 0 then pwr[k, i]=nl.echdata.rfpow[i]
	endfor
	
	goto, write_file

endif

;*************** for MDSplus data (DIII-D) **********************

if getenv('mdsip') eq '' then mdsconnect, 'atlas'

e=get_ech(shot, 0.0)

if not e.stat then begin
	message, 'Could not open RF tree for shot ' + strtrim(shot,2), /info
	error_message='Could not open RF tree'
	return
endif

; see if ECH is on, accounting for some noise
i=where(smooth(e.echpwrc,21, /edge_truncate) gt 35.e3, count)
if count lt 1 then begin
	message, 'No ECH power for shot ' + strtrim(shot,2), /info
	error_message='No ECH power'
	return
endif

; be sure to get beginning and end of power traces
;extraind=20
;addind=indgen(extraind)
;itemp=max(i)
;if max(i) lt n_elements(e.time)-extraind-1 then i=[i, itemp+addind+1]
;itemp=min(i)
;if itemp gt extraind+1 then i=[itemp-reverse(addind)-1, i]

i0=min(i)
i1=max(i)
dt=e.time[i0+1]-e.time[i0]
dtmin=max([dt, dtmin])
iskip=long(dtmin/dt) > 1
ntimes=long(count/iskip)+1
ind=long(get_range(i0, i1, ntimes))
iavg=2*long(dtavg/dt/2.)+1
iavgmax=long(100./dt)	; limit to 100 msec to avoid error
iavg=min([iavgmax, iavg])
times=e.time[ind]

; get_ech at times of interest; fast because data come from cache
e=get_ech(shot, times)

nsys=e.num_systems
nstimes=n_elements(e.stime)

; set some parameters for toray calculation
if n_elements(nharm0) eq 1 then nharm=replicate(nharm0, nsys) else $
	if n_elements(nharm0) eq nsys then nharm=nharm0 else begin
		message, 'Wrong number of nharm elements; using first one', /info
		nharm=replicate(nharm0[0], nsys)
	endelse
if n_elements(gauszone0) eq 1 then gauszone=replicate(gauszone0, nsys) else $
	if n_elements(gauszone0) eq nsys then gauszone=gauszone0 else begin
		message, 'Wrong number of gauszone elements; using first one', /info
		gauszone=replicate(gauszone0[0], nsys)
	endelse 
if n_elements(modelc0) eq 1 then modelc=replicate(modelc0, nsys) else $
	if n_elements(modelc0) eq nsys then modelc=modelc0 else begin
		message, 'Wrong number of modelc elements; using first one', /info
		modelc=replicate(modelc0[0], nsys)
	endelse
if n_elements(netcdfdat0) eq 1 then netcdfdat=replicate(netcdfdat0, nsys) else $
	if n_elements(netcdfdat0) eq nsys then netcdfdat=netcdfdat0 else begin
		message, 'Wrong number of netcdfdat elements; using first one', /info
		netcdfdat=replicate(netcdfat0[0], nsys)
	endelse
if n_elements(irfcur0) eq 1 then irfcur=replicate(irfcur0, nsys) else $
	if n_elements(irfcur0) eq nsys then irfcur=irfcur0 else begin
		message, 'Wrong number of irfcur elements; using first one', /info
		irfcur=replicate(irfcur0[0], nsys)
	endelse
if n_elements(ratwec0) eq 1 then ratwec=replicate(ratwec0, nsys) else $
	if n_elements(ratwec0) eq nsys then ratwec=ratwec0 else begin
		message, 'Wrong number of ratwec elements; using first one', /info
		ratwec=replicate(ratwec0[0], nsys)
	endelse
if n_elements(idamp0) eq 1 then idamp=replicate(idamp0, nsys) else $
	if n_elements(idamp0) eq nsys then idamp=idamp0 else begin
		message, 'Wrong number of idamp elements; using first one', /info
		idamp=replicate(idamp0[0], nsys)
	endelse
;if n_elements(igafit0) eq 1 then igafit=replicate(igafit0, nsys) else $
;	if n_elements(igafit0) eq nsys then igafit=igafit0 else $
;		message, 'Wrong number of igafit elements'
igafit=replicate(1, nsys)	; see note above

launchr=fltarr(ntimes, nsys)
launchz=fltarr(ntimes, nsys)
polarang=fltarr(ntimes, nsys)
aziang=fltarr(ntimes, nsys)

for k=0, nsys-1 do begin
	launchr[*,k]=e.systems[k].launch_r
	launchz[*,k]=e.systems[k].launch_z
	polarang[*,k]=e.systems[k].ray_polars
	aziang[*,k]=e.systems[k].ray_azis
endfor

freq=e.systems.freq
mm=where(freq eq 0, count)
if count gt 0 then freq[mm]=1.1e11
omode_frac=1. - e.systems.xmfracs

gyrotron=e.systems.gyrotron
sysnum=e.systems.tank_number
portname=e.systems.port
portnum=e.systems.antenna_number
antname=e.systems.drawing_num
antnum=e.systems.antenna_number
dispersion=e.systems.dispersion

pwr=fltarr(ntimes, nsys)
for k=0, nsys-1 do begin
	ptemp=smooth(e.systems[k].pinj, iavg, /edge_truncate)
	pwr[*,k]=ptemp[ind]
endfor

write_file:

; open a netcdf file for the ECH data
ncid=ncdf_create(file, /clobber)
ncdf_attput, ncid, 'Function', 'Data for time-dependent ONETWO', /global
nid=ncdf_dimdef(ncid, 'nsys', nsys)			; number of systems
tid=ncdf_dimdef(ncid, 'ntimes', ntimes) 	; number of times for data
gid=ncdf_dimdef(ncid, 'namelength', 12) 	; max length gyrotron names
did=ncdf_dimdef(ncid, 'stringlength', 64)	; max length for strings

; define scalar variables
u4=ncdf_vardef(ncid, 'date', did, /char)

u5=ncdf_vardef(ncid, 'user', did, /char)

v0=ncdf_vardef(ncid, 'shot', /long)
ncdf_attput, ncid, v0, 'Description', 'Shot number'

v1=ncdf_vardef(ncid, 'nsys', /long)
ncdf_attput, ncid, v1, 'Description', 'Number of ECH systems'

u0=ncdf_vardef(ncid, 'dt_fast', /double)
ncdf_attput, ncid, u0, 'Description', 'Time between power samples for power time base'
ncdf_attput, ncid, u0, 'Units', 'msec'

u1=ncdf_vardef(ncid, 't_avg', /double)
ncdf_attput, ncid, u1, 'Description', 'Full averaging time for power'
ncdf_attput, ncid, u1, 'Units', 'msec'

; define size nsys variables
v2=ncdf_vardef(ncid, 'gyrotron', [gid, nid], /char)
ncdf_attput, ncid, v2, 'Description', 'Gyrotron name'

vsysnum=ncdf_vardef(ncid, 'sys_number', nid, /short)
ncdf_attput, ncid, vsysnum, 'Description', 'System number in gyrotron vault'

vportname=ncdf_vardef(ncid, 'port_name', [gid, nid], /char)
ncdf_attput, ncid, vportname, 'Description', 'Port name'

vportnum=ncdf_vardef(ncid, 'port_number', nid, /short)
ncdf_attput, ncid, vportnum, 'Description', 'Port number, def in echcal.pro'

vantname=ncdf_vardef(ncid, 'ant_name', [gid, nid], /char)
ncdf_attput, ncid, vantname, 'Description', 'Antenna name'

vantnum=ncdf_vardef(ncid, 'ant_number', nid, /short)
ncdf_attput, ncid, vantnum, 'Description', 'antenna number, def in echcal.pro' 

v3=ncdf_vardef(ncid, 'FREQ', nid, /double)
ncdf_attput, ncid, v3, 'Description', 'Gyrotron frequency (FREQ)'
ncdf_attput, ncid, v3, 'Units', 'Hz'

v4a=ncdf_vardef(ncid, 'IGAFIT', nid, /short)
ncdf_attput, ncid, v4a, 'Description', '1 to run gafit, otherwise 0'

v4c=ncdf_vardef(ncid, 'GAUSZONE', nid, /short)
ncdf_attput, ncid, v4c, 'Description', 'Number Gaussian zones: 4 -> 30rays'

v4k=ncdf_vardef(ncid, 'NRAY', nid, /short)
ncdf_attput, ncid, v4k, 'Description', '30; number rays set by gauszone variable'

v4=ncdf_vardef(ncid, 'HLWEC', nid, /double)
ncdf_attput, ncid, v4, 'Description', 'Beam divergence, HWHM (HLWEC)'
ncdf_attput, ncid, v4, 'Units', 'deg'

v4g=ncdf_vardef(ncid, 'RATWEC', nid, /double)
ncdf_attput, ncid, v4g, 'Description', 'Aspect ratio of launcher pattern'

v4i=ncdf_vardef(ncid, 'IDAMP', nid, /short)
ncdf_attput, ncid, v4i, 'Description', 'Damping model'

v4b=ncdf_vardef(ncid, 'NHARM', nid, /short)
ncdf_attput, ncid, v4b, 'Description', 'harmonic number for ECCD calculation'

v4d=ncdf_vardef(ncid, 'MODELC', nid, /short)
ncdf_attput, ncid, v4d, 'Description', 'Model for ECCD'

v4f=ncdf_vardef(ncid, 'IRFCUR', nid, /double)
ncdf_attput, ncid, v4f, 'Description', 'Multiplier of ECCD'

v4e=ncdf_vardef(ncid, 'NETCDFDAT', nid, /short)
ncdf_attput, ncid, v4e, 'Description', '1 to write netcdf output file, 0 to not'

; define size ntimes variables
vtime=ncdf_vardef(ncid, 'time', tid, /double)
ncdf_attput, ncid, vtime, 'Description', 'Power time base'
ncdf_attput, ncid, vtime, 'Units', 'msec'

vpwr=ncdf_vardef(ncid, 'RFPOW', [tid, nid], /double)
ncdf_attput, ncid, vpwr, 'Description', 'Incident power each gyrotron (RFPOW)'
ncdf_attput, ncid, vpwr, 'Units', 'W'

; define size nstimes variables
;s1=ncdf_vardef(ncid, 'stime', tid, /float)
;ncdf_attput, ncid, s1, 'Description', 'Slow time base for beam aiming parameters'
;ncdf_attput, ncid, s1, 'Units', 'msec'

v5=ncdf_vardef(ncid, 'WRFO', [tid, nid], /double)
ncdf_attput, ncid, v5, 'Description', 'Fraction of incident power in O-mode (WRFO)'

s2=ncdf_vardef(ncid, 'XEC', [tid, nid], /double)
ncdf_attput, ncid, s2, 'Description', 'Major radius of launch location (XEC)'
ncdf_attput, ncid, s2, 'Units', 'm'

s3=ncdf_vardef(ncid, 'ZEC', [tid, nid], /double)
ncdf_attput, ncid, s3, 'Description', 'Z of launch location (ZEC)'
ncdf_attput, ncid, s3, 'Units', 'm'

s4=ncdf_vardef(ncid, 'PHAIEC', [tid, nid], /double)
ncdf_attput, ncid, s4, 'Description', 'Azimuthal angle of launched beam (PHAIEC)'
ncdf_attput, ncid, s4, 'Units', 'deg'

s5=ncdf_vardef(ncid, 'THETEC', [tid, nid], /double)
ncdf_attput, ncid, s5, 'Description', 'Polar angle of launched beam (THETEC)'
ncdf_attput, ncid, s5, 'Units', 'deg'

; enter data entry mode
ncdf_control, ncid, /endef

; now enter data
ncdf_varput, ncid, u4, systime()
ncdf_varput, ncid, u5, getenv('USER')
ncdf_varput, ncid, v0, shot
ncdf_varput, ncid, u0, double(dtmin)
ncdf_varput, ncid, u1, double(dtavg)
ncdf_varput, ncid, v1, nsys
ncdf_varput, ncid, v2, gyrotron
ncdf_varput, ncid, vsysnum, sysnum
ncdf_varput, ncid, vportname, portname
ncdf_varput, ncid, vportnum, portnum
ncdf_varput, ncid, vantname, antname
ncdf_varput, ncid, vantnum, antnum
ncdf_varput, ncid, v3, double(freq)
ncdf_varput, ncid, v4, double(dispersion)

ncdf_varput, ncid, v4a, igafit
ncdf_varput, ncid, v4k, replicate(30, nsys)
ncdf_varput, ncid, v4c, gauszone
ncdf_varput, ncid, v4g, double(ratwec)
ncdf_varput, ncid, v4i, idamp
ncdf_varput, ncid, v4b, nharm
ncdf_varput, ncid, v4d, modelc
ncdf_varput, ncid, v4f, double(irfcur)
ncdf_varput, ncid, v4e, netcdfdat

ncdf_varput, ncid, v5, double(omode_frac)
ncdf_varput, ncid, vtime, double(times)
ncdf_varput, ncid, vpwr, double(pwr)
ncdf_varput, ncid, s2, double(launchr)
ncdf_varput, ncid, s3, double(launchz)
ncdf_varput, ncid, s4, double(aziang)
ncdf_varput, ncid, s5, double(polarang)

ncdf_close, ncid

status=1B
message, 'Wrote file '+file, /info

return

end
