; write_torayin.pro
; Writes toray.in file
; returns 1 for success, 0 for failure

pro write_torayin, $
	infile=infile, $	; toray.in template file
	header=header, $	; header message
	igafit=igafit, $	; 1=run gafit, 0=not
	ecpower=ecpower, $	; in W
	smax=smax, $		; max ray length in cm
	ds=ds, $			; distance in cm between calculated ray points
	minds=minds, $		; min distance between ray points
	fdout=fdout, $		; output ever fdout'th point
	cr=cr, $			; array orientation in radians around central ray
	mray=mray, $		; array of number of rays in annuli, for gaussian pattern
	gauszone=gauszone, $; number of annuli in ray pattern
	nharm=nharm, $		; nharm for where ECCD is calculated
	pattern=pattern, $	; '.km.' for old Matsuda pattern, otherwise gaussian
	netcdfdat=netcdfdat, $	; .true. or .false. , default is .false.
	cqldat=cqldat, $	; .true. or .false. , default is .false.
	pwrfmin=pwrfmin, $	; minimum power fraction (us 1.e-3)
	outfile=outfile, $	; output file; toray.in is default
	status=status, $
	data=data, $		; returns the data of toray.in
	delim=delim			; delimiter in namelist files

status=0

if not keyword_set(delim) then delim='&'

if not keyword_set(infile) then infile='$ECHRES_PATH/toray.in_template'
fh=file_search(infile, count=count)
if count ne 1 then begin
	print, '  !!! write_torayin: error opening template file '+infile
	return
endif
nl=read_nl2(infile, delim=delim)
t=nl.edata

; write data to file toray.in
if keyword_set(outfile) then outfile=outfile else outfile='toray.in'
openw, diskf, /get_lun, outfile

; Hierarchy is keyword, then infile data if present, then defaults
if keyword_set(header) then header=header else $
if keyword_set(infile) then begin
	pos=strpos(strupcase(tag_names(nl)), 'HEADER')
	if pos[0] ne -1 then header=nl.header 
endif else header=['  toray.in file written by write_torayin.pro']
printf, diskf, header

printf, diskf, " " + delim + "EDATA"

if keyword_set(ecpower) then ec_power=ecpower*1.0e7 else begin
	pos=where(strpos(tag_names(t), 'POWINC') ne -1)
	if pos[0] ne -1 then ec_power=t.powinc else ec_power=1.e13 ; 1 MW
endelse
ec_power=float(ec_power[0])
printf, diskf, "  powinc = ", ec_power

if keyword_set(smax) then smax=smax else begin
	pos=where(strpos(tag_names(t), 'SMAX') ne -1)
	if pos[0] ne -1 then smax=t.smax else smax=250.
endelse
smax=float(smax[0])
printf, diskf, "  smax = ", smax

if keyword_set(ds) then ds=ds else begin
	pos=where(strpos(tag_names(t), 'DS') ne -1)
	if pos[0] ne -1 then ds=t.ds else ds=1.0
endelse
ds=float(ds[0])
printf, diskf, "  ds = ", ds

if keyword_set(minds) then minds=minds else begin
	pos=where(strpos(tag_names(t), 'DSMIN') ne -1)
	if pos[0] ne -1 then minds=t.dsmin else minds=ds/5.
endelse
minds=float(minds[0])
printf, diskf, "  dsmin = ", minds

if keyword_set(pwrfmin) then pwrfmin=pwrfmin else begin
	pos=where(strpos(tag_names(t), 'PWRFMIN') ne -1)
	if pos[0] ne -1 then pwrfmin=t.pwrfmin else pwrfmin=0.00010 ; 1 MW
endelse
pwrfmin=float(pwrfmin[0])
printf, diskf, "  pwrfmin = ", float(pwrfmin)

if keyword_set(igafit) then igafit=igafit else igafit=0
; Note: segmentation error in toray on venus if igafit ne 1
printf, diskf, "  igafit = ", 1

if keyword_set(fdout) then fdout=fdout else begin
	pos=where(strpos(tag_names(t), 'FDOUT') ne -1)
	if pos[0] ne -1 then fdout=t.fdout else fdout=2.0
endelse
fdout=float(fix(fdout[0]))
printf, diskf, "  fdout = ", fdout

if keyword_set(nharm) then nharm=nharm else begin
	pos=where(strpos(tag_names(t), 'NHARM') ne -1)
	if pos[0] ne -1 then nharm=t.nharm else nharm = 2
endelse
nharm=fix(nharm[0])
printf, diskf, "  nharm = ", nharm

if keyword_set(pattern) then pattern=pattern else begin
	pos=where(strpos(tag_names(t), 'PATTERN') ne -1)
	if pos[0] ne -1 then pattern=t.pattern else pattern=''
endelse
if pattern ne '' then printf, diskf, "  pattern = ", pattern

if keyword_set(cr) then cr=cr else begin
	pos=where(strpos(tag_names(t), 'CR$') ne -1)
	if pos[0] ne -1 then begin
		cr=fltarr(n_elements(pos))
		for jj=0, n_elements(pos)-1 do cr[jj]=t.(pos[jj]) 
	endif else cr=[1.]
endelse 
cr=float(cr)
if n_elements(cr) gt 1 then $
	for ii=0, n_elements(cr)-1 do $
		printf, diskf, "  cr(" + strtrim(string(ii+1),2) + ") = ", fix(cr[ii])
		
if keyword_set(mray) then mray=mray else begin
	pos=where(strpos(tag_names(t), 'MRAY$') ne -1)
	if pos[0] ne -1 then begin
		mray=intarr(n_elements(pos))
		for jj=0, n_elements(pos)-1 do mray[jj]=t.(pos[jj]) 
	endif else mray=[1]
endelse
mray[0]=1
if n_elements(mray) gt 1 then begin
	mray=fix(mray)
	for ii=0, n_elements(mray)-1 do $
		printf, diskf, "  mray(" + strtrim(string(ii+1),2) + ") = ", mray[ii]
	gzone = n_elements(mray)
endif else gzone=1

if keyword_set(gauszone) then gzone=gauszone else begin
	pos=where(strpos(tag_names(t), 'GAUSZONE') ne -1)
	if pos[0] ne -1 then gzone = t.(pos[0])
endelse
gzone=fix(gzone[0])
printf, diskf, "  gauszone = ", gzone

nc='.false.'
if keyword_set(netcdfdat) then $
	if ((string(netcdfdat) eq string(1)) or (string(netcdfdat) eq '.true.')) then nc='.true.' 
if not keyword_set(netcdfdat) then begin
	pos=where(strpos(tag_names(t), 'NETCDFDAT') ne -1)
	if pos[0] ne -1 then nc=t.netcdfdat else nc='.false.'
endif
nc=nc[0]
if nc ne '.true.' then cq='.false.'
printf, diskf, "  netcdfdat = ", nc

cq='.false.'
if keyword_set(cqldat) then $
	if ((string(cqldat) eq string(1)) or (string(cqldat) eq '.true.')) then cq='.true.' 
if not keyword_set(cqldat) then begin
	pos=where(strpos(tag_names(t), 'CQLDAT') ne -1)
	if pos[0] ne -1 then cq=t.cqldat else cq='.false.'
endif
cq=cq[0]
if cq ne '.true.' then cq='.false.'
printf, diskf, "  cqldat = ", cq

printf, diskf, "  nlout(1) = .false."	; suppress printing ecrh1001
printf, diskf, "  nlout(2) = .false."	; suppress printing ecrh2001

printf, diskf, " &END_OF_EDATA"

close, diskf
free_lun, diskf

status=1

data={cqldat:cq, netcdfdat:nc, gauszone:gzone, $
	mray:mray, cr:cr, pattern:pattern, nharm:nharm, $
	fdout:fdout, pwrfmin:pwrfmin, $
	smax:smax, dsmin:minds, ds:ds, $
	powinc:ec_power, igafit:igafit}

return

end

