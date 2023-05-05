; combine_toraync
; Reads toray.nc files in ncfilelist array and writes a new nc file
;	that has the ray data concatenated, for use with CQL3D
;	Returns the structure of the combined nc files.
; R. Prater, 20091113

function combine_toraync, ncfilelist, outfile=outfile

if not keyword_set(outfile) then outfile='combined_toraync.nc'
if not keyword_set(att_file) then att_file=''

nfiles=n_elements(ncfilelist)

nc0=readnc(ncfilelist[0])
maxnrayelt=max(nc0.nrayelt)
radres=n_elements(nc0.weecrh)
nray=nc0.nray
totnray=nray*nfiles

nrayelt=lonarr(totnray)
; get max number of ray elements
for i=0, nfiles-1 do begin
	s=readnc(ncfilelist[i])
	nrayelt[i*nray:(i+1)*nray-1]=s.nrayelt
endfor
maxnrayelt=max(nrayelt)

; create new structure t with totnray rays
t=create_struct( 'GYROTRON', strarr(nfiles), 'NSYSTEMS', 0, $
	'NRAY', totnray, 'NHARM', nc0.nharm, 'IDAMP', nc0.idamp, $
	'FREQCY', nc0.freqcy, 'LEDGE', nc0.ledge, 'PSIMAG', nc0.psimag, 'PSILIM', nc0.psilim, $
	'RAXIS', nc0.raxis, 'ZAXIS', nc0.zaxis, 'BTAXIS', nc0.btaxis, 'VOLUME', nc0.volume, $
	'XMRHO', nc0.xmrho, 'WEECRH', fltarr(radres), 'CURRF', fltarr(radres), $
	'WEECRH_i', fltarr(radres, nfiles), 'CURRF_i', fltarr(radres, nfiles), $
	'XRHO', nc0.xrho, 'XETE', nc0.xete, 'XENE', nc0.xene, 'POWER', fltarr(nfiles))

arr=dblarr(totnray)
mat=dblarr(maxnrayelt, totnray)
t=create_struct(t, 'X0', arr, 'Y0', arr, 'Z0', arr, $
	'ANGRID1', arr, 'ANGRID2', arr, 'NRAYELT', nrayelt, $
	'WS', mat, 'SPSI', mat, 'WR', mat, 'WPHI', mat, 'WZ', mat, 'WNPAR', mat, 'WNPER', mat, $
	'DELPWR', mat, 'FLUXN', mat, 'SVGRPDC', mat, 'SBTOT', mat, 'SASPCTI', mat, $
	'SENE', mat, 'STE', mat, 'SALPHAC', mat, 'SALPHAL', mat, $
	'CWEXDE', dblarr(maxnrayelt, totnray, 2), $
	'CWEYDE', dblarr(maxnrayelt, totnray, 2), 'CWEZDE', dblarr(maxnrayelt, totnray, 2))

; now populate the new structure
power=fltarr(nfiles)
for i=0, nfiles-1 do begin
	k=indgen(nray)+i*nray
	s=readnc(ncfilelist[i])
	stgs=tag_names(s)
	iii=where(stgs eq 'GYROTRON')
	if iii ge 0 then t.gyrotron[i]=s.gyrotron
	t.x0[k]=s.x0
	t.y0[k]=s.y0
	t.z0[k]=s.z0
	t.angrid1[k]=s.angrid1
	t.angrid2[k]=s.angrid2
	t.nrayelt[k]=s.nrayelt
	pow=total(s.delpwr[0,*])/1.e7	; Watts
	power[i]=pow
	t.weecrh += s.weecrh*pow
	t.weecrh_i[*,i]=s.weecrh*pow
	t.currf += s.currf*pow
	t.currf_i[*,i]=s.currf*pow
	for j=0, nray-1 do begin
		t.ws[0:s.nrayelt[j]-1,k[j]]        = s.ws[0:s.nrayelt[j]-1,j]
		t.wr[0:s.nrayelt[j]-1,k[j]]        = s.wr[0:s.nrayelt[j]-1,j]
		t.wphi[0:s.nrayelt[j]-1,k[j]]      = s.wphi[0:s.nrayelt[j]-1,j]
		t.wz[0:s.nrayelt[j]-1,k[j]]        = s.wz[0:s.nrayelt[j]-1,j]
		t.spsi[0:s.nrayelt[j]-1,k[j]]      = s.spsi[0:s.nrayelt[j]-1,j]
		t.wnpar[0:s.nrayelt[j]-1,k[j]]     = s.wnpar[0:s.nrayelt[j]-1,j]
		t.wnper[0:s.nrayelt[j]-1,k[j]]     = s.wnper[0:s.nrayelt[j]-1,j]
		t.delpwr[0:s.nrayelt[j]-1,k[j]]    = s.delpwr[0:s.nrayelt[j]-1,j]
		t.fluxn[0:s.nrayelt[j]-1,k[j]]     = s.fluxn[0:s.nrayelt[j]-1,j]
		t.sene[0:s.nrayelt[j]-1,k[j]]      = s.sene[0:s.nrayelt[j]-1,j]
		t.ste [0:s.nrayelt[j]-1,k[j]]	   = s.ste[0:s.nrayelt[j]-1,j]
		t.salphac[0:s.nrayelt[j]-1,k[j]]   = s.salphac[0:s.nrayelt[j]-1,j]
		t.salphal[0:s.nrayelt[j]-1,k[j]]   = s.salphal[0:s.nrayelt[j]-1,j]
		t.svgrpdc[0:s.nrayelt[j]-1,k[j]]   = s.svgrpdc[0:s.nrayelt[j]-1,j]
		t.sbtot[0:s.nrayelt[j]-1,k[j]]     = s.sbtot[0:s.nrayelt[j]-1,j]
		t.saspcti[0:s.nrayelt[j]-1,k[j]]   = s.saspcti[0:s.nrayelt[j]-1,j]
		t.cwexde[0:s.nrayelt[j]-1,k[j],0]  = s.cwexde[0:s.nrayelt[j]-1,j,0]
		t.cwexde[0:s.nrayelt[j]-1,k[j],1]  = s.cwexde[0:s.nrayelt[j]-1,j,1]
		t.cweyde[0:s.nrayelt[j]-1,k[j],0]  = s.cweyde[0:s.nrayelt[j]-1,j,0]
		t.cweyde[0:s.nrayelt[j]-1,k[j],1]  = s.cweyde[0:s.nrayelt[j]-1,j,1]
		t.cwezde[0:s.nrayelt[j]-1,k[j],0]  = s.cwezde[0:s.nrayelt[j]-1,j,0]
		t.cwezde[0:s.nrayelt[j]-1,k[j],1]  = s.cwezde[0:s.nrayelt[j]-1,j,1]
	endfor
endfor

; open netcdf file and enter dimensions
ncid=ncdf_create(outfile, /clobber)
inelt=ncdf_dimdef(ncid, 'neltmax', maxnrayelt)
inray=ncdf_dimdef(ncid, 'nrays', totnray)
itwo=ncdf_dimdef(ncid, 'two', 2)
iledge=ncdf_dimdef(ncid, 'ledge', nc0.ledge)
iledgem1=ncdf_dimdef(ncid, 'ledgem1', nc0.ledge-1)
insys=ncdf_dimdef(ncid, 'nsys', nfiles)
itwelve=ncdf_dimdef(ncid, 'twelve', 12)

; define scalar variables
v0=ncdf_vardef(ncid, 'nsystems', /long)
v1=ncdf_vardef(ncid, 'idamp', /long)
v2 = ncdf_vardef(ncid, 'nray', /long)
v3 = ncdf_vardef(ncid, 'nharm', /long)
v4 = ncdf_vardef(ncid, 'freqcy', /double)

; define variables of size nsys
v4b=ncdf_vardef(ncid, 'GYROTRON', [itwelve, insys], /char)
v4c=ncdf_vardef(ncid, 'POWER', insys, /double)

; define variables of size nray
v5=ncdf_vardef(ncid, 'x0', inray, /double)
v6=ncdf_vardef(ncid, 'y0', inray, /double)
v7=ncdf_vardef(ncid, 'z0', inray, /double)
v8=ncdf_vardef(ncid, 'angrid1', inray, /double)
v9=ncdf_vardef(ncid, 'angrid2', inray, /double)
v10=ncdf_vardef(ncid, 'nrayelt', inray, /long)

; define variables of size [nrays, neltmax]
v11=ncdf_vardef(ncid, 'ws', [inelt, inray], /double)
v12=ncdf_vardef(ncid, 'spsi', [inelt, inray], /double)
v13=ncdf_vardef(ncid, 'wr', [inelt, inray], /double)
v14=ncdf_vardef(ncid, 'wphi', [inelt, inray], /double)
v15=ncdf_vardef(ncid, 'wz', [inelt, inray], /double)
v16=ncdf_vardef(ncid, 'wnpar', [inelt, inray], /double)
v17=ncdf_vardef(ncid, 'wnper', [inelt, inray], /double)
v18=ncdf_vardef(ncid, 'delpwr', [inelt, inray], /double)
v19=ncdf_vardef(ncid, 'fluxn', [inelt, inray], /double)
v20=ncdf_vardef(ncid, 'svgrpdc', [inelt, inray], /double)
v21=ncdf_vardef(ncid, 'sbtot', [inelt, inray], /double)
v22=ncdf_vardef(ncid, 'saspcti', [inelt, inray], /double)
v23=ncdf_vardef(ncid, 'sene', [inelt, inray], /double)
v33=ncdf_vardef(ncid, 'ste',  [inelt, inray], /double)
v24=ncdf_vardef(ncid, 'salphac', [inelt, inray], /double)
v25=ncdf_vardef(ncid, 'salphal', [inelt, inray], /double)
;v26=ncdf_vardef(ncid, 'cnperp', [inelt, inray, itwo], /double)
v27=ncdf_vardef(ncid, 'cwexde', [inelt, inray, itwo], /double)
v28=ncdf_vardef(ncid, 'cweyde', [inelt, inray, itwo], /double)
v29=ncdf_vardef(ncid, 'cwezde', [inelt, inray, itwo], /double)

v29b=ncdf_vardef(ncid, 'xrho', iledge, /double)
v29c=ncdf_vardef(ncid, 'xete', iledge, /double)
v29d=ncdf_vardef(ncid, 'xene', iledge, /double)
v30=ncdf_vardef(ncid, 'xmrho', iledgem1, /double)
v31=ncdf_vardef(ncid, 'weecrh', iledgem1, /double)
v32=ncdf_vardef(ncid, 'currf', iledgem1, /double)

v35=ncdf_vardef(ncid, 'weecrh_i', [iledgem1, insys], /double)
v34=ncdf_vardef(ncid, 'currf_i', [iledgem1, insys], /double)

; enter data entry mode
ncdf_control, ncid, /endef

; now enter the data
ncdf_varput, ncid, v0, nfiles
ncdf_varput, ncid, v4b, t.gyrotron
ncdf_varput, ncid, v4c, power
ncdf_varput, ncid, v1, t.idamp
ncdf_varput, ncid, v2, t.nray
ncdf_varput, ncid, v3, t.nharm
ncdf_varput, ncid, v4, t.freqcy
ncdf_varput, ncid, v5, t.X0
ncdf_varput, ncid, v6, t.Y0
ncdf_varput, ncid, v7, t.Z0
ncdf_varput, ncid, v8, t.angrid1
ncdf_varput, ncid, v9, t.angrid2
ncdf_varput, ncid, v10, t.nrayelt
ncdf_varput, ncid, v11, t.ws
ncdf_varput, ncid, v12, t.spsi
ncdf_varput, ncid, v13, t.wr
ncdf_varput, ncid, v14, t.wphi
ncdf_varput, ncid, v15, t.wz
ncdf_varput, ncid, v16, t.wnpar
ncdf_varput, ncid, v17, t.wnper
ncdf_varput, ncid, v18, t.delpwr
ncdf_varput, ncid, v19, t.fluxn
ncdf_varput, ncid, v20, t.svgrpdc
ncdf_varput, ncid, v21, t.sbtot
ncdf_varput, ncid, v22, t.saspcti
ncdf_varput, ncid, v23, t.sene
ncdf_varput, ncid, v33, t.ste
ncdf_varput, ncid, v24, t.salphac
ncdf_varput, ncid, v25, t.salphal
;ncdf_varput, ncid, v26, t.cnperp
ncdf_varput, ncid, v27, t.cwexde
ncdf_varput, ncid, v28, t.cweyde
ncdf_varput, ncid, v29, t.cwezde
ncdf_varput, ncid, v30, t.xmrho
ncdf_varput, ncid, v31, t.weecrh
ncdf_varput, ncid, v32, t.currf
ncdf_varput, ncid, v28, t.cweyde

ncdf_varput, ncid, v35, t.weecrh_i
ncdf_varput, ncid, v34, t.currf_i

ncdf_varput, ncid, v29b, t.xrho
ncdf_varput, ncid, v29c, t.xete
ncdf_varput, ncid, v29d, t.xene

; close the file
ncdf_close, ncid

return, t

end
	
