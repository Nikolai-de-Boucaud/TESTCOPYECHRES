; add gyroname to toray nc file
; Reads toray.nc file and writes a new nc file
; Based on combine_toraync.pro
;; R. Prater, 20091113

function add_gyroname, ncfile, gyroname, outfile=outfile
; ncfile is toray.nc file
; gyroname is string like 'LION'

on_error, 2

if not file_test(ncfile) then return, {filename:''}

if not keyword_set(outfile) then outfile=ncfile

nc0=readnc(ncfile)

; test whether already has gyroname
tgs=tag_names(nc0)
itgs=where(tgs eq 'GYRONAME')
if itgs[0] ge 0 then return, nc0

maxnrayelt=max(nc0.nrayelt)
radresm=fltarr(n_elements(nc0.xmrho))
radres=fltarr(n_elements(nc0.xrho))
nray=nc0.nray
totnray=nray 
arr=dblarr(totnray)
mat=dblarr(maxnrayelt, totnray)

t=create_struct('GYRONAME', '', nc0)

; open netcdf file and enter dimensions
ncid=ncdf_create(outfile, /clobber)
inelt=ncdf_dimdef(ncid, 'neltmax', maxnrayelt)
inray=ncdf_dimdef(ncid, 'nrays', totnray)
itwo=ncdf_dimdef(ncid, 'two', 2)
itwelve=ncdf_dimdef(ncid, 'twelve', 12)
i128=ncdf_dimdef(ncid, 'onetwentyeight', 128)
iledge=ncdf_dimdef(ncid, 'ledge', nc0.ledge)
iledgem1=ncdf_dimdef(ncid, 'ledgem1', nc0.ledge-1)

; define scalar variables
v0=ncdf_vardef(ncid, 'gyrotron', itwelve, /char)
v1=ncdf_vardef(ncid, 'idamp', /long)
v2 = ncdf_vardef(ncid, 'nray', /long)
v3 = ncdf_vardef(ncid, 'nharm', /long)
v4 = ncdf_vardef(ncid, 'freqcy', /double)

; define variables of size nsys
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
v19=ncdf_vardef(ncid, 'curds', [inelt, inray], /double)
v20=ncdf_vardef(ncid, 'arcs', [inelt, inray], /double)
v21=ncdf_vardef(ncid, 'arcds', [inelt, inray], /double)
v22=ncdf_vardef(ncid, 'cnperp', [inelt, inray, itwo], /double)
v23=ncdf_vardef(ncid, 'cwexde', [inelt, inray, itwo], /double)
v24=ncdf_vardef(ncid, 'cweyde', [inelt, inray, itwo], /double)
v25=ncdf_vardef(ncid, 'cwezde', [inelt, inray, itwo], /double)
v26=ncdf_vardef(ncid, 'fluxn', [inelt, inray], /double)
v27=ncdf_vardef(ncid, 'sedenfac', [inelt, inray], /double)
v28=ncdf_vardef(ncid, 'svgrpdc', [inelt, inray], /double)
v29=ncdf_vardef(ncid, 'sbtot', [inelt, inray], /double)
v30=ncdf_vardef(ncid, 'saspcti', [inelt, inray], /double)
v31=ncdf_vardef(ncid, 'sene', [inelt, inray], /double)
v32=ncdf_vardef(ncid, 'ste', [inelt, inray], /double)
v33=ncdf_vardef(ncid, 'salphac', [inelt, inray], /double)
v34=ncdf_vardef(ncid, 'salphal', [inelt, inray], /double)
v35=ncdf_vardef(ncid, 'salpha_vg', [inelt, inray], /double)
v36=ncdf_vardef(ncid, 'ledge', /long)
v37=ncdf_vardef(ncid, 'psimag', /double)
v38=ncdf_vardef(ncid, 'psilim', /double)
v39=ncdf_vardef(ncid, 'raxis', /double)
v40=ncdf_vardef(ncid, 'zaxis', /double)
v41=ncdf_vardef(ncid, 'btaxis', /double)
v42=ncdf_vardef(ncid, 'volume', /double)
v43=ncdf_vardef(ncid, 'xbouni', iledge, /double)
v44=ncdf_vardef(ncid, 'bin_vol', iledgem1, /double)
v45=ncdf_vardef(ncid, 'xrho', iledge, /double)
v46=ncdf_vardef(ncid, 'xene', iledge, /double)
v47=ncdf_vardef(ncid, 'xete', iledge, /double)
v48=ncdf_vardef(ncid, 'xzeff', iledge, /double)
v49=ncdf_vardef(ncid, 'xmrho', iledgem1, /double)
v50=ncdf_vardef(ncid, 'weecrh', iledgem1, /double)
v51=ncdf_vardef(ncid, 'tpowde', iledgem1, /double)
v52=ncdf_vardef(ncid, 'wiecrt', iledgem1, /double)
v53=ncdf_vardef(ncid, 'tidept', iledgem1, /double)
v54=ncdf_vardef(ncid, 'h_factr', iledgem1, /double)
v55=ncdf_vardef(ncid, 'b_avg', iledgem1, /double)
v56=ncdf_vardef(ncid, 'bsq_avg', iledgem1, /double)
v57=ncdf_vardef(ncid, 'currf', iledgem1, /double)
v58=ncdf_vardef(ncid, 'rjpdrho', iledgem1, /double)
v59=ncdf_vardef(ncid, 'power', /double)

; enter data entry mode
ncdf_control, ncid, /endef

; now enter the data
ncdf_varput, ncid, v0, gyroname
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
ncdf_varput, ncid, v19, t.curds
ncdf_varput, ncid, v20, t.arcs
ncdf_varput, ncid, v21, t.arcds
ncdf_varput, ncid, v22, t.cnperp
ncdf_varput, ncid, v23, t.cwexde
ncdf_varput, ncid, v24, t.cweyde
ncdf_varput, ncid, v25, t.cwezde
ncdf_varput, ncid, v26, t.fluxn
ncdf_varput, ncid, v27, t.sedenfac
ncdf_varput, ncid, v28, t.svgrpdc
ncdf_varput, ncid, v29, t.sbtot
ncdf_varput, ncid, v30, t.saspcti
ncdf_varput, ncid, v31, t.sene
ncdf_varput, ncid, v32, t.ste
ncdf_varput, ncid, v33, t.salphac
ncdf_varput, ncid, v34, t.salphal
ncdf_varput, ncid, v35, t.salphal_vg
ncdf_varput, ncid, v36, t.ledge
ncdf_varput, ncid, v37, t.psimag
ncdf_varput, ncid, v38, t.psilim
ncdf_varput, ncid, v39, t.raxis
ncdf_varput, ncid, v40, t.zaxis
ncdf_varput, ncid, v41, t.btaxis
ncdf_varput, ncid, v42, t.volume
ncdf_varput, ncid, v43, t.xbouni
ncdf_varput, ncid, v44, t.bin_vol
ncdf_varput, ncid, v45, t.xrho
ncdf_varput, ncid, v46, t.xene
ncdf_varput, ncid, v47, t.xete
ncdf_varput, ncid, v48, t.xzeff
ncdf_varput, ncid, v49, t.xmrho
ncdf_varput, ncid, v50, t.weecrh
ncdf_varput, ncid, v51, t.tpowde
ncdf_varput, ncid, v52, t.wiecrt
ncdf_varput, ncid, v53, t.tidept
ncdf_varput, ncid, v54, t.h_factr
ncdf_varput, ncid, v55, t.b_avg
ncdf_varput, ncid, v56, t.bsq_avg
ncdf_varput, ncid, v57, t.currf
ncdf_varput, ncid, v58, t.rjpdrho
ncdf_varput, ncid, v59, total(t.delpwr[0,*])/1.d7

; close the file
ncdf_close, ncid
return, t

end
	
