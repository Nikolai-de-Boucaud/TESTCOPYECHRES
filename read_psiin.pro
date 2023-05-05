;read_psiin.pro

function read_arr, lun, nn
; lun = unit
; nn = number of values to read in format='(5e16.9)'
sf=fltarr(nn)
a=0.0 & b=0.0 & c=0.0 & d=0.0 & e=0.0
if nn ge 5 then begin
	for i=0L, nn/5L-1 do begin
		readf, lun, a,b,c,d,e, format='(5e16.9)'
		sf[i*5L:(i+1)*5L-1]=[a,b,c,d,e]
	endfor
	lo=nn-i*5L
endif else lo=nn

if lo gt 0 then begin
	case lo of
		1: begin
			readf, lun, a, format='(e16.9)'
			sf[i*5L]=a
		end
		2: begin
			readf, lun, a, b, format='(2e16.9)'
			sf[i*5L:i*5L+1]=[a,b]
		end
		3: begin
			readf, lun, a, b, c, format='(3e16.9)'
			sf[i*5L:i*5L+2]=[a,b,c]
		end
		4: begin
			readf, lun, a, b, c, d, format='(4e16.9)'
			sf[i*5L:i*5L+3]=[a,b,c,d]
		end
	endcase
endif
return, sf
end

function read_psiin, infile
nargs=n_params()
if nargs eq 0 then infile='psiin'
fh=file_search(infile, count=count)
if count ne 1 then return, {success:0}

openr, lun, fh[0], /get_lun

lcentr=0 & ledge=0 & nr=0L & nz=0L
readf, lun, lcentr, ledge, nr, nz
readf, lun, rdim, zdim, rcenter, rinside
readf, lun, rmaxis, zmaxis, psimax, psilim, b0, format='(5e16.9)'
readf, lun, gafsep
sf=read_arr(lun, nr)

psi=fltarr(nr, nz)
temp=read_arr(lun, long(nr*nz))
for j=0, nz-1 do psi[*,j]=temp[j*nr:(j+1)*nr-1]
psir=read_arr(lun, ledge)
qpsi=read_arr(lun, nr)
rpbdry=0 & nlimtr=0
readf, lun, npbdry, nlimtr
rpbdry=fltarr(npbdry) & zpbdry=rpbdry
temp=read_arr(lun, 2*npbdry)
rpbdry=temp[indgen(npbdry)*2]
zpbdry=temp[indgen(npbdry)*2+1]
rlim=fltarr(nlimtr) & zlim=fltarr(nlimtr)
temp=read_arr(lun, 2*nlimtr)
rlim=temp[indgen(nlimtr)*2]
zlim=temp[indgen(nlimtr)*2+1]

if not EOF(lun) then begin
	h_factr_rf=read_arr(lun, nr)
	bsq_avg_rf=read_arr(lun, nr)
	b_avg_rf=read_arr(lun, nr)
	r0rinv_rf=read_arr(lun, nr)
endif else begin
	h_factr_rf=fltarr(nr)
	bsq_avg_rf=fltarr(nr)
	b_avg_rf=fltarr(nr)
	r0rinv_rf=fltarr(nr)
endelse

close, lun
free_lun, lun
return, {success:1, $
	lcentr:lcentr, ledge:ledge, nr:nr, nz:nz, $
	rdim:rdim, zdim:zdim, rcenter:rcenter, rinside:rinside, $
	rmaxis:rmaxis, zmaxis:zmaxis, psimax:psimax, psilim:psilim, b0:b0, $
	gafsep:gafsep, $
	sf:sf, $
	psi:psi, $
	psir:psir, $
	qpsi:qpsi, $
	npbdry:fix(npbdry), nlimtr:fix(nlimtr), $
	rlim:rlim, zlim:zlim, $
	rpbdry:rpbdry, zpbdry:zpbdry, $
	h_factr_rf:h_factr_rf, $
	bsq_avg_rf:bsq_avg_rf, $
	b_avg_rf:b_avg_rf, $
	r0rinv_rf:r0rinv_rf $
	}
end
