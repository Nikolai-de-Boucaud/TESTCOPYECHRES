; read_echin.pro
; Read echin file and return a structure with ech data for toray

function read_data, unit, num
data=dblarr(num)
a=0.d0 & b=0.d0 & c=0.d0 & d=0.d0 & e=0.d0
j=0
for i=0, num/5-1 do begin
	readf, unit, a, b, c, d, e, format='(5e16.9)'
	data[j:j+4]=[a,b,c,d,e]
	j=j+5
endfor
if num - (num/5)*5 eq 0 then return, data
if num - (num/5)*5 eq 1 then begin
	readf, unit, a, format='(e16.9)'
	data[j]=a
	return, data
endif else begin
	print, '  !!! read_data: problem with data format'
	return, [0]
endelse
end

;***********************************************************
function read_echin, echinfile, infile=infile
; infile usually echin
if n_params() eq 0 then echinfile='echin'
if keyword_set(infile) then echinfile=infile
fh=file_search(echinfile, count=count)
if count ne 1 then begin
	print, '  !!! read_echin: cannot find file '+echinfile+'; aborting.'
	return, {success:0}
endif

thet=0.d0 & phai=0.d0 & x00=0.d0 & z00=0.d0 & time12=0.d0 & fmu0=0.d0
rfmod=0.d0 & bhalf=0.d0 & bsratio=0.d0 & rmajs=0.d0 & b0=0.d0 & rmins=0.d0

openr, un, echinfile, /get_lun
readf, un, time12, format='(e16.9)'
readf, un, idamp, j12, nray, nbfld, format='(4i4)'
j12=fix(j12) & idamp=fix(idamp) & nray=fix(nray) & nbfld=fix(nbfld)
psinrmr=dblarr(j12)
zef=dblarr(j12)
ete=dblarr(j12)
ene=dblarr(j12)
readf, un, fmu0, rfmod, x00, z00, thet, format='(5e16.9)' 
readf, un, phai, bhalf, bsratio, rmajs, b0, format='(5e16.9)' 
readf, un, rmins, format='(e16.9)'
psinrmr=read_data(un, j12)
zef=read_data(un, j12)
ene=read_data(un, j12)
ete=read_data(un, j12)
close, un
free_lun, un
p={success:1, time12:time12, idamp:idamp, j12:j12, nray:nray, $
	nbfld:nbfld, fmu0:fmu0, rfmod:rfmod, x00:x00, z00:z00, thet:thet, $
	phai:phai, bhalf:bhalf, bsratio:bsratio, rmajs:rmajs, b0:b0, rmins:rmins, $
	psinrmr:psinrmr, zef:zef, ene:ene, ete:ete}
return, p
end
