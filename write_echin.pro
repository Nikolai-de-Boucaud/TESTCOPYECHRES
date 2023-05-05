; success=write_echin, echin_struct, outfile, idamp=idamp, nray=nray, $
;	phai=phai, thet=thet, r_ant=r_ant, z_ant=z_ant, frac_omode=frac_omode		
;
;	echin_struct is structure returned by read_echin(echin_file)
;	phai and thet are azimuthal and polar angles, in deg
;	r_ant and z_ant are the antenna locations in cm
;   frequency is in Hz

function write_echin, e, outfile, frequency=frequency, $
	phai=phai, thet=thet, r_ant=r_ant, z_ant=z_ant, idamp=idamp
; e is structure returned by read_echin()
; returns 1 for success, 0 for failure
if e.success ne 1 then return, 0

if not keyword_set(phai) then phai=e.phai
if not keyword_set(thet) then thet=e.thet
if not keyword_set(idamp) then idamp=e.idamp
if not keyword_set(nray) then nray=e.nray
if keyword_set(r_ant) then X00=r_ant else X00=e.X00
if keyword_set(z_ant) then Z00=z_ant else Z00=e.Z00
if keyword_set(frac_omode) then rfmod=frac_omode else rfmod=e.rfmod
if keyword_set(frequency) then freq=frequency else freq=e.fmu0	; Hz

; do some quality assurance
nrho=n_elements(e.ene)
rho=findgen(nrho)/float(nrho-1.)
i=where(e.ene gt 0., count)
if count lt nrho then begin
	message, 'echin.ene le 0. at '+strtrim(nrho-count, 2)+' locations.'
	j=where(e.ene le 0., count)
	for k=0, count-1 do e.ene[j[k]]=spline(rho[i], e.ene[i], rho[j[k]])
endif
i=where(e.ete gt 0., count)
if count lt nrho then begin
	message, 'echin.ete le 0. at '+strtrim(nrho-count, 2)+' locations.'
	j=where(e.ete le 0., count)
	for k=0, count-1 do e.ete[j[k]]=spline(rho[i], e.ete[i], rho[j[k]])
endif
i=where(e.zef gt 1., count)
if count lt nrho then begin
	message, 'echin.zef le 1. at '+strtrim(nrho-count, 2)+' locations.'
	j=where(e.zef le 1., count)
	for k=0, count-1 do e.zef[j[k]]=spline(rho[i], e.zef[i], rho[j[k]])
endif

; write the file
openw, un, outfile, /get_lun
printf, un, e.time12, format='(e16.9)'
printf, un, idamp, e.j12, nray, e.nbfld, format='(4i4)'
printf, un, freq, rfmod, X00, Z00, thet, format='(5e16.9)' 
printf, un, phai, e.bhalf, e.bsratio, e.rmajs, e.b0, format='(5e16.9)' 
printf, un, e.rmins, format='(e16.9)'
printf, un, e.psinrmr, format='(5e16.9)'
printf, un, e.zef, format='(5e16.9)'
printf, un, e.ene, format='(5e16.9)'
printf, un, e.ete, format='(5e16.9)'

close, un
free_lun, un
return, 1
end
