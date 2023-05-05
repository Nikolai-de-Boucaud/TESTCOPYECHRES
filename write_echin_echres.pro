; write_echin_all
; writes echin file for radial resolution ledge

pro write_echin_echres, p, sys, _EXTRA=extr, dens_mult=dens_mult
; p is psiin-structure
; sys is system number

common ECHCOM

g=gstruct

if sys eq !null then sys=0
if dens_mult eq !null then dens_mult=1.0

outfile='echin'
ledge=201
nbfld=3
case PrgDat.Prefs.gauss_zones of 
	1: nray=1
	2: nray=6
	3: nray=18
	4: nray=30
	5: nray=50
	6: nray=74
	7: nray=100
	else: nray=30
endcase
bsratio=1.0

if g.cpasma lt 0.0 then bfac=-1.e4 else bfac=1.e4

psinorm=(p.psir-p.psir[0])/(p.psir[ledge-1]-p.psir[0])
openw, un, outfile, /get_lun
printf, un, g.time, format='(e16.9)'
printf, un, PrgDat.Prefs.idamp, ledge, nray, nbfld, format='(4i4)'
printf, un, ECHSys.Tank[sys].freq*1.e9, 1.0-ECHSys.Tank[sys].xfrac, $
	ECHSys.Antenna[sys].r_launch*100., ECHSys.Antenna[sys].z_launch*100., $
	ECHSys.InputTable[sys].PolarAng, format='(5e16.9)'
printf, un, ECHSys.InputTable[sys].AziAng, ECHSys.Antenna[sys].divergence, $
	bsratio, g.rzero*100., g.bcentr*bfac, format='(5e16.9)'
printf, un, ffstruct.rho[n_elements(ffstruct.rho)-1]*100., format='(e16.9)'

; ledge size arrays:
printf, un, psinorm, format='(5e16.9)'
rhon=get_range(0.,1.,ledge)

zef=spline(PrgDat.Profiles.rzeffin[0:PrgDat.Profiles.njzef-1], $
	PrgDat.Profiles.zeffin[0:PrgDat.Profiles.njzef-1], rhon)
printf, un, zef, format='(5e16.9)'

ene=spline(PrgDat.Profiles.renein[0:PrgDat.Profiles.njene-1], $
	PrgDat.Profiles.enein[0:PrgDat.Profiles.njene-1], rhon)
i=where(ene gt 0., count)
if count lt ledge then begin
	message, 'echin.ene le 0. at '+strtrim(ledge-count, 2)+' locations.'
	j=where(ene le 0., count)
	for k=0, count-1 do ene[j[k]]=spline(rho[i], ene[i], rho[j[k]])
endif
printf, un, ene*dens_mult, format='(5e16.9)'

ete=spline(PrgDat.Profiles.rtein[0:PrgDat.Profiles.njte-1], $
	PrgDat.Profiles.tein[0:PrgDat.Profiles.njte-1], rhon)
i=where(ete gt 0., count)
if count lt ledge then begin
	message, 'echin.ete le 0. at '+strtrim(ledge-count, 2)+' locations.'
	j=where(ete le 0., count)
	for k=0, count-1 do ete[j[k]]=spline(rho[i], ete[i], rho[j[k]])
endif
printf, un, ete, format='(5e16.9)'

close, un
free_lun, un

file_copy, 'echin', 'echin_sys'+strtrim(sys+1,2), /overwrite

return

end
