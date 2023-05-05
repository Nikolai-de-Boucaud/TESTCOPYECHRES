pro write_toray_input_files, $
	polar_angle, $			; deg
	azimuthal_angle, $		; deg
	gfile, $				; g file
	freq, $ 				; frequency (GHz)
	;
	; kinetic profile knots ( <202 knots)
	edens, $				; electron density (10^13/cm3)
	rho_edens, $
	etemp, $				; electron temperature (keV)
	rho_etemp, $
	zeff, $ 				; zeff
	rho_zeff, $
	;
	;keywords have valid defaults for DIII-D X2 use
	gauszones=gauszones, $	; 4=>30rays
	xfrac=xfrac, $			; x-mode fraction, default = 1.0
	r_launch=r_launch, $	; launch location (m), default=2.399
	z_launch=z_launch, $	; launch z (m), default=0.6794
	divergence=divergence, $; divergence of beam (deg), default=1.7 deg
	ledge=ledge			; radial resolution (201 is default)

if ledge eq !null then ledge=201
if gauszones eq !null then gauszones=4
if r_launch eq !null then r_launch=2.3999
if z_launch eq !null then z_launch=0.67940
if divergence eq !null then divergence=1.7

g=readg(gfile, mode='file')
f=fluxfun_ech(g)

; write the psiin file
; interpolate f arrays from 101 to ledge radial points
rhon=get_range(0.,1.,ledge)	; eg, 201
rhor=get_range(0.,1.,g.mw)	; eg, 129

psir=spline(f.rhonorm, f.psi, rhon)*1.e5	; kG/m^2 !
psin=(psir-psir[0])/(psir[ledge-1]-psir[0])	; 201
psinorm=spline(rhon, psin, rhor) ; normalized psi(rho) 129 elements
ind=interpol(findgen(g.mw), psinorm, rhor)	; indexes for plotting vs psi

bsq_avg=spline(f.rhonorm, f.b2ave, rhor)/g.bcentr^2
bsq_avg_rf=interpolate(bsq_avg, ind)
b_avg=abs(spline(f.rhonorm, f.bave, rhor)/g.bcentr)
b_avg_rf=interpolate(b_avg, ind)
r0rinv=spline(f.rhonorm, f.rm1_ave, rhor)*g.rmaxis
r0rinv_rf=interpolate(r0rinv, ind)
h_factr=spline(f.rhonorm, f.rth_ave, rhor)*g.bcentr
h_factr_rf=interpolate(h_factr, ind)

nr=g.mw
nz=g.mh
rdim=g.xdim
zdim=g.zdim
rcenter=g.rzero
rinside=g.rgrid1
rmaxis=g.rmaxis
zmaxis=g.zmaxis
psimax=g.ssimag
psilim=g.ssibry
b0=g.bcentr
sf=g.fpol
psi=g.psirz
qpsi=g.qpsi
npbdry=g.nbdry
nlimtr=g.limitr
rlim=g.lim[0,*]
zlim=g.lim[1,*]
rpbdry=g.bdry[0, 0:npbdry-1]
zpbdry=g.bdry[1, 0:npbdry-1]
lcentr=2
gafsep=1.e-6

; In case Ip < 0 have to reverse sign of g.fpol and Bt
if g.cpasma lt 0.0 then begin
	sf *= -1.0
	b0 *= -1.0
endif

openw, lun, 'psiin', /get_lun
printf, lun, lcentr, ledge, nr, nz, format='(4i4)'
printf, lun, rdim, zdim, rcenter, rinside, format='(5e16.9)'
printf, lun, rmaxis, zmaxis,psimax,psilim, b0, format='(5e16.9)'
printf, lun, gafsep,format='(e16.9)'
print_arr, lun, sf

print_arr, lun, psi
print_arr, lun, psir
print_arr, lun, qpsi

printf, lun, npbdry, nlimtr, format='(2i5)'

pbdry=fltarr(2*npbdry)
for i=0, npbdry-1 do pbdry[2*i:2*i+1]=[rpbdry[i], zpbdry[i]]
print_arr, lun, pbdry

plim=fltarr(2*nlimtr)
for i=0, nlimtr-1 do plim[2*i:2*i+1]=[rlim[i], zlim[i]]
print_arr, lun, plim

print_arr, lun, h_factr_rf
print_arr, lun, bsq_avg_rf
print_arr, lun, b_avg_rf
print_arr, lun, r0rinv_rf

close, lun
free_lun, lun

; write the echin file
nbfld=3
case gauszones of 
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
idamp=8
xfrac=1.0

if g.cpasma lt 0.0 then bfac=-1.e4 else bfac=1.e4

openw, un, 'echin', /get_lun
printf, un, g.time, format='(e16.9)'
printf, un, idamp, ledge, nray, nbfld, format='(4i4)'
printf, un, freq*1.e9, 1.0-xfrac, $
	r_launch*100., z_launch*100., $
	polar_angle, format='(5e16.9)'
printf, un, azimuthal_angle, divergence, $
	bsratio, g.rzero*100., g.bcentr*bfac, format='(5e16.9)'
printf, un, f.rho[n_elements(f.rho)-1]*100., format='(e16.9)'

; ledge size arrays:
printf, un, psin, format='(5e16.9)'
rhon=get_range(0.,1.,ledge)

zef=spline(rho_zeff, zeff, rhon)
printf, un, zef, format='(5e16.9)'

ene=spline(rho_edens, edens*1.e13, rhon)
i=where(ene gt 0., count)
if count lt ledge then begin
	message, 'echin.ene le 0. at '+strtrim(ledge-count, 2)+' locations.'
	j=where(ene le 0., count)
	for k=0, count-1 do ene[j[k]]=spline(rho[i], ene[i], rho[j[k]])
endif
printf, un, ene, format='(5e16.9)'

ete=spline(rho_etemp, etemp, rhon)
i=where(ete gt 0., count)
if count lt ledge then begin
	message, 'echin.ete le 0. at '+strtrim(ledge-count, 2)+' locations.'
	j=where(ete le 0., count)
	for k=0, count-1 do ete[j[k]]=spline(rho[i], ete[i], rho[j[k]])
endif
printf, un, ete, format='(5e16.9)'

close, un
free_lun, un

; write toray.in file
t=read_nl2('$ECHRES_PATH/toray.in')
t.edata.gauszone=gauszones
write_nl2, t, 'toray.in'

end

pro print_arr, lun, sf
; lun = unit
; sf = values to prinf in format='(5e16.9)'
nn=long(n_elements(sf))
i=0L

if nn ge 5L then $
	for i=0L, nn/long(5)-1 do printf, lun, sf[i*5:(i+1)*5-1], format='(5e16.9)'

if nn-i*5L gt 0 then begin
	case nn-i*5L of
		1: printf, lun, sf[i*5], format='(e16.9)'
		2: printf, lun, sf[i*5:i*5+1], format='(2e16.9)'
		3: printf, lun, sf[i*5:i*5+2], format='(3e16.9)'
		4: printf, lun, sf[i*5:i*5+3], format='(4e16.9)'
	endcase
endif

end

