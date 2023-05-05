pro print_arr, lun, sf
; lun = unit
; sf = values to prinf in format='(5e16.9)'
nn=n_elements(sf)
if nn ge 5 then $
	for i=0, nn/5-1 do printf, lun, sf[i*5:(i+1)*5-1], format='(5e16.9)'

lo=nn-i*5
if lo gt 0 then begin
	case lo of
		1: printf, lun, sf[i*5], format='(e16.9)'
		2: printf, lun, sf[i*5:i*5+1], format='(2e16.9)'
		3: printf, lun, sf[i*5:i*5+2], format='(3e16.9)'
		4: printf, lun, sf[i*5:i*5+3], format='(4e16.9)'
	endcase
endif

end

; write_psiin.pro
function write_psiin, gfile, outfile, ledge=ledge, gafsep=gafsep, lcentr=lcentr
if n_params() eq 1 then outfile='psiin'
if not keyword_set(ledge) then ledge=201
if not keyword_set(gafsep) then gafsep=1.e-6
if not keyword_set(lcentr) then lcentr=2

fh=file_search(gfile, count=count)
if count ne 1 then return, {success:0}
g=readg(fh[0])
if g.error then return, {success:0}

; interpolate f arrays from 101 to ledge radial points
rhon=get_range(0.,1.,ledge)	; eg, 201
rhor=get_range(0.,1.,g.mw)	; eg, 129
f=fluxfun_ech(g)				; eg, 101

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

openw, lun, outfile, /get_lun
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

return, {success:1}
end


pro write_psiin_file, p, outfile
; writes the psiin file from a structure
; p is structure from read_psiin()

openw, lun, outfile, /get_lun
printf, lun, p.lcentr, p.ledge, p.nr, p.nz, format='(4i4)'
printf, lun, p.rdim, p.zdim, p.rcenter, p.rinside, format='(5e16.9)'
printf, lun, p.rmaxis, p.zmaxis, p.psimax, p.psilim, p.b0, format='(5e16.9)'
printf, lun, p.gafsep,format='(e16.9)'
print_arr, lun, p.sf
print_arr, lun, p.psi
print_arr, lun, p.psir
print_arr, lun, p.qpsi
printf, lun, p.npbdry, p.nlimtr, format='(2i5)'

pbdry=fltarr(2*p.npbdry)
for i=0, p.npbdry-1 do pbdry[2*i:2*i+1]=[p.rpbdry[i], p.zpbdry[i]]
print_arr, lun, pbdry

plim=fltarr(2*p.nlimtr)
for i=0, p.nlimtr-1 do plim[2*i:2*i+1]=[p.rlim[i], p.zlim[i]]
print_arr, lun, plim

print_arr, lun, p.h_factr_rf
print_arr, lun, p.bsq_avg_rf
print_arr, lun, p.b_avg_rf
print_arr, lun, p.r0rinv_rf

done:
close, lun
free_lun, lun

return
end
