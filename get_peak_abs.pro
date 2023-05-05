; get_peak_abs.pro
; inputs: s -> structure from toray.nc, g -> g structure
; keyword: ray is the number of the ray to get the info for
;	central ray has ray number 0
;	if keyword absent, then get the data for the assembly of rays
; 	
; Outputs: structure containing variables at peak of absorption
;
; If keyword arc_len (cm) is set, then the structure returns values at
;	that distance along the ray rather than at the peak, unless arc_len le 0.,
;	in which case arc_len is ignored
;
; changed the saved f_abs from f_abs:1.-delpwr_pk to f_abs=max(s.tpowde)
; to be consistent with the lower table in ECHRES window and the eps file
; by Xi Chen, on Feb 16, 2017

function get_peak_abs, s, g, ray=ray, arc_len=arc_len

forward_function bfield_f

if s.filename eq '' then begin
	print, '  !!! get_peak_abs: toray.nc structure invalid'
	return, {success:0}
endif

if g.error eq 1 then begin
	print, '  !!! get_peak_abs: g structure invalid'
	return, {success:0}
endif

catch, an_error
if an_error ne 0 then begin
	print, '  !!! get_peak_abs: error: ', !err_string
	return, {success:0}
endif

if keyword_set(ray) then rn=ray else rn=0
if rn gt s.nray-1 then return, {success:0}
rpts=where(s.wr[0:s.nrayelt[rn]-1,rn] gt 0.10)
if rpts[0] eq -1 then return, {success:0}
npts=s.nrayelt[rn]
; sometimes problem with last point long ray
; RP 20090430 This seems to be not working right:
;rpts=where(s.wr[*,rn] gt 0.10)
;if n_elements(rpts) gt 2 then rpts=rpts[0:n_elements(rpts)-2]
;npts=s.nrayelt[rn]-2

; variables--use MKS!
r=float(s.wr[rpts,rn])/100.
z=float(s.wz[rpts,rn])/100.
phi=float(s.wphi[rpts,rn])-float(s.wphi[0,rn])
arcl=float(s.arcs[rpts,rn])

npar=float(s.wnpar[rpts,rn])
nperp=float(s.wnper[rpts,rn])
normpow=float(s.delpwr[rpts,rn]/s.delpwr[0,rn])
frac_abs=1.0-normpow[n_elements(normpow)-1]
if frac_abs lt 0.02 then begin
	print, '  !!! get_peak_abs: Absorption lt 2%!'
	return, {success:0}
endif

; get fraction of power in ray rn where ECCD begins
pkj=max(abs(s.curds[*,rn]))
in=where(abs(s.curds[*,rn]) gt 0.00, count)
if ((count gt 0) and (in[0] gt 3)) then $                      
	frac_pow_j=s.delpwr[in[0]-2,rn]/s.delpwr[0,rn] else frac_pow_j=1.0

; if keyword arc_len is set, then evaluate at that location along the ray
if keyword_set(arc_len) then begin
	if arc_len ge max(arcl) then begin
		print, '  !!! get_peak_abs: target arc_len=' + strtrim(arc_len,2) + $
			' but max(arcs)=' + strtrim(max(arcl),2)
		return, {success:0}
	endif
	if arc_len le 0. then goto, halfpower	
	fi=max(where(arcl lt arc_len))
	fid=fi+((arc_len-arcl[fi])/(arcl[fi]-arcl[fi-1]))
endif else begin	; keyword arc_len not set, find arcl at half power
	halfpower:
	dnormpow=-deriv(arcl,normpow)
	; condition dnormpow to keep gaussfit happy
	inds=where(finite(dnormpow) eq 0)
	if inds[0] ne -1 then dnormpow[inds]=0.
	inds=where(abs(dnormpow) lt 1.e-5*max(abs(dnormpow)))
	if inds[0] ne -1 then dnormpow[inds]=0.

	fit=gaussfit(arcl, dnormpow, ga, nterms=3)
	fi=max(where(arcl lt ga[1]))
	if fi[0] lt 1 then begin	; some problem with fit
		print, '  !!! get_peak_abs: problem with gaussian fit; aborting.'
		return, {success:0}
	endif
	fid=fi+((ga[1]-arcl[fi])/(arcl[fi]-arcl[fi-1]))
	; find some quantities at (1/e)^2 power point towards antenna
	fip=get_flind(fit[fi:*], ga[0]*exp(-2))+fi
	fim=get_flind(fit, ga[0]*exp(-2))
	r_oem=interpolate(r, fim)
	z_oem=interpolate(z, fim)
	r_oep=interpolate(r, fip)
	z_oep=interpolate(z, fip)
	rho_oem=rho_rz(g,r_oem,z_oem,psi_oem) & rho_oem=rho_oem[0] & psi_oem=psi_oem[0]
	rho_oep=rho_rz(g,r_oep,z_oep,psi_oep) & rho_oep=rho_oep[0] & psi_oep=psi_oep[0]
endelse

; evaluate other quantities at the maximum of absorption
arcl_pk=interpolate(arcl,fid)
r_pk=interpolate(r,fid)
z_pk=interpolate(z,fid)
phi_pk=interpolate(phi, fid)
delpwr=float(s.delpwr[*,rn])
delpwr_pk=interpolate(delpwr,fid)/delpwr[0]
phi_pk=interpolate(phi,fid)
btot_pk=abs(interpolate(s.sbtot[*,rn], fid)/10000.); what toray thinks is |B|
npar_pk=interpolate(npar,fid)
nperp=float(s.wnper[*,rn])
nperp_pk=interpolate(nperp,fid)
eps=float(s.saspcti[*,rn])
eps_pk=interpolate(eps,fid)
den=float(s.sene[*,rn])
den_pk=interpolate(den,fid)
curds=float(s.curds[*,rn])
curds_pk=interpolate(curds,fid)
te=float(s.ste[*,rn])
te_pk=interpolate(te,fid)
rho_pk=rho_rz(g,r_pk,z_pk,psi_pk) & rho_pk=rho_pk[0] & psi_pk=psi_pk[0]
norm_psi_pk=(g.ssimag-psi_pk)/(g.ssimag-g.ssibry)
deltar=float(r_pk-s.Raxis/100.)
deltaz=float(z_pk-s.Zaxis/100.)
theta_pk_geom=90. - atan( deltar/deltaz )/!dtor
if deltaz lt 0. then if deltar le 0. then theta_pk_geom=theta_pk_geom+180. $
	else theta_pk_geom=theta_pk_geom - 180.
minmaxB, g, r_pk, z_pk, B_min, B_tot, B_max
Bn=abs(B_min) & Bl=abs(B_tot) & Bx=abs(B_max); what bfield_f thinks is |B_tot|
Bl=Bl[0]
if Bl le Bn then theta_pol=0.0 else $
	if Bl ge Bx then theta_pol=180. else $
	if Bx gt Bn then $
	theta_pol=acos((Bn+Bx-2.*Bn*Bx/Bl)/(Bn-Bx))/!dtor $
	else theta_pol=0.
f_ce=27.993763e9*B_tot
freq=float(s.freqcy)
nharm=round(freq/f_ce)
nharm=nharm[0]
yy=f_ce/freq
f_pe=8970.*sqrt(den_pk)
kk=(f_pe/f_ce)^2
n_pk=sqrt(nperp_pk^2 + npar_pk^2)
beta_e=4.0212e-16*den_pk*Te_pk/b_tot^2
beta_e=beta_e[0]
power=float(s.delpwr[0,0])/1.e7  ;delpwr[0,0] is the initial power per ray in erg/s
				 ;"power" here is the initial power per ray in Watt
tnames=tag_names(s)
tnum=where(tnames eq 'XMZEFF')
if tnum[0] ne -1 then $
	zeff_pk=spline(s.xmrho, s.xmzeff, [rho_pk]) else $
	zeff_pk=spline(s.xrho, s.xzeff, [rho_pk])
zeff_pk=zeff_pk[0]
npar_spread, s, npar0, npar_min, npar_max, npar_mean, npar_dev

; get radial information
factor_FWHM=2.*sqrt(2.*alog(2.)) ; factor to convert to FWHM
jeccd=s.wiecrt*2.*!pi*s.raxis/s.volume

maxjeccd=max(abs(jeccd))
;if maxjeccd lt 1. then maxjeccd=1.
radjfit=gaussfit(s.xmrho, jeccd/maxjeccd, coef, nterms=3, chisq=j_chisq)
j0=coef[0]*max(abs(jeccd))
rho_j0=coef[1]
drho_j0=coef[2]*factor_FWHM

radqfit=gaussfit(s.xmrho, s.weecrh/max(abs(s.weecrh)), coef, nterms=3, chisq=q_chisq)
qe0=coef[0]*max(abs(s.weecrh))
rho_qe0=coef[1]
drho_qe0=coef[2]*factor_FWHM

return, {PEAK_ABS, $
	r:r_pk, z:z_pk, phi:phi_pk, rho:rho_pk, thetpol_geom:theta_pk_geom, $
	theta_pol:theta_pol[0], eps:eps_pk, inv_aspect:eps_pk, $
	psi:psi_pk, psi_norm:norm_psi_pk, arc_length:float(arcl_pk), $
	T_e:te_pk, n_e:den_pk, Zeff:Zeff_pk, beta_e:beta_e, Z_eff:Zeff_pk, $
	n_par:npar_pk, n_perp:nperp_pk, B_mag:float(btot_pk), $
	B_min:Bn, B_loc:Bl, B_max:Bx, nharm:nharm, f_abs:max(s.tpowde), $
;	B_min:Bn, B_loc:Bl, B_max:Bx, nharm:nharm, f_abs:1.-delpwr_pk, $
	curds:curds_pk, $
	yy:yy[0], kk:kk[0], frac_abs:frac_abs, $ ; frac_pow_j:frac_pow_j, $
	frequency:freq, ray_power:power, $
	n_par_min:npar_min, n_par_max:npar_max, n_par_mean:npar_mean, $
	n_par_stddev:npar_dev,  $
	r_oem:r_oem, z_oem:z_oem, rho_oem:rho_oem, $
	r_oep:r_oep, z_oep:z_oep, rho_oep:rho_oep, $
	ioverp:float(s.tidept[s.ledge-2]), $	I/P (A/W)
	cd_eff:float(33.e-16*den_pk*s.raxis*s.tidept[s.ledge-2]/te_pk), $
	qe0:float(qe0), rho_qe0:float(rho_qe0), drho_qe0:float(drho_qe0), $
	chisq_q:float(q_chisq), $
	j0:float(j0), rho_j0:float(rho_j0), drho_j0:float(drho_j0), $
	chisq_j:float(j_chisq), $
	ECCD:float(s.tidept[n_elements(s.tidept)-1]), $ ; A/W
	;I_ECCD:float(s.tidept[n_elements(s.tidept)-1])*total(s.delpwr[0,*])/1.e7, $
	gyroname:'', success:1}

	; NB: B_mag is toray version of B, B_loc is bfield_f version of B

end
