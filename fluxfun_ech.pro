;***********************************************************************
;
;  4dlib/EFITLIB/kupfer/idl_geqdsk/fluxfun.pro
;
;  created:
;
;  modified:
;  990302  Qian Peng - updates for efit
;  970623  Ted Terpstra - putin check for 0 pts.
;
;	Modified 20050622 by RP to add flux surface average quantities
;   <R>, <R^2>, <1/R>, <1/R^2> ; and normalized rho rhonorm
;   Also added quantities <h^2> and <sqrt(1-h)> for linliu GACD,
;	where h=B/B_max
;***********************************************************************
;
; -- (1) Tabulates the radial flux coordinate "rho" at specified values
;    of the poloidal flux function "psi". 
; -- (2) Computes the B-field components on a set of 30 flux surfaces,
;     as reconstructed from GEQDSK.
; -- (3) Computes the flux surface averages of various quantities, 
;    such as J_parallel, B, Volume, etc. (see output list below).

; -- INPUT --
; -- g = structure obtained from readg containing the GEQDSK.
; -- do_spline -- a keyword arguement; if present and non-zero,
;       then a bi-cubic spline is used to increase the size of 
;       the (R,Z) mesh before finding the psi contours.
;       Note, the bi-cubic spline is time consuming. 

; -- OUTPUT --
; -- output = data structure containing the following fields:
; -- shot = shot number (g.shot)
; -- time = time in msec (g.time)
; -- bcentr = abs(g.bcentr) in Tesla. This is the B-field 
;             normalization used to define rho (below).
; -- rho = values of rho IN METERS at 101 equally spaced 
;          grid points, from the magnetic axis to the boundary.
;          Note rho = sqrt[(Toroidal Flux)/(pi*bcentr)], where
;          bcentr is the toroidal field on axis -- from the 
;          GEQDSK -- we take bcentr = abs(g.bcentr).
; -- psi = corresponding values of poloidal flux function (in MKS units).
;       psi = g.ssimag at the magnetic axis,
;       psi = g.ssibry at the boundary (last closed flux surface).
;          ALL FLUX FUNCTIONS ARE TABULATED
;          ON THIS GRID OF 101 RHO/PSI VALUES.
; -- q = the MHD safety factor, i.e. the poloidal average of 
;        the local safety factor.
; -- jll = <J_parallel*B>/bcentr in (A/m^2), where J_parallel 
;          is the parallel current density and <*> refers to
;          the usual flux surface average.
; -- iphi = total toroidal current (Amps) passing through 
;           cross-sectional area of the flux surfaces, as
;           calculated by method 1 (see comments below).
; -- iphi_2 = iphi (Amps) calculated by method 2 (see comments below).
; -- jeff = effective toroidal current density (A/m^2), defined in
;           therms of d(iphi)/d(psi) (see comments below).
; -- bave = flux surface average of B (Tesla).
; -- b2ave = flux surface average of B^2.
; -- bphi2 = flux surface average of (B_phi)^2, where B_phi is the
;            toroidal field in Tesla.
; -- vol = total volume (m^3) enclosed by flux surfaces.
; -- dvol = d(vol)/d(rho) at all flux surfaces (in m^2).
; -- f = the flux function (R*B_phi). (MKS units).
; -- p = plasma pressure flux function (MKS units).
; -- fprime = d(f)/d(psi). (MKS units).
; -- pprime = the flux function d(Pressure)/d(psi). (MKS units).
; -- fc = fraction of circulating particles (flux function).
; -- grad_term = <(parallel gradient of |B|)^2> in (Tesla/meter)^2.
; -- rho_x = values of rho (in meters) corresponding to the location
;            of 30 reconstructed flux surfaces described below.
; -- theta = equally spaced grid in poloidal angle (defined as the
;        usual polar coordinate with respect to the magnetic axis).
;        Note, 0 <= theta(i) < 2*pi (including 0 but not 2*pi).
; -- r = local minor radius (meters) evaluated on the flux surfaces:
;        r(i,j) corresponds to location theta(i) and rho_x(j).
; -- b = B-field (in Tesla) evaluated on the flux surfaces: 
;        if the components of B = (B_phi, B_theta, B_rad), then
;        b(i,j,0) = B_phi, b(i,j,1) = B_theta, b(i,j,2) = B_rad,
;        corresponding to location theta(i) and rho_x(j).
;
;**********************************************************************

function fluxfun_ech, g, pts, $
                  do_spline = do_spline
;nar=n_params()
if n_params() eq 1 then pts=40
;
; -- calculate the map between rho and psi -- 
;

If (g.mw Lt 2) Then Begin
  Print, 'Fluxfun: Error. Input g.mw NOT gt 1. Return. ', g.mw
  Return, {ierr:1}
EndIf

map = fluxmap(g)
psi = map.psi
rho = map.rho
bcentr = map.bcentr

;
; Note flux_toroidal = !pi*bcentr*rho^2
;
; -- normalize psi --
; We work with normalized psi, where 
; psi=0 (at g.ssimag) and psi=1 (at g.ssibdry).
;

dpsi = (g.ssibry-g.ssimag) 
psi = (psi - g.ssimag)/dpsi

;
; -- reconstruct a set of flux surfaces --
;

psi1 = 0.02          ; first psi-contour (normalized).
psi2 = 0.99          ; last psi-contour (normalized).
;pts = 80             ; number of theta points.

;
; -- set the (R,Z) mesh size for psi table --
; nx = number of R-points (g.mw is minimum).
; ny = number of Z-points (g.mh is minimum).
;

if keyword_set(do_spline) then begin 
  nx = 2*g.mw
  ny = 2*g.mh
endif else begin
  nx = g.mw
  ny = g.mh
endelse 

contour_psi, g,pts,psi1,psi2,nx,ny,psi_x,theta,r,b
IF N_Elements(b) EQ 0 THEN BEGIN
   Return,{ierr:1}
ENDIF

rho_x = spline(psi,rho,psi_x)

;
; Here psi_x (or rho_x) gives the value of psi (or rho) on each
; of the reconstructed flux surfaces.
;
; -- components of B-field at location (i,j) on flux surfaces --
;

b_phi = b(*,*,0)
b_theta = b(*,*,1)
b_rad = b(*,*,2)
btot = sqrt(b(*,*,0)^2 + b(*,*,1)^2 + b(*,*,2)^2)

;
; -- jac(i,j) = local Jacobian in (phi,theta,psi) coordinates at
;               location (i,j) on flux surfaces.
; -- jacsum(j) = sum elements of jac(*,j) at flux surface j.
; -- jac0 = theta average of jac, interpolated onto rho/psi grid.
;

jac = r/b_theta
jacsum = total(jac,1)
jac0 = spline(psi_x,jacsum/float(pts),psi)

;
; -- calculate flux surface averages --
;

w = btot

;
; Here w(i,j) is the function to be flux surface averaged
; and w_ave = <w> interpolated onto psi/rho grid.
;

temp = total(w*jac,1)/jacsum
w_ave = spline(psi_x,temp,psi)
bave = w_ave ; flux surface average of B.

w = btot^2
temp = total(w*jac,1)/jacsum
w_ave = spline(psi_x,temp,psi)
b2ave = w_ave ; flux surface average of B^2.

w = b_phi^2  
temp = total(w*jac,1)/jacsum
w_ave = spline(psi_x,temp,psi)
bphi2 = w_ave ; flux surface average of (B_phi)^2

; ********** This section added by RP ************
na=n_elements(theta)		; number points on a surface (40)
nb=n_elements(b[0,*,0])		; number of surfaces (30)
th=fltarr(na, nb)
for i=0, nb-1 do th[*,i]=theta

pol_length=fltarr(nb)
for j=1, nb-1 do begin
	for i=1, n_elements(theta)-1 do $
		pol_length[j] += sqrt(r[i,j]^2 + r[i-1,j]^2 - 2.*r[i,j]*r[i-1,j]*cos(theta[i]-theta[i-1]))
	pol_length[j] += sqrt(r[0,j]^2 + r[na-1,j]^2 - 2.*r[0,j]*r[na-1,j]*cos(theta[0]-theta[nb-1]))
endfor

w = b_phi
temp = total(w*jac,1)/jacsum
w_ave = spline(psi_x,temp,psi)
bphi = w_ave ; flux surface average of R (major radius in m)


w = g.rmaxis + r * cos(th)
temp = total(w*jac,1)/jacsum
w_ave = spline(psi_x,temp,psi)
r_ave = w_ave ; flux surface average of R (major radius in m)

w = (g.rmaxis + r * cos(th))^2
temp = total(w*jac,1)/jacsum
w_ave = spline(psi_x,temp,psi)
r2_ave = w_ave ; flux surface average of R^2 (major radius in m^2)

w = 1. / (g.rmaxis + r * cos(th))
temp = total(w*jac,1)/jacsum
w_ave = spline(psi_x,temp,psi)
rm1_ave = w_ave ; flux surface average of 1/R (major radius in m)

w = 1. / (g.rmaxis + r * cos(th))^2
temp = total(w*jac,1)/jacsum
w_ave = spline(psi_x,temp,psi)
rm2_ave = w_ave ; flux surface average of 1/R^2 (major radius in m^2)

b_pol=sqrt(b_rad^2 + b_theta^2)
w = b_pol  
temp = total(w*jac,1)/jacsum
w_ave = spline(psi_x,temp,psi)
bpave = w_ave ; flux surface average of (B_pol)^2

h=fltarr(na, nb)	; h = B/B_max on each flux surface
hfac=fltarr(na,nb)	; hfac=sqrt(1-h)
for i=0, nb-1 do begin
	bmax=max(btot[*,i])
	h[*,i]=btot[*,i]/bmax
	hfac[*,i]=sqrt(1. - btot[*,i]/bmax)
endfor

w = h^2
temp = total(w*jac,1)/jacsum
w_ave = spline(psi_x,temp,psi)
h2_ave = w_ave ; flux surface average of h^2

w=hfac
temp = total(w*jac,1)/jacsum
rth_ave = spline(psi_x,temp,psi)

w = b_phi/(g.rmaxis + r * cos(th))
temp = total(w*jac,1)/jacsum
w_ave = spline(psi_x,temp,psi)
bphior = w_ave

;***********************
;
;
; -- calculate jll -- 
; Here we calculate jll from the following expression
; [J (dot) B] = fprime*B^2/mu_o + pprime*f,
; which follows from the Grad Shafranov formulation, where
; f(psi) = R*B_phi, p(psi) = plasma pressure,
; fprime = d(f)/d(psi) and pprime = d(p)/d(psi).
;
; Note, it is assumed that the GEQDSK evaluates flux functions
; (such as f and p) on a grid of NUMPTS equally spaced points in psi
; from g.ssimag to g.ssibry.
;

numpts = g.mw
num1 = numpts-1
psi_eqdsk = findgen(numpts)/float(num1)
f_eqdsk = g.fpol(0:num1)
p_eqdsk = g.pres(0:num1)
fprime_eqdsk = g.ffprim(0:num1)/g.fpol(0:num1)
pprime_eqdsk = g.pprime(0:num1)
f = spline(psi_eqdsk,f_eqdsk,psi)
p = spline(psi_eqdsk,p_eqdsk,psi)
fprime = spline(psi_eqdsk,fprime_eqdsk,psi)
pprime = spline(psi_eqdsk,pprime_eqdsk,psi)
mu_o = 4.*!pi*1e-7

jll = (fprime*b2ave/mu_o + pprime*f)/bcentr

;
; -- calculate iphi (method 1) --
; Here we calculate iphi from jphi (current density in phi 
; direction) as given by the RHS of the Grad Shafranov equation,
; jphi*R = pprime*R^2 + f*fprime/mu_o.
;
; Note, diphi_dpsi = d(iphi)/d(psi), as follows:
;

diphi_dpsi = -2.*!pi*jac0*(pprime + fprime*bphi2/f/mu_o)
iphi = qsimpv(psi,diphi_dpsi)*dpsi

;
; -- calculate iphi (method 2) --
; Here we calculate iphi from the line integral of B around
; one poloidal circuit. 
;

bpol2 = b_theta^2 + b_rad^2
iphi_x = total(jac*bpol2,1)*5e6/float(pts)
iphi_2 = spline([0.,psi_x,1.],[0.,iphi_x,g.cpasma],psi)

;
; -- safety factor (q) on rho/psi grid --
;
; determine q on last re-constructed flux surface --
;

j = n_elements(psi_x)-1
qlocal = b_phi(*,j)*r(*,j)/b_theta(*,j)/(g.rmaxis + r(*,j)*cos(theta))
qbar = total(qlocal)/float(pts)

;
; for inner points use values of q from GEQDSK -- 
;

q_eqdsk = g.qpsi(0:num1)
index = indgen(num1)
if psi_x(j) gt psi_eqdsk(num1-1) then $
  q = spline([psi_eqdsk(index),psi_x(j)],[q_eqdsk(index),qbar],psi) $
else $
  q = spline(psi_eqdsk(index),q_eqdsk(index),psi)

;
; -- calculate jeff (defined as follows) --
;  jeff*(2*pi*rho) = d(iphi)/d(rho) 
;

jeff = diphi_dpsi*bcentr/(2.*!pi*q)

;
; -- calculate dvol and vol --
;

dpsi_drho = bcentr*rho/q    ; d(psi)/d(rho)
dvol = 4.*!pi^2*dpsi_drho*jac0
vol = qsimpv(rho,dvol)

;
; -- calculate the fraction of circulating particles -- 
;
; make calculation using re-constructed flux surfaces:
;

fc_x = fraction_circ(jac,btot)

;
; Spline fit to rho grid, setting the appropriate limiting 
; form as rho goes to zero:
;

coef = (1. - fc_x(0))/sqrt(rho_x(0))
rho_0 = rho_x(0)*findgen(3)/3.
fc_0 = 1. - coef*sqrt(rho_0)
fc = spline([rho_0, rho_x],[fc_0, fc_x],rho)

;
; -- calculate the parallel gradient of B --
; Here grad_parallel = parallel gradient of |B|.
;

num = n_elements(psi_x)
deriv_theta  = fltarr(pts,num)
for j=0,(num-1) do $
  deriv_theta(*,j) = deriv(theta,btot(*,j))
grad_parallel = deriv_theta/jac/btot

;
; -- form grad_term = <grad_parallel^2> --
;

grad_term_x = total(jac*grad_parallel^2,1)/jacsum

;
; Spline fit to rho grid, setting the appropriate limiting 
; form as rho goes to zero:
;

coef = grad_term_x(0)/rho_x(0)^2
rho_0 = rho_x(0)*findgen(3)/3.
grad_term_0 = coef*rho_0^2
grad_term = spline([rho_0, rho_x],[grad_term_0,grad_term_x],rho)

;
; -- output --
;

psi = g.ssimag + dpsi*psi

;
; This re-scales psi to un-normalized value.
;

dvdpsi=dvol/dpsi_drho
fcql=4.*!pi^2 * q/ (rm1_ave * dvdpsi); mult by j_midplane/B_midplane to get <j>

output = {shot:g.shot, time:g.time, $
     bcentr:bcentr, psi:psi, rho:rho, rhonorm:rho/rho[n_elements(rho)-1], $
	 q:q, jll:jll, iphi:iphi, $
     jeff:jeff, iphi_2:iphi_2, bave:bave, b2ave:b2ave, bphi2:bphi2, $
     vol:vol, dvol:dvol, f:f, p:p, fprime:fprime, pprime:pprime, $
     fc:fc, grad_term:grad_term, bphi:bphi, bphior:bphior, fcql:fcql, $
	 r_ave:r_ave, r2_ave:r2_ave, rm1_ave:rm1_ave, rm2_ave:rm2_ave, $
	 h2_ave:h2_ave, rth_ave:rth_ave, pol_length:pol_length, bpave:bpave, $
     rho_x:fltarr(30), theta:theta, r:fltarr(pts,30), b:fltarr(pts,30,3)}

n = n_elements(rho_x)
j = indgen(n)
output.rho_x(j) = rho_x
output.r(*,j) = r
output.b(*,j,*) = b

return, output
end
