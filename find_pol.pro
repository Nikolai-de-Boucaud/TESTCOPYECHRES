; Procedures to calculate the polarizations for a given gyrotron tank
; connected to a given antenna. Uses data from the DIII-D transmission
; lines for geometry, anisotropy, etc. 

function mat, gam	; gam is angle in radians, returns 2x2 rotation matrix
cosgam=cos(gam) & singam=sin(gam)
return, [ [cosgam, singam], [-singam, cosgam] ]
end

; efields0 returns the complex electric field components ef=[[ex],[ey]] for
; input inclination alpha0 (rad) and ellipticity beta0 (rad)
function efields0, alpha0, beta0
return, [ [complex(cos(alpha0)*cos(beta0),-sin(alpha0)*sin(beta0))], $
	   [complex(sin(alpha0)*cos(beta0), cos(alpha0)*sin(beta0))] ]
end

pro ellipt, ef, alpha, beta
; ef=[complex Ex, complex Ey] is the electric field
; Returns inclination alpha and ellipticity beta parameters in rad
i=complex(0.0, 1.0)
x=ef[0]
y=ef[1]
an=.5*imaginary(alog((x+i*y)*(x-i*y)))
x=x*exp(-i*an)
y=y*exp(-i*an)
alpha=atan(float(y)/float(x))		
if alpha LT -!PI/2. then alpha=alpha+!PI else if alpha GT !PI/2. then alpha=alpha-!PI 
beta=atan(-imaginary(x)/float(y))
end

; function nu returns the angle to add to the steering mirror (rad)
; eps is the tilt (rad) of the steering mirror from vertical (us ~ 72 deg)
; bet is the facet angle (rad) of the steering mirror (us 0 or 19 deg)
; theta is the angle from vertical (rad) of the ray incicident on st mirror
function nu, eps, bet, theta
sigma = !PI/2. - theta 
mu1 = !PI/2. - eps
sintaum = sin(2.*bet)*cos(sigma-mu1)
sinpsip = -sin(sigma)+2.*(cos(bet)^2)*sin(mu1)*cos(sigma-mu1)
cosrhom = sqrt(1. - sintaum^2 - sinpsip^2)
return, asin(sintaum/sqrt(1.-(cos(sigma)*cosrhom + sin(sigma)*sinpsip)^2))
end

;*********************************************************************************
pro find_pol, alpha_0, beta_0, pol1, pol2, co1, co2, mir_type, gammas, delay, nu, $
	alpha_a, beta_a

; This differs from Find_Pol in the additional argument nu(time).
; nu (radians) is an array of angles to be added to the last value of gamma,
; 	useful if the steering mirror is moving.
; All other arguments are assumed static. 
; The return values alpha_a and beta_a are arrays the same size as nu.

; alpha_0 and beta_0 (rad) are the polarization from the gyrotron,
; pol_1 and pol_2 (rad) are the polarizer settings,
; co1 and co2 are the Fourier components of the two grooved mirrors,
; mir_type is an array specifying the mirror type (4=plane, 1=first
; grooved polarizer, 2=second grooved polarizer, 3=anisotropy, 0=dummy,
; gammas (rad) are the angles specifying the transmission line geometry,
; delay (rad) is the phase delay of the anisotropic sections.
;
; alpha_a and beta_a return the inclination and ellipticity (rad)
; of the wave incident on the plasma.

; NOTE: gammas must be the list of gammas from ECHSysData; that is, 
;	lacking any modification of the last entry of 180 deg for the 
;	steering mirror. Changes to the last entry come in through the
;	values of nu.

phi1=pol1
phi2=pol2

nnu=n_elements(nu)
alpha_a=fltarr(nnu)
beta_a=fltarr(nnu)
if nnu eq 1 then multinu=0B else if (max(nu) eq min(nu)) then multinu=0B else multinu=1B

dtr=!PI/180.
i = complex(0.,1.)
inv = [ [1.,0.], [0.,-1.] ]
delaymatrix=complexarr(2,2)

; parameters for grooved mirror
xi1=atan(tan(phi1)/sqrt(2.))
xi2=atan(tan(phi2)/sqrt(2.))

tau1=co1[0] & tau2=co2[0]
for kk=1,5 do begin
	tau1=tau1+2.*co1[kk]*cos(kk*2.*(phi1+!pi/2.))
	tau2=tau2+2.*co2[kk]*cos(kk*2.*(phi2+!pi/2.))
endfor

; transformation matrices for 1st and 2nd mirrors:
a1= cos(tau1/2.)+i*cos(2.*xi1)*sin(tau1/2.)
b1=-cos(tau1/2.)+i*cos(2*xi1)*sin(tau1/2.)
c1=i*sin(2*xi1)*sin(tau1/2.)
a2=cos(tau2/2.)+i*cos(2*xi2)*sin(tau2/2.)
b2=-cos(tau2/2.)+i*cos(2*xi2)*sin(tau2/2.)
c2=i*sin(2*xi2)*sin(tau2/2.)
gm1=[ [a1, -c1], [c1, b1] ]	; matrix for grooved mirror 1
gm2=[ [a2, -c2], [c2, b2] ]	; matrix for grooved mirror 2

nbends = max(where(mir_type NE 0))	; mir_type=0 are dummies

ef0=efields0(alpha_0, beta_0)
ef = ef0

gammasmn=gammas	;in radians

if multinu then goto, multiple_nus
gammasmn[nbends]=gammas[nbends]-nu[0]
; for each bend perform the matrix rotation of polarization:
for kk=0, nbends do begin
	case mir_type[kk] of
		4: mm = inv ## mat(gammasmn[kk])	; plane mirrors
		1: mm = gm1 ## mat(gammasmn[kk])	; first grooved mirror
		2: mm = gm2 ## mat(gammasmn[kk])	; second grooved mirror
		else:
	endcase
	ef = mm ## ef
	;print, ef
	;print, ''	
endfor
ellipt, ef, a_a, b_a
alpha_a[*]=a_a
beta_a[*]=b_a
return

multiple_nus:
for ii=0, nnu-1 do begin
	ef=ef0
	gammasmn[nbends] = gammas[nbends]-nu[ii]
	for kk=0, nbends do begin
		case mir_type[kk] of
			4: mm = inv ## mat(gammasmn[kk])	; plane mirrors
			1: mm = gm1 ## mat(gammasmn[kk])	; first grooved mirror
			2: mm = gm2 ## mat(gammasmn[kk])	; second grooved mirror
			else:
		endcase
		ef = mm ## ef	
	endfor
	ellipt, ef, a_a, b_a
	alpha_a[ii]=a_a
	beta_a[ii]=b_a
endfor
return
end

;*****************************************************************************
; GetPolSettings.pro finds polarizer settings which give a desired polarization
; at the plasma, for a given antenna geometry
; called by echres.pro by
;	GetPolSettings, $ 
;		ECHSys.XLine[jj].alpha0*!dtor, ECHSys.XLine[jj].beta0*!dtor, $
;		ECHSys.XLine[jj].gm1, ECHSys.XLine[jj].gm2, $
;		ECHSys.XLine[jj].mir_type, ECHSys.Xline[jj].gammasd*!dtor, $
;		ECHSys.XLine[jj].delay, alpha_0*!dtor, beta_0*!dtor, $
;		ECHSys.Antenna[jj].Tor_Ang*!dtor, ECHSys.Antenna[jj].Pol_Ang*!dtor, $
;		ECHSys.Antenna[jj].pol_id*!dtor, polvalues

pro GetPolSettings, alpha_0, beta_0, c1, c2, mirror_type, gammaa, delaya, $
	alpha_x, beta_x, facang, tiltang, theta, polvals
	
dtr=!PI/180.
nphi=30	; number of angles in polarizer matrix
polvals=fltarr(6,4) ; default return value, if no solution found

last_mir=max(where(mirror_type NE 0))
if last_mir LT 0 then last_mir=N_elements(mirror_type)

nuu=nu(tiltang, facang, theta)

mir_type=mirror_type[0:last_mir]
gamma=gammaa[0:last_mir]
;gamma[last_mir]=gamma[last_mir]-nu(tiltang, facang, theta)
delay=delaya[0:last_mir]

; set up polarizer angle matrix
phi=(findgen(nphi)*(180./nphi)-88.)*dtr
alpha_a=fltarr(nphi,nphi)	; will fill with applied inclination angles
beta_a=alpha_a				; will get applied ellipticity angles

; starting electric fields ef = [ [ex], [ey] ]
ef0 = efields0(alpha_0, beta_0)

for jj=0,nphi-1 do begin
	for hh=0,nphi-1 do begin
		Find_Pol, alpha_0, beta_0, phi[hh], phi[jj], c1, c2, $
			mir_type, gamma, delay, nuu, a, b
		alpha_a[hh,jj]=a
		beta_a[hh,jj]=b	
	endfor
endfor

; select a and b both within eps of target for initial round:
fpow=0.995		; look for alpha_a and beta_a within this mode purity
da=0.25*dtr		; angle excursion in search

; find how close (alpha_a, beta_a) must be to (alpha_x, beta_x) to
; have adequate mode purity
jinds=0
maxinds=100
getinds:
jinds++
a_a=alpha_x & b_a=beta_x & px=1.0
for i=0, maxinds do begin
	a_a=a_a+da
	px=x_frac(a_a, alpha_x, b_a, beta_x)
	if px lt fpow then goto, got_a
endfor
print, '  !!! GetPolSettings failed to get solution for inclination.'
return
got_a:
epsa=abs(a_a-alpha_x)

a_a=alpha_x & b_a=beta_x & px=1.0
for i=0, maxinds do begin
	b_a=b_a+da
	px=x_frac(a_a, alpha_x, b_a, beta_x)
	if px lt fpow then goto, got_b
endfor
print, '  !!! GetPolSettings failed to get solution for inclination.'
return
got_b:
epsb=abs(b_a-beta_x)

ind=where(((abs(alpha_a-alpha_x) LT epsa) AND (abs(beta_a-beta_x) LT epsb)), count)
if count GT 12 then begin
	fpow=fpow+(1.-fpow)/2.
	if jinds lt maxinds then goto, getinds
endif
if count LT 2 then begin
	fpow=fpow - 0.005
	if jinds lt maxinds then goto, getinds
endif

if jinds ge maxinds-1 then begin
	print, '  !!! GetPolSettings failed to get solution for all polarizer settings'
	return
endif

pols=fltarr(6,12)
for kk=0,count-1 do begin	; find selected elements 
	is = ind[kk] mod nphi
	js = (ind[kk] - is)/nphi
	phi1 = phi[is]
	phi2 = phi[js]
	a_a = alpha_a[is,js]
	b_a = beta_a[is,js]
	xfrac = x_frac(a_a, alpha_x, b_a, beta_x)
	again:			; narrow down to nearest degree
	errx = 0.0
		for mm=-1,1 do begin
			for nn=-1,1 do begin
				phi1p = phi1 + float(mm)*dtr
				phi2p = phi2 + float(nn)*dtr

				Find_Pol, alpha_0, beta_0, phi1p,phi2p,$
					c1, c2, mir_type, gamma, delay, nuu, a_t, b_t
				xf = x_frac(a_t, alpha_x, b_t, beta_x)
				if xf GT xfrac then begin
					xfrac = xf
					phi1=phi1p
					phi2=phi2p
					a_a = a_t
					b_a = b_t
					goto, again
					endif
				errx = errx + (xfrac-xf)^2
			endfor
		endfor
		errx = 100.*sqrt(errx/9.)
	pols[*,kk] = [phi1/dtr,phi2/dtr,100.*xfrac,errx,a_a/dtr,b_a/dtr]
	endfor

if max(pols[2,*]) eq 0 then begin
	res=dialog_message(['find_pol:', 'No polarizer combo found.'])
	return
endif

; sort results on mode purity
in=reverse(sort(pols[2,(where(pols[2,*] GT 0.))]))
pols=pols[*,in]
pols[0,*]=pols[0,*] + (pols[0,*] LT -90.)*180.
pols[0,*]=pols[0,*] - (pols[0,*] GT  90.)*180.
pols[1,*]=pols[1,*] + (pols[1,*] LT -90.)*180.
pols[1,*]=pols[1,*] - (pols[1,*] GT  90.)*180.

; eliminate duplicate elements
polvals=fltarr(6, 4)
polvals[*,0]=pols[*,0]
jk=1
for ii=1, n_elements(in)-1 do $
	if ((abs(pols[0,ii] - pols[0,ii-1]) GT 0.2) OR $
		(abs(pols[1,ii] - pols[1,ii-1]) GT 0.2)) then begin
			polvals[*,jk]=pols[*,ii]
			jk=jk+1
			if jk GT 3 then goto, enough
	endif
enough:
return
end


