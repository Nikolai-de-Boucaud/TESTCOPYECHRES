; test_findpol.pro
; Procedures to calculate the polarizations for a given gyrotron tank
; connected to a given antenna. Uses data from the DIII-D transmission
; lines for geometry, anisotropy, etc. As opposed to findpol, test_findpol
; prints out the inclination and ellipticity after each mitre bend, useful
; for validating polarizations. 

@$ECHRES_PATH/echcal
@$ECHRES_PATH/nu

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

; Input the ellipticity beta (rad) for pure x-mode and for the applied wave,
; and input the tilt alpha (rad) for x-mode and the applied wave,
; returns the fractional power in x-mode.
function x_frac, alpha_a, alpha_x, beta_a, beta_x
return, 1.-(sin(beta_x-beta_a)^2 + sin(alpha_x-alpha_a)^2 * (1.-2.*sin(beta_x)^2) * $
	(1.-2.*sin(beta_a)^2))
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

;*********************************************************************************
pro Find_Pol, alpha_0, beta_0, phi1, phi2, co1, co2, mir_type, gammas, delay, $
	alpha_a, beta_a
; alpha_0 and beta_0 (rad) are the polarization from the gyrotron,
; phi_1 and phi_2 (rad) are the polarizer settings,
; co1 and co2 are the Fourier components of the two grooved mirrors,
; mir_type is an array specifying the mirror type (4=plane, 1=first
; grooved polarizer, 2=second grooved polarizer, 3=anisotropy, 0=dummy,
; gammas (rad) are the angles specifying the transmission line geometry,
; delay (rad) is the phase delay of the anisotropic sections.
;
; alpha_a and beta_a return the inclination and ellipticity (rad)
; of the wave incident on the plasma.

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
ellipt, ef0, a, b
print, a/!dtor, b/!dtor
ef = ef0
; for each bend perform the matrix rotation of polarization:
for kk=0, nbends do begin
	case mir_type[kk] of
		4: mm = inv ## mat(gammas[kk])	; plane mirrors
		1: mm = gm1 ## mat(gammas[kk])	; first grooved mirror
		2: mm = gm2 ## mat(gammas[kk])	; second grooved mirror
		3: begin						; anisotropy correction
			delaymatrix=[[exp(i*delay[kk]), 0.0], [0.0, 1.0]]
			mm = mat(-gammas[kk]) ## (delaymatrix ## mat(gammas[kk]))
			end
		else: goto, nextmir
		endcase
	ef = mm ## ef
	if kk eq (n_elements(gammas)-3) then blab='  IMB' else blab=''
	print, kk+1, gammas[kk]/!dtor, mir_type[kk], blab
	ellipt, ef, alpha_a, beta_a
	print, alpha_a/!dtor, beta_a/!dtor
	nextmir:
	endfor
ellipt, ef, alpha_a, beta_a
end

;************************************** main ************************
gyroname=''
shot=1000000L

beginning:
;read, 'Enter shot number: ', shot
read, 'Enter gyrotron name (no quotes): ', gyroname

z=echcal(shot, gyroname)

;read, 'Enter poloidal(tilt) and toroidal(facet) angles: ', tilt, facet
tilt=55.	; dummy values needed for last miter bend
facet=0.

read, 'Enter polarizer pol1 and pol2 settings (deg): ', pol1, pol2
phi1=pol1*!dtor & phi2=pol2*!dtor

gammasd=z.gamma
gammasd[n_elements(gammasd)-1] -= nu(tilt*!dtor, facet*!dtor, $
	z.antenna_data.pol_id*!dtor)/!dtor

Find_Pol, z.gyro.polinc*!dtor, z.gyro.polell*!dtor, $
	phi1, phi2, z.gm1, z.gm2, z.mir_type, gammasd*!dtor, $
	z.delay*!dtor, alpha_a, beta_a

end	
