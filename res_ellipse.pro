; res_ellipse.pro
; Inputs:
;	frequency, frequency in GHz
; 	Tkev, thermal electron temperature in keV
;	wpar, parallel velocity divided by thermal velocity
;	npar, parallel index of refraction
;	btot, magnetic field in Tesla
; Outputs:
;	wperp, perpendicular velocity divided by thermal velocity
;	vparovtnr, the v_par/v_t corresponding to non relativistic resonance 

function res_ellipse, TkeV, frequency, wpar, nparin, btot, vparovtnr, $
	nharm=nharm, bsign=bsign	; harmonic number
if n_elements(nharm) ne 0 then harm=float(nharm) else $
	harm=float(round(abs(frequency/btot/28.)))
if n_elements(bsign) ne 0 then sgn=-float(bsign) else sgn=1.

npar=nparin*sgn

y2=(frequency/(28.*harm*btot))^2
Tn=TkeV/511.	; nonrel v_t/c
T_norm=(Tn + Tn^2/4.)/(1. + Tn + Tn^2/4.)	; rel v_t/c
Tn5=sqrt(T_norm)
wperp2=(1.-y2)/T_norm + 2.*npar*y2*wpar/Tn5 - (1.+npar^2*y2)*wpar^2
vparovtnr=0.
return, sqrt(wperp2 > 0.)
end
