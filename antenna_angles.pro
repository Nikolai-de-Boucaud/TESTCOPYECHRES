; antenna_angles.pro
; functions which translate from angles to counts
; for the P1999, P2001, P2002, GA1, and GA2 antennas
; "Tilt angle" is the tilt of the cradle; 0 is upward, positive in inward
; "Facet angle" is the angle of the mirror facet from radial; radial is
; zero, leftward is positive (co-ECCD)
; For all cases, positive tilt or crank angle corresponds to co-CD (left)
; antenna_number= 0(GA1_M1), 1(GA1_M2), 2(P1999_M1), 3(P1999_M2),
;	4(GA2_M1), 5(GA2_M2), 6(P2001_M1), 7(P2001_M2), 8 (P2002_M1), 9(P2002_M2),
;	10(P2006_M1), and 11(P2006_M2)
; For the P200* launcher, tilt <-> scan and facet <-> crank


function cubic_soln, coef, x
; solves coef[3]*x^3 + coef[2]*x^2 + coef[1]*x + coef[0] = 0 for any real coef
if abs(coef[3]) lt 1.e-30 and abs(coef[2]) lt 1.e-30 then goto, linear
if abs(coef[3]) lt 1.e-30 then goto, quadratic

a=coef[2]/coef[3]
b=coef[1]/coef[3]
c=(coef[0]-x)/coef[3]
Q=(a^2-3.*b)/9.
R=(2.*a^3 - 9.*a*b + 27.*c)/54.
if (R^2 lt Q^3) then begin	; three real roots
	Qsrt=sqrt(Q)
	th=acos(R/Qsrt^3)
	y1=-2.*Qsrt*cos(th/3.) - a/3.
	y2=-2.*Qsrt*cos((th+2.*!pi)/3.) - a/3.
	y3=-2.*Qsrt*cos((th-2.*!pi)/3.) - a/3.
	y=[y1, y2, y3]
	ind=where( (y gt 2500) and (y lt 16000))	; special stuff to pick right root
	if ind[0] lt 0 then return, -1.
	if n_elements(ind) eq 1 then return, y[ind] else return, -1.
endif else begin	; one real root
	if R lt 0 then sgn=-1. else sgn=1.
	AC=-sgn*(abs(R)+sqrt(R^2-Q^3))^(1./3.)
	if AC ne 0. then BC=Q/AC else BC=0.
	return, AC + BC - a/3.
endelse

quadratic:
; soln to coef[2]*counts^2 + coef[1]*counts + (coef[0]-x) = 0

surd=coef[1]^2 - 4.*(coef[0]-x)*coef[2]
if surd lt 0. then return, 0. else return,(-coef[1]+sqrt(surd))/(2.*coef[2])

linear: if coef[1] gt 1.e-30 then return, (x-coef[0])/coef[1] else return, 0.
	
end

function polynom, coefficients, z
; returns coef[0] + coef[1]*x + coef[2]*x^2 + ...
coef=float(coefficients)
x=float(z)
y=0.0
for i=0, n_elements(coef)-1 do y=y + coef[i]*x^i
return, y
end

;****************** Antenna Angles *************************************
; Returns the antenna angles of mirror normal
pro Antenna_Angles, shot, antenna_id, $	; antenna number=6,7,...
	 scan_angle, crank_angle, $		; outputs
	 scan_cts, crank_cts, $		; inputs
	 antenna_num=antenna_num
	 
; if antenna_num is set, then antenna_id is a number 6,7,8,...
; if antenna_num is not set, then antenna_id is 'P2001_M1'...

if not keyword_set(antenna_num) then begin
	case antenna_id of
		'P2001_M1': antenna_number=6
		'P2001_M2': antenna_number=7
		'P2002_M1': antenna_number=8
		'P2002_M2': antenna_number=9
		'P2006_M1': antenna_number=10
		'P2006_M2': antenna_number=11
		'P2012_M1': antenna_number=12
		'P2012_M2': antenna_number=13
		else: antenna_number=-1
	endcase
endif else antenna_number=antenna_id

;if n_elements(scan_coefs) eq 0 and n_elements(crank_coefs) eq 0 then begin
	z=echcal(shot, antenna_number, id_type='antenna')
	scan_angle=polynom(z.scan_coef, scan_cts)
	crank_angle=polynom(z.crank_coef, crank_cts)
;endif else begin
;	scan_angle=polynom(scan_coefs, scan_cts)
;	crank_angle=polynom(crank_coefs, crank_cts)
;endelse

end

pro Antenna_Counts, shot, antenna_id, $
	 scan_angle, crank_angle, $		; inputs (deg)
	 scan_cts, crank_cts, $		; outputs
	 antenna_num=antenna_num
	 
; if antenna_num is set, then antenna_id is a number 6,7,8,...
; if antenna_num is not set, then antenna_id is 'P2001_M1'...
	 
if not keyword_set(antenna_num) then begin
	case antenna_id of
		'P2001_M1': antenna_number=6
		'P2001_M2': antenna_number=7
		'P2002_M1': antenna_number=8
		'P2002_M2': antenna_number=9
		'P2006_M1': antenna_number=10
		'P2006_M2': antenna_number=11
		'P2012_M1': antenna_number=12
		'P2012_M2': antenna_number=13
		else: antenna_number=-1
	endcase
endif else antenna_number=antenna_id

;if n_elements(scan_coefs) eq 0 and n_elements(crank_coefs) eq 0 then begin
	z=echcal(shot, antenna_number, id_type='antenna') 
	scan_cts=round(cubic_soln(z.scan_coef, scan_angle))
	crank_cts=round(cubic_soln(z.crank_coef, crank_angle))
;endif else begin
;	scan_cts=round(cubic_soln(scan_coefs, scan_angle))
;	crank_cts=round(cubic_soln(crank_coefs, crank_angle))	
;endelse

end
