; alphabeta_to_polazi
; procedure to calculate the polar and azimthal angles from 
;	zvonkov's alpha, beta angles (all angles in deg)

; signs are defined so that positive alpha has rays going upward from plane
;	of the launcher
; beta is defined as negative if it corresponds to an azimuthal angle greater
;	than 180 deg, which gives ctr-ECCD for for Ip<0 and dominantly the F-B effect;
;	this is right-hand rotation about z.

pro alphabeta_to_polazi, alpha, beta, pol, azi
a=alpha*!dtor
b=beta*!dtor
p=acos(cos(b)*sin(a))
azi=180. - asin(sin(b)/sin(p))/!dtor 
pol=p/!dtor
end
