; polazi_to_alphabeta
; procedure to calculate the alpha, beta of zvonkov 
; 	from the polar and azimuthal angles (all angles in deg)

pro polazi_to_alphabeta, pol, azi, alpha, beta
p=pol*!dtor
a=azi*!dtor
b=asin(sin(p)*sin(a))
alpha=asin(cos(p)/cos(b))/!dtor
beta=b/!dtor
end
