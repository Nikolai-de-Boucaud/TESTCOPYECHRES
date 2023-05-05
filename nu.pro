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
