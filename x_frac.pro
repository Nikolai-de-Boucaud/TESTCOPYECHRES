; Input the ellipticity beta (rad) for pure x-mode and for the applied wave,
; and input the tilt alpha (rad) for x-mode and the applied wave,
; returns the fractional power in x-mode.
function x_frac, alpha_ac, alpha_xc, beta_ac, beta_xc
alpha_a=alpha_ac[0] & alpha_x=alpha_xc[0] 
beta_a=beta_ac[0] & beta_x=beta_xc[0]
xfrac=1.-(sin(beta_x-beta_a)^2 + sin(alpha_x-alpha_a)^2 * (1.-2.*sin(beta_x)^2) * $
	(1.-2.*sin(beta_a)^2))
return, xfrac[0]
end
