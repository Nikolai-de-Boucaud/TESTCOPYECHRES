function rho_to_psin, rho, g
; rho is array of rho values
; g is equilibrium g structure
; Returns array of normalized poloidal flux values corresponding to rho

s=fluxmap(g)
rhog=s.rho/s.rho(n_elements(s.rho)-1)    ;rho, normalized toroidal flux
psing=(s.psi-min(s.psi))/(max(s.psi)-min(s.psi)) ; psin, normalized pol flux
finds=get_flind(rhog, rho)
psin=interpolate(psing, finds)
return, psin
end

; main, to test
;g=readg('~/echresdev/g103969.01600')
;rh=get_range(0.,1.,11)
;ps=rho_to_psin(rh, g)
;end
