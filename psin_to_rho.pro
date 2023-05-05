function psin_to_rho, psin, g
; psin is array of normalized poloidal flux values
; g is g structure
; returns array of rho corresponding to psin
s=fluxmap(g)
rhog=s.rho/s.rho(n_elements(s.rho)-1)    ;rho, normalized toroidal flux
psing=(s.psi-min(s.psi))/(max(s.psi)-min(s.psi)) ; psin, normalized pol flux
finds=get_flind(psing, psin)
rho=interpolate(rhog, finds)
return, rho
end

; main, to test
;g=readg('~/echresdev/g103969.01600')
;ps=get_range(0.,1.,11)
;rho=psin_to_rho(ps, g)
;end
