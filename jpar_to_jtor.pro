; jpar_to_jtor.pro
; Translates <j_parallel(rho)> from rjpdrho to <j_toroidal>
; <j_tor> = <j_par> * F * <R^-2> / <B> / <R^-1>
; Inputs: g structure, rho=normalized minor radius
; Returns ratio <j_tor>/<j_par>

function jpar_to_jtor, g, rho
f=fluxfun_ech(g)
factor=abs(f.f*f.rm2_ave/f.bave/f.rm1_ave)
find=get_flind(f.rho/f.rho[n_elements(f.rho)-1], rho)
return, interpolate(factor, find)
end
