; minmaxB finds min and max B along a flux surface containing the point r,z
pro minmaxB, g, r, z, min_b, loc_b, max_b

forward_function bfield_f

rho_ech=rho_rz(g,r,z, psival) 
psinorm=(g.ssimag-psival[0])/(g.ssimag-g.ssibry)

bf=bfield_f(g,r,z)
loc_b=bf.tot[0]

nx=4.*g.mw
ny=4.*g.mh
npts=100

contour_psi, g, npts, 0., psinorm, nx, ny, psi, thetag, rg, b, /do_one

xs=rg*cos(thetag)+g.rmaxis
ys=rg*sin(thetag)+g.zmaxis

bs=bfield_f(g, xs, ys)
min_b=min(bs.tot)
max_b=max(bs.tot)

end
