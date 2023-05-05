; 04-01-98	Q. Peng	- made the array size compatible with readg

pro rho_map,r,z,g,f,psi_n,rho_n

;procedure maps data from 2d R,Z grid into rho space
;R,Z are coordinates of data array
;g = readg structure
;f = fluxfun structure
;psi_n and rho_n are the returned normalized psi and rho vectors
;rho here is the sqrt of toroidal flux 

  w=findgen(g.mh)
  iz=interpol(w,(g.z)[0:g.mh-1],z)
  w=findgen(g.mw)
  ir=interpol(w,(g.r)[0:g.mw-1],r)
						;psi values of r,z grid points
  psi=interpolate((g.psirz)[0:g.mw-1,0:g.mh-1],ir,iz)
  rho=interpol(f.rho,f.psi,psi)			;rho values at psi points
  rho_n=rho/max(f.rho)				;normalize
  psi_n=(g.ssimag-psi)/(g.ssimag-g.ssibry)      ;normalize

return
end
