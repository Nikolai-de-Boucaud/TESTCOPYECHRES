;
;   4dlib/DIAGS/MSE/bfield_f.pro
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;



function bfield_pol,g


; HISTORY:
;
;  06NOV02 Q.Peng - modified from bfield_f.pro
;                   skip reducing grid size and bispline
;                   compute br,bz on 2-D grid from g structure instead of 
;                   interpolating onto 1-D r1 and z1 (old inputs of bfield_f)
;
;  060897 Max Austin - fixed problem with 0's at the end
;                      g file


;modified version of grid_calc.pro, B. Rice 3/15/96
;calculates br,bz,bphi components at r,z from EFIT g structure
;this faster version does derivatives at grid points then bicubic spline

forward_function bispline

  zi1 = 0 & zi2 = g.mh-1
  ri1 = 0 & ri2 = g.mw-1
  iw=indgen(g.mw)
  ih=indgen(g.mh)

  r=g.r
  z=g.z
  nz=n_elements(z)
  nr=n_elements(r)

  if g.cpasma ne 0 then ip_sign=-g.cpasma/abs(g.cpasma) else ip_sign = 1
  psi_1=ip_sign*g.psirz(ri1:ri2,zi1:zi2)
  dsdr=psi_1 & dsdr(*,*)=0.0
  dsdz=dsdr & br=dsdr & bz=dsdr & bt=dsdr

  for i=0,nz-1 do dsdr(*,i)=deriv(r,psi_1(*,i))	    ;derivatives at grid points
  for i=0,nr-1 do dsdz(i,*)=deriv(z,psi_1(i,*))
  for i=0,nz-1 do   br(*,i)=-1./r*dsdz(*,i)
  for i=0,nz-1 do   bz(*,i)= 1./r*dsdr(*,i)

btr=g.bcentr*g.rzero/r
for i=0,nz-1 do bt(*,i)=btr	;vacuum field

pn=findgen(g.mw)/(g.mw-1)
if g.ssimag ne g.ssibry then $
psi_norm=(g.ssimag-psi_1/ip_sign)/(g.ssimag-g.ssibry) $
else psi_norm = psi_1/ip_sign

for i=0,nz-1 do begin
  w=where(abs(psi_norm(*,i)) lt 1.0001)
  if w(0) gt -1 then begin
    Bt(w,i)=interpol(g.fpol(iw),pn,psi_norm(w,i))/r(w)
  endif
endfor

return,{r: br,z: bz, phi: bt, tot: sqrt( br^2 + bz^2 + bt^2), $
        pol: sqrt(br^2 + bz^2), psi_n:psi_norm}
end

