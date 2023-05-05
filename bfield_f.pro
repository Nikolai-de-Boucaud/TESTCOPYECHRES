;
;   4dlib/DIAGS/MSE/bfield_f.pro
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;



function bfield_f,g,r1,z1


; Modified:

;  06NOV02 Q.Peng - check if dividing by 0, allowing non-plasma g file
;  060897 Max Austin - fixed problem with 0's at the end
;                      g file


;modified version of grid_calc.pro, B. Rice 3/15/96
;calculates br,bz,bphi components at r,z from EFIT g structure
;this faster version does derivatives at grid points then bicubic spline

;  Print, 'Bfield_f: 6/8/97.'

forward_function bispline

  zi1 = 1 & zi2 = g.mh-1
  ri1 = 1 & ri2 = g.mw-1
  iw=indgen(g.mw)
  ih=indgen(g.mh)
  w = where(g.z(ih) lt min(z1))
  IF w(0) NE -1 THEN zi1=max(w)-2      	;reduce grid size to surround r,z
  w = where(g.z(ih) gt max(z1))
  IF w(0) NE -1 THEN zi2=min(w)+2
  w = where(g.r(iw) lt min(r1))
  IF w(0) NE -1 THEN ri1=max(w)-2
  w = where(g.r(iw) gt max(r1))
  IF w(0) NE -1 THEN ri2=min(w)+2 < g.mw-1

  z=g.z(zi1:zi2)			;reduced z grid
  r=g.r(ri1:ri2)			;reduced r grid
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

bz1 = bispline(bz,r,z,r1,z1)		;interpolate
br1 = bispline(br,r,z,r1,z1)
bt1 = bispline(bt,r,z,r1,z1)
psi_n = bispline(psi_norm,r,z,r1,z1)

return,{r: br1,z: bz1, phi: bt1, tot: sqrt( br1^2 + bz1^2 + bt1^2), $
        psi_n:psi_n}
end

