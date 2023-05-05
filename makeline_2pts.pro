; makeline_2pts.pro
; Same engine as makeline but absent the rho calculations.
; Also, instead of input angles, the inputs are two points.
; makeline_2pts then draws a straight line through those 2 points.
; Includes phi calculation.

;  This function generates equally spaced points along a line specified by
;  (R1,z1,phi1,R2,Z2,phi2), starting at R1,Z1,phi1 and only in the direction
;  through R2,Z2,phi2
; 
;  Inputs: 
;  
;  R1 - The major radius of the first reference point (in m).
;  Z1 - The elevation of the reference point (in m).
;  phi1 - The phi of the first reference point (in deg).
;  R2,Z2,phi2 - Same for the second point.
;  DL - The distance between adjacent points in the propagation of the line
;       (in m).
; 
;  Optional keywords:
; 
;  RMAX - The maximum major radius to which the line should be propagated
;         (in m).
;  RMIN - The minimum major radius to which the line should be propagated
;         (in m).
;  ZMAX - The maximum elevation to which the line should be propagated (in
;         m).
;  ZMIN - The minimum elevation to which the line should be propagated (in
;         m).
;  polang - returns the polar angle (deg)
;  aziang - returns the azimuthal angle (deg)
;
;  left_hand_coords - use left hand cylindrical coordinate system to
;         conform to DIII-D vacuum vessel convention. 
;  
;  Outputs:
;  
;  X - The x positions of the line (in m).  
;  Y - The y positions of the line (in m)
;  R - The R positions of the line (in m)
;  phi - The phi position of the line (in deg)
;  Z - The z positions of the line (in m)
;
;**************************************************************************
   
function makeline_2pts, R1, Z1, PHI1, R2, Z2, PHI2, dl, $
                   left_hand_coords=left_hand_coords, $
				   maxr=maxr, minr=minr, maxz=maxz, minz=minz, $
				   polang=polang, aziang=aziang

if keyword_set(left_hand_coords) then phisign=-1.0 else phisign=1.0

rloc=R1
zloc=Z1

if n_elements(maxz) eq 0 then maxz=2.0
if n_elements(minz) eq 0 then minz=-2.0

if R2 ge R1 then begin
	if n_elements(maxr) eq 0 then maxr=2.5
	if n_elements(minr) eq 0 then minr=R1
endif else begin
	if n_elements(minr) eq 0 then minr=1.0
	if n_elements(maxr) eq 0 then maxr=R1
endelse

; generate the poloidal and azimuthal angles
phi0=phi1
x1=R1*cos(0.*!dtor)
y1=0.0 ;R1*sin(0.*!dtor)
x2=R2*cos((phi2-phi1)*!dtor)
y2=R2*sin(abs(phi2-phi1)*!dtor)
h=sqrt( (x2-x1)^2 + (y2-y1)^2 )
ds=sqrt((x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2 )

poangr=asin(h/ds)
if Z2 lt Z1 then poangr=!pi -poangr
azangr=asin((y2-y1)/h)
if phi2 lt phi1 then azangr=-azangr
if R1 gt R2 then begin
	poangr = - poangr
	azangr = -azangr
endif

polang=poangr/!dtor
aziang=azangr/!dtor

;
;generate the unit reference vector
;
dx = sin(poangr) * cos(azangr)
dy = sin(poangr) * sin(azangr)
dz = cos(poangr)

;
;set the limits of the propagation
;
if (rloc gt maxr or rloc lt minr or $
    zloc gt maxz or zloc lt minz) then begin
  x = rloc
  y = 0.
  z = zloc
  k = 0
  while (sqrt(x^2.+y^2.) lt minr or sqrt(x^2.+y^2.) gt maxr $
         or z lt minz or z gt maxz) do begin
    x = x + dl*dx
    y = y + dl*dy
    z = z + dl*dz
    k = k + 1
    if (k gt 200) then goto, JUMP1
  endwhile
  JUMP1:
  if (k le 200) then begin
    rloc = sqrt(x^2. + y^2.)
    zloc = z
    azangr = azangr - atan(y,x)
    dx = sin(poangr) * cos(azangr)
    dy = sin(poangr) * sin(azangr)
    goto, LIMIT 
  endif
  x = rloc
  y = 0.
  z = zloc
  k = 0
  while (sqrt(x^2.+y^2.) lt minr or sqrt(x^2.+y^2.) gt maxr $
         or z lt minz or z gt maxz) do begin
    x = x - dl*dx
    y = y - dl*dy
    z = z - dl*dz
    k = k + 1
    if (k gt 200) then goto, JUMP2
  endwhile
  JUMP2:
  if (k le 200) then begin
    rloc = sqrt(x^2. + y^2.)
    zloc = z
    azangr = azangr - atan(y,x)
    dx = sin(poangr) * cos(azangr)
    dy = sin(poangr) * sin(azangr)
    goto, LIMIT 
  endif
  print,'MAKELINE:  Cannot translate the reference point to the EQDSK grid.'
  print,'           Check the input values:  R = ',rloc,'  z =',zloc
  print,'           Polar angle = ',poangr
  print,'           Azimuthal angle = ',azangr
  return, rho
endif
 
LIMIT:
x = rloc
y = 0.
z = zloc
i = 0

while (sqrt(x^2.+y^2.) ge minr and sqrt(x^2.+y^2.) le maxr $
       and z ge minz and z le maxz) do begin
  x = x + dl*dx
  y = y + dl*dy
  z = z + dl*dz
  i = i + 1
endwhile

x = rloc
y = 0.
z = zloc
j = 0

while (sqrt(x^2.+y^2.) ge minr and sqrt(x^2.+y^2.) le maxr $
       and z ge minz and z le maxz) do begin
  x = x - dl*dx
  y = y - dl*dy
  z = z - dl*dz
  j = j + 1
endwhile

if (i gt 1) then pos = findgen(i-1) else pos = 0.

if (j gt 2) then begin
  neg = 1. - float(j) + findgen(j-1) 
  mult = [neg,pos] 
endif else mult = pos

xarr = rloc + mult * dx * dl
yarr = mult * dy * dl
zarr = zloc + mult * dz * dl
rarr = sqrt(xarr^2. + yarr^2.)

phi=phi0 + phisign*atan(yarr/rarr)/!dtor

f = {  X: xarr, $
       Y: yarr, $
       R: rarr, $
	   phi:phi, $
       Z: zarr }

return,f
end

;; main, for testing

;!p.multi=[0,1,2]
;R1=1.72 & Z1=0.0 & phi1=0.
;R2=1.75 & Z2=-0.02 & phi2=-1.
;
;s=makeline_2pts(R1,Z1,phi1,R2,Z2,phi2, .05);, /left_hand)
;plot, s.r, s.z, xr=[1.,2.5], yr=[-2.,2.]
;plots, R1, Z1, psym=4
;plots, R2, Z2, psym=4
;
;plot, s.r, s.phi, xr=[1.,2.5], yr=[-20,20]
;plots, R1, phi1, psym=4
;plots, R2, phi2, psym=4
;
;end
