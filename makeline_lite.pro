; makeline_lite.pro
; Same as makeline but absent the rho calculations
; USES RADIANS FOR INPUT ANGLES!!
; also doesn't require a g structure

;*************************************************************************
; 
;  4dlib/FITTING/makeline.pro
;
;  created: 2/21/96 TCL
;
;  modified:
;    6-19-96	K. Greene -- Include name of procedure at beginning of
;		all print statements
;
;
;*************************************************************************
; 
;  This function generates equally spaced points along a line specified by
;  (R,z,poang,azang) suitable for doing line integrations in 4d.  The 
;  geometry is to specify a starting point in a poloidal plane using the
;  cylindrical coordinates (R,z) and a direction using two angles of a
;  spherical coordinate system (poang,azang) with the origin at (R,z) and
;  poang being the polar angle referenced to a line parallel to the z 
;  axis, and azang being the azimuthal angle referenced to the major
;  radius.  The propagation of the line is done in cartesian coordinates
;  and translated back to cylindrical for the call to RHO_RZ.  The 
;  routine will trace the line to the boundaries of the EQDSK grid unless
;  the keywords RMAX, RMIN, ZMAX, ZMIN are set and are more constraining 
;  than the EQDSK grid.
;  
;  Inputs: 
;  
;  RLOC - The major radius of the reference point in the EQDSK coordinate
;         system (in m).
;  ZLOC - The elevation of the reference point (in m).
;  !! Changed to be in RADIANS for makeline_lite !!
;  POANG - The polar angle of the reference vector (in deg).  The range is
;          assumed to be [0.,180.] 
;  AZANG - The azimuthal angle of the reference vector (in deg).  The range
;          is assumed to be [0.,360.]
;  DL - The distance between adjacent points in the propagation of the line
;       (in m).
; 
;  Optional:
; 
;  RMAX - The maximum major radius to which the line should be propagated
;         (in m).
;  RMIN - The minimum major radius to which the line should be propagated
;         (in m).
;  ZMAX - The maximum elevation to which the line should be propagated (in
;         m).
;  ZMIN - The minimum elevation to which the line should be propagated (in
;         m).
;  
;  Outputs:
;  
;  X - The x positions of the line (in m).  
;  Y - The y positions of the line (in m)
;  R - The R positions of the line (in m)
;  Z - The z positoins of the line (in m)
;
;**************************************************************************
   
function makeline_lite, rloc0, zloc0, poang, azang, dl, $
                   maxr, minr, maxz, minz
 
; RP: isolate the arguments
rloc=rloc0
zloc=zloc0

poangr = poang
azangr = azang

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
  print,'           Polar angle = ',poang
  print,'           Azimuthal angle = ',azang
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
 
f = {  X: xarr, $
       Y: yarr, $
       R: rarr, $
       Z: zarr }

return,f
end
