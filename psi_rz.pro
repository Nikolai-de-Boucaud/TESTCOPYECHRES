;**************************************************************************
;
;  /u/schaffer/idl/psi_rz.pro
;
;  created:	
;   1998aug18	Mike Schaffer
;
;  modified:
;
;**************************************************************************
;
; -- This function calculates poloidal flux, psi, at specified points
;    in the (R,Z) plane, where R is the major radius and Z is the height
;    above the midplane. The function interpolates from an array of 
;    psi values specified at GEQDSK grid points.
;
; -- CALLING SEQUENCE --
;    psi_result = psi_rz(g, r_pts, z_pts, method [,norm=norm])
;
; -- INPUT --
; -- g = structure obtained from readg containing the GEQDSK.
; -- r_pts = array of R locations of a set of points (specified below)
; -- z_pts = array of Z locations of a set of points (specified below)
;	r_pts(i) = R location of the i-th point (in METERS).
;	z_pts(i) = Z location of the i-th point (in METERS).
; -- method = specifies interpolation method (specified below)
;	method = 0 is bilinear interpolation by IDL INTERPOLATE function.
;	method = 1 is bicubic convolution by IDL INTERPOLATE function.
;	method = 2 is bicubic spline by EFITLIB BISPLINE function.
;
; -- KEYWORDS --
; -- norm -- if set and non zero, normalized poloidal flux is returned. 
;	Default is absolute poloidal flux (MKS units)
;
; -- OUTPUT --
; -- psi_result = array of flux values (absolute or normalized) at the 
;	specified input coordinate points, such that psi_result = 
;	psi_pts(i) = flux at (r_pts(i), z_pts(i)).
;
;**************************************************************************


function  psi_rz,  g,  r_pts,  z_pts,  method,  norm = norm


if n_elements(norm) le 0 then norm = 0		; default to absolute flux

nr_pts = N_Elements(r_pts)
nz_pts = N_Elements(z_pts)
if (nr_pts ne nz_pts) then Begin
   print, 'psi_rz: ERROR: Unmatched # r_pts, # z_pts:', nr_pts, nz_pts
   GoTo, RTRN
endif


; -- construct R,Z grid points from GEQDSK and get corresponding fluxes --

rg = linspace(g.rgrid1, g.rgrid1 + g.xdim, g.mw)
zg = linspace(g.zmid - g.zdim/2., g.zmid + g.zdim/2., g.mh)
psigrid = g.psirz(0:g.mw-1, 0:g.mh-1)


; -- interpolate to get psi at (x_pts,y_pts) --

if method le 0  then begin		; -- bilinear interpolation --
   dr = rg(1) - rg(0)
   dz = zg(1) - zg(0)
   psi_pts = interpolate(psigrid, (r_pts-rg(0))/dr, (z_pts-zg(0))/dz)
   GoTo, NORM
endif

if method eq 1  then begin		; -- bicubic convolution --
   dr = rg(1) - rg(0)
   dz = zg(1) - zg(0)
   psi_pts = $
     interpolate(psigrid, (r_pts-rg(0))/dr, (z_pts-zg(0))/dz, cubic = -.5)
   GoTo, NORM
endif

if method ge 2  then begin		; -- bicubic spline --
   psi_pts = bispline(psigrid, rg, zg, r_pts, z_pts)
   GoTo, NORM
endif

print, 'psi_rz: ERROR: method =', method, ' No such interpolation method.'


; -- normalize fluxes --

NORM:
if norm ge 1 then begin
   denom = g.ssimag - g.ssibry
   psi_pts = (g.ssimag - temporary(psi_pts))/denom
endif


RTRN:
return, psi_pts
end
