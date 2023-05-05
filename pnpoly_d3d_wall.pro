;Originally from Mike Van Zeeland, pnpoly_d3d.pro (version on August 4th, 2014)
;the returned value (1) means inside the wall; (-1) means outside
;convert d3d coordinate phi into the RHCS phi
;Last modified by Xi Chen, on Sept. 4th, 2014

function pnpoly_d3d_wall,r,z,phi,lim_dep_wid=lim_dep_wid,usebeambox=usebeambox
;uses http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html
;C INOUT - THE SIGNAL RETURNED: 
;C -1 IF THE POINT IS OUTSIDE OF THE POLYGON, 
;C 0 IF THE POINT IS ON AN EDGE OR AT A VERTEX, 
;C 1 IF THE POINT IS INSIDE OF THE POLYGON. 
;limiters at in rhcs
;philimsr=(90.-[95.,230.,310.])*;pi/180.
;lim_dep_wid=[limiter dR (m) *positive sticks inward and negative outward, limiter width in degrees]

;****NOTE, phi is in RHCS w/ phi=0 centered at d3d=90 in d3d device angle


usebeambox = 0

;so_file = '/fusion/projects/codes/echres/d3d/pnpoly_d3d.so'	
setenv,'sf='+getenv('ECHRES_PATH')+'/pnpoly_d3d.so'
so_file = getenv('sf')

IF NOT keyword_set(lim_dep_wid) THEN lim_dep_wid = double([.01,1.8]) ELSE lim_dep_wid=double(lim_dep_wid)

ninp=long(n_elements(r)) 

;convert d3d coordinate phi (degree) into the RHCS phi (radian)
phirhcs = (90.-phi)*!pi/180.

;this will take in any array of r,z,phi 
rd=reform(double(r),ninp) ;double([1.02,1.05])
zd=reform(double(z),ninp)
pd=reform(double(phirhcs),ninp)  ;phi in rhcs	 	

inpoly=long(r*0)

S = CALL_EXTERNAL(so_file, $ 
                  'pnpoly_access_',rd,zd,pd ,ninp,lim_dep_wid,inpoly)

if ninp eq 1 then inpoly=inpoly(0)

inpoly=fix(inpoly)
return, inpoly

end
