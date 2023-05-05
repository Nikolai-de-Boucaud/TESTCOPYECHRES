; ang.pro
; functions for returning mirror and reflected ray angles

; Procedures:
; scancrank_to_polazi, scand, crankd, axis_tiltd, polard, azid
; polazi_to_scancrank, pold, azid, axis_tiltd, scand, crankd
; tiltfacet_to_polazi, tilt_d, facet_d, polar_d, azi_d
; polazi_to_tiltfacet, polar_d, azi_d, tilt_d, facet_d
; tiltfacet_to_scancrank, tilt_d, facet_d, antenna_inclinationd, scan_d, crank_d
; scancrank_to_tiltfacet, scan_d, crank_d, antenna_inclinationd, tilt_d, facet_d
; mir_norm, angle1, angle2, polar_n, azi_n, antenna_inclinationd, $
;	P2001style=P2001style
; ang, pol_i, azi_i, pol_n, azi_n, pol_r, azi_r
; get_mir_angles, pol_i, azi_i, pol_r, azi_r, offset, $
;	pol_n, azi_n, tilt, facet, scan, crank
; get_reflected_angles, tilt, facet, pol_i, azi_i, offset_angle, $
;	pol_r, azi_r, antenna_inclinationd, antenna_style=antenna_style	

function dot, a, b
return, total(a*b)
end

;*************************************************************
; The following procedures to transform among the (pol, azi), (tilt, facet), and
; (scan, crank) coordinate systems are per notes of 26 November 2001.

pro rot_t, polid, aziid, rotate_angled, pold, azid
; Rotates a vector with polar and azimuthal angles pold, azid, by
;	an angle rotate_angled about the toroidal axis (ie, par to  ZxR)
dtr=!PI/180.
poli=polid*dtr & azii=aziid*dtr & ra=rotate_angled*dtr
nz=cos(poli)
nr=sin(poli)*cos(azii)
nt=sin(poli)*sin(azii)
gamma=-atan(nr/nz)
s=sqrt(nr^2+nz^2)
ntp=nt
nrp=s*sin(gamma-ra)
nzp=s*cos(gamma-ra)
pold=acos(nzp)/dtr
azid=180. - atan(ntp/nrp)/dtr
end

pro rot_r, polid, aziid, rotate_angled, pold, azid
; Rotates a unit vector with polar and azimuthal angles pold, azid, by
;	an angle rotate_angled about the radial axis in RH sense
;	per notes of 30 Nov 01
dtr=!PI/180.
poli=polid*dtr & azii=aziid*dtr & ra=rotate_angled*dtr
nz=cos(poli)
nr=sin(poli)*cos(azii)
nt=sin(poli)*sin(azii)
gamma=-atan(nt/nz)
s=sqrt(nt^2+nz^2)
pold=acos(s*cos(gamma-ra))/dtr
azid=180. + atan(s*sin(gamma-ra)/nr)/dtr
end

pro scancrank_to_polazi, scand, crankd, axis_tiltd, polard, azid
; axis_tiltd is the inclination of the axis of the crank from horizontal, 
;	assumed to be in the azi=0 deg plane
; scand is the inclination of the normal to the mirror from vertical,
;	in the azi=180 deg plane
; crank is the rotation about the crank axis, positive being co-cd
dtr=!PI/180.
; first rotate axes about t-axis by the tilt of the launcher, then rotate
; the unit vector (the mirror normal) about
; the new R axis by the crank angle, then unrotate the axes about the t_axis by the tilt
rot_t, scand, 180., axis_tiltd, pol, azi
rot_r, pol, azi, crankd, polp, azip
rot_t, polp, azip, -axis_tiltd, polard, azid
end

pro polazi_to_scancrank, pold, azid, axis_tiltd, scand, crankd
rot_t, pold, azid, axis_tiltd, poldp, azidp
scan=-asin(sin(poldp*!dtor )*cos(azidp*!dtor ))
crankd=-asin(sin(poldp*!dtor )*sin(azidp*!dtor )/cos(scan))/!dtor
scand=scan/!dtor + axis_tiltd
end

pro tiltfacet_to_polazi, tilt_d, facet_d, polar_d, azi_d
; convert tilt and facet angles to polar and azimuthal angles of mirror normal
; all angles in deg
dtr=!PI/180.
polar_d=acos(cos(tilt_d*dtr)*cos(facet_d*dtr))/dtr
azi_d=(!PI +asin(sin(facet_d*dtr)/sin(polar_d*dtr)))/dtr
end

pro polazi_to_tiltfacet, polar_d, azi_d, tilt_d, facet_d
; convert to tilt and facet angles from polar and azimuthal angles of mirror normal
; all angles in deg
dtr=!PI/180.
facet_d=asin(sin(polar_d*dtr)*sin(azi_d*dtr-!PI))/dtr
tilt_d=acos(cos(polar_d*dtr)/cos(facet_d*dtr))/dtr
end

pro tiltfacet_to_scancrank, tilt_d, facet_d, antenna_inclinationd, scan_d, crank_d
tiltfacet_to_polazi, tilt_d, facet_d, polar_d, azi_d
polazi_to_scancrank, polar_d, azi_d, antenna_inclinationd, scan_d, crank_d
end

pro scancrank_to_tiltfacet, scan_d, crank_d, antenna_inclinationd, tilt_d, facet_d
scancrank_to_polazi, scan_d, crank_d, antenna_inclinationd, polar_d, azi_d
polazi_to_tiltfacet, polar_d, azi_d, tilt_d, facet_d
end

pro mir_norm, angle1, angle2, polar_n, azi_n, antenna_inclinationd, P2001style=P2001style
; if P2001style NOT set, then use tilt, facet angle system
; 	appropriate to GA antennas and P1999 antenna
; if P2001style IS set, then use scan, crank angle system
;	 appropriate to P2001 antenna
; inputs:
; angle1=mirror tilt angle or scan angle, in degrees
; angle2=mirror facet  or crank angle, in degrees (+ is co-CD; polar angle GT pi)
; antenna_inclinationd=inclination in deg from horizontal of crank axis, in azi=0 plane
; returns:
; polar_n, azi_n are polar and azimuthal angles, IN RADIANS, of mirror normal

if keyword_set(P2001style) then $
	scancrank_to_polazi, angle1, angle2, antenna_inclinationd, polar_nd, azi_nd $
else tiltfacet_to_polazi, angle1, angle2, polar_nd, azi_nd
polar_n=polar_nd*!dtor
azi_n=azi_nd*!dtor
end

pro ang, pol_i, azi_i, pol_n, azi_n, pol_r, azi_r
; inputs: (all angles in radians)
; pol_i, azi_i are polar and azimuthal angles of incident ray, in radians
; pol_n, azi_n are same for mirror normal
; output: pol_r, azi_r are same for the output angles of the reflected ray

; Generalize to input arrays for pol_n, azi_n on 3 June 2010,
; assume pol_i and azi_i are scalars

np=n_elements(pol_n)
pol_r=fltarr(np)
azi_r=fltarr(np)
nx=fltarr(np)
ny=fltarr(np)
nz=fltarr(np)
dv=fltarr(3,np)
nv=fltarr(3,np)

;make unit vectors from angles
ix=sin(pol_i)*cos(azi_i)	; incident ray vector
iy=sin(pol_i)*sin(azi_i)
iz=cos(pol_i)
iv=[ix, iy, iz]

for i=0, np-1 do begin
	nx[i]=sin(pol_n[i])*cos(azi_n[i])	; mirror normal
	ny[i]=sin(pol_n[i])*sin(azi_n[i])
	nz[i]=cos(pol_n[i])
	nv[*,i]=[nx[i], ny[i], nz[i]]
	dv[*,i]=crossp(iv, nv[*,i])
endfor

dx=dv[0,*]
dy=dv[1,*]
dz=dv[2,*]

; solve per notes 11/13/2000
for i=0, np-1 do begin
	A=dot(iv,nv[*,i])/(nz[i]-ny[i]*dz[i]/dy[i])
	B=-(nx[i]-ny[i]*dx[i]/dy[i])/(nz[i]-ny[i]*dz[i]/dy[i])
	C=-dx[i]/dy[i]
	D=-dz[i]/dy[i]
	QA=(dx[i]/dy[i])^2 + 2.*B*dx[i]*dz[i]/dy[i]^2 + (B*dz[i]/dy[i])^2 + B^2 +1.
	QB=2.*A*(dx[i]*dz[i]/dy[i]^2 + B*(dz[i]/dy[i])^2 +B)
	QC=(A*dz[i]/dy[i])^2 + A^2 -1.
	bsmfac=QB^2 - 4.*QA*QC
	rx=(-QB-sqrt(bsmfac))/2./QA
	rz=A+rx*B
	ry=-rx*dx[i]/dy[i] - rz*dz[i]/dy[i]
	rv=[rx,ry,rz]
	pol_r[i]=acos(rv[2])
	azi_r[i]=!PI - asin(rv[1]/sin(pol_r[i]))
endfor
end

pro get_mir_angles, pol_ip, azi_ip, pol_rp, azi_rp, offset, $
	pol_n, azi_n, tilt, facet, scan, crank, antenna_inclinationd
; inputs: 
; 	pol_ip, azi_ip: polar and azimuthal angles of incident ray, in radians
; 	pol_r, azi_r: the output angles of the reflected ray, radians
;	offset: offset angle, radians
;	antenna_inclinationd: inclination of P2001 launcher from horizontal (22.3 deg)
; outputs: 
;	pol_n, azi_n: angles of mirror normal, in radians
; 	tilt, facet, scan, crank: angles of mirrors, in deg

dtr=!PI/180.

azi_i=azi_ip; + offset
azi_r=azi_rp + offset
pol_i=pol_ip
pol_r=pol_rp

;make unit vectors from angles for incident and reflected rays
ix=sin(pol_i)*cos(azi_i)
iy=sin(pol_i)*sin(azi_i)
iz=cos(pol_i)
rx=sin(pol_r)*cos(azi_r)
ry=sin(pol_r)*sin(azi_r)
rz=cos(pol_r)

; solve per notes of 16 Nov 01
a=(ry-iy)/(ix-rx)
b=(rz-iz)/(ix-rx)
c=(rx*iz-rz*ix)/(ry*iz-rz*iy)
d=(ry*ix-rx*iy)/(ry*iz-rz*iy)
e=a*(d-b)/(a-c) + b
nz=1./sqrt(e^2 + ((d-b)/(a-c))^2 + 1.)
nx=e*nz
ny=((d-b)/(a-c))*nz
pol_n=asin(sqrt(nx^2+ny^2))
azi_n=!PI + atan(ny/nx)

polazi_to_tiltfacet, pol_n/dtr, azi_n/dtr, tilt, facet
polazi_to_scancrank, pol_n/dtr, azi_n/dtr, antenna_inclinationd, scan, crank

end

pro get_reflected_angles, tilt, facet, pol_i, azi_i, offset_angle, $
	pol_r, azi_r, antenna_inclinationd, antenna_style=antenna_style
; tilt and facet angles in degrees
; pol_i, azi_i angles of incident ray on steering mirror (radians)
; pol_r, azi_r angles of reflected ray from steering mirror (radians)
; offset_angle in radians due to sideward shift of antenna from center of port
;
; If antenna_style eq 'P2001' send this info to mir_norm to use (scan,crank) system,
;	otherwise use the (tilt,facet) system of angles to characterize mirror
;
if strmid(antenna_style, 0, 2) eq 'P2' then P2001style=1 else P2001style=0
mir_norm, tilt, facet, pol_n, azi_n, antenna_inclinationd, P2001style=P2001style
ang, pol_i, azi_i, pol_n, azi_n, pol_r, azi_rd
azi_r=azi_rd - offset_angle
if n_elements(pol_r) eq 1 then begin
	pol_r=pol_r[0]
	azi_r=azi_r[0]
endif
end
