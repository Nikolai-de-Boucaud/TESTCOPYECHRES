;@$IDLSOURCE/efitview/bfield_f
;@$ECHRES_PATH/fluxfun_ech
;@$ECHRES_PATH/makeline_lite
;@$ECHRES_PATH/index

;function dot, VA, VB
;return, total(VA*VB)
;end

;function mag, VC
;return, sqrt(dot(VC,VC))
;end


; snell.pro
; Returns a ray vector and estimated absorption point (r,z) (m) 
; for the refracted ray, given an incident ray starting at
; (R_ant, Z_ant) meters and polar and azimuthal angles (deg). Density (ne_13) and freq 
; (GHz) are used for the dispersion relation. The gstruct is used for geometry of
; the surface and for the cyclotron resonance location.

;*******************

function snell, R_ant, Z_ant, polar_angle, azi_angle, ne_13, $
	Te, freq, nharm, gstruct, debug=debug

if not keyword_set(debug) then debug=0
if ne_13 lt 0.001 then return, {status:0}

r_res=0.
z_res=0.

; All geometry in Cartesian coordinates
; Antenna is at y=0; (x,z) are radial and vertical location of antenna

polar_r=polar_angle*!dtor
azimuthal_r=azi_angle*!dtor

ntheta=180
f=fluxfun_ech(gstruct, ntheta, /do_spline)
rho_edge=f.rho_x[n_elements(f.rho_x)-1]/f.rho[n_elements(f.rho)-1]
nsurf=n_elements(f.r[0,*])

; use makeline to simulate the incident ray
dl=0.005	; separation between points on a ray, in m
ray=makeline_lite(R_ant,Z_ant,polar_r,azimuthal_r,dl,R_ant,1.8,2.0,0.0)
nrp=n_elements(ray.r)

; find (r,z) where ray crosses separatrix
rhoi=rho_rz(gstruct,ray.r,ray.z,psival,/norm)
inds=where(rhoi lt rho_edge, cinds)
if cinds gt 0 then ind=min(inds) else begin
	print, '  !!! snell: ray[0] misses the plasma!'
	return, {status:0}
endelse

; home in using linear interpolation
frac=(rhoi[ind-1]-1.0)/(rhoi[ind-1]-rhoi[ind])
r_edge=ray.r[ind-1]-frac*(ray.r[ind-1]-ray.r[ind])
z_edge=ray.z[ind-1]-frac*(ray.z[ind-1]-ray.z[ind])
x_edge=ray.x[ind-1]-frac*(ray.x[ind-1]-ray.x[ind])
y_edge=ray.y[ind-1]-frac*(ray.y[ind-1]-ray.y[ind])

; gamma is toroidal angle of edge point from antenna
gamma=atan(y_edge/x_edge)

; get the unit vector along the ray k_uv
k_uv=[ray.x[0]-x_edge, ray.y[0]-y_edge, ray.z[0]-z_edge]
ki_uv=k_uv/mag(k_uv)	; unit vector along incident ray

; now get unit vector perpendicular to flux surface surf_uv
thetapol_edge=atan((z_edge-gstruct.zmaxis)/(r_edge-gstruct.rmaxis))
ith=where(f.theta le thetapol_edge)
if ith[0] ge 0 then ith0=max(ith) else return, {status:0}

x1=f.r[ith0,nsurf-1]*cos(f.theta[ith0])+gstruct.rmaxis
x2=f.r[ith0+1,nsurf-1]*cos(f.theta[ith0+1])+gstruct.rmaxis
z1=f.r[ith0,nsurf-1]*sin(f.theta[ith0])+gstruct.zmaxis
z2=f.r[ith0+1,nsurf-1]*sin(f.theta[ith0+1])+gstruct.zmaxis
beta=-atan((x2-x1)/(z2-z1))
s=[cos(beta)*cos(gamma), cos(beta)*sin(gamma), sin(beta)]	; unit vector normal to plasma surface

; get the B unit vector B_uv
B=bfield_f(gstruct, r_edge, z_edge)
Bx=B.r*cos(gamma)-B.phi*sin(gamma)
By=B.r*sin(gamma)+B.phi*cos(gamma)
Bz=B.z
Bmag=B.tot
B_uv=[Bx[0], By[0], Bz[0]]/Bmag[0]	; unit vector B

; get the index of refraction
fce=28.0*B.tot[0]
fpe=28.46*sqrt(ne_13)
kk=(fpe/freq)^2
y=fce/freq
angle_bk=acos(dot(B_uv, k_uv))/!dtor
index=index('X', kk, y, angle_bk)

; apply Snell's law
theta_i=acos(abs(dot(ki_uv, s))) ; incident angle between k and s
theta_r=asin(sin(theta_i)/index.n)	; refracted angle in plane of k,s

; get unit vector for refracted ray
p=crossp(s,ki_uv)		; unit vector perp to s and k_uv
p=p/mag(p)

; Use condition k.p =0 (=> same plane as ki_uv and s) 
; and  k.s=cos(theta_r) (or Snell's law) to get the refracted wavevector k
C=-p[1]/p[0]
D=-p[2]/p[0]
E=cos(theta_r)
F=E/(C*s[0]+s[1])
G=-(D*s[0]+s[2])/(C*s[0]+s[1])
H=D+C*G
I=C*F
J=H^2+G^2+1.
L=2.*H*I+2.*F*G
M=I^2 + F^2 -1.
Q=L^2 - 4.*J*M
kz=(-L - sqrt(Q))/(2.*J)	; negative sign on sqrt works
kx=H*kz+I
ky=F+G*kz
kr=-1.*[kx, ky, kz] ; refracted ray

pol_refr=acos(kr[2])
azi_refr=!pi -gamma +atan(kr[1]/kr[0])

; get the (straight) refracted ray
R_edge=sqrt(x_edge^2+y_edge^2)
ray_refr=makeline_lite(R_edge,Z_edge,pol_refr,azi_refr,0.01,R_edge,1.25,2.0,-0.50)

; find the cyclotron resonance
Bcrit=freq/28./float(nharm)
Bray=bfield_f(gstruct, ray_refr.r, ray_refr.z)
ib=where(Bray.tot le Bcrit)
if ib[0] eq -1 then return, 0
ib0=max(ib)
r_res=ray_refr.r[ib0]
z_res=ray_refr.z[ib0]
y_res=ray_refr.y[ib0]
x_res=ray_refr.x[ib0]
gamma_res=atan(y_res/x_res)

; need k_parallel:

; First get unit vector B at the resonance
Bray_x=Bray.r[ib0]*cos(gamma_res)-Bray.phi[ib0]*sin(gamma_res)
Bray_y=Bray.r[ib0]*sin(gamma_res)+Bray.phi[ib0]*cos(gamma_res)
Bray_z=Bray.z[ib0]
B_uv=[Bray_x, Bray_y, Bray_z]/Bray.tot[ib0]	; unit vector B

; now get unit vector krc at the resonance
krc=[ray_refr.x[ib0]-ray_refr.x[ib0-1], ray_refr.y[ib0]-ray_refr.y[ib0-1], $
	ray_refr.z[ib0]-ray_refr.z[ib0-1]]
krc=krc/mag(krc)
;Rc=sqrt(ray_refr.x[ib0]^2+ray_refr.y[ib0]^2)
theta_rcb=acos(dot(krc, B_uv))

; get index of refraction near resonance
c=3.e10 	; cm/sec
lambda=c/(freq*1.e9)	; cm
fcec=freq/float(nharm)
fpec=28.46*sqrt(ne_13)
kkc=(fpec/freq)^2
yc=fcec/freq
indexc=index('X', kkc, yc, theta_rcb/!dtor )
lambda_c=lambda/indexc.n
k_par=2.*!pi *dot(krc, B_uv)/lambda_c ; /cm

; get k_parallel*v_thermal
v_th=0.2*1.325e9*sqrt(Te)		; cm/sec
kpar_vth=k_par*v_th/1.e9	; GHz

B_res=(freq-kpar_vth)/(28.*float(nharm))
ii=where(Bray.tot gt B_res)
ibc=ii[0]

r_res=ray_refr.r[ibc]
z_res=ray_refr.z[ibc]

if debug then begin

	print, 'Should be 0, 1:', dot(kr,p), dot(kr,s)/cos(theta_r)
	print, 'theta i, r=  ', theta_i/!dtor, theta_r/!dtor
	print, 'Density_19=  ', ne_13
	print, 'Index of ref=', index.n
	print, 'edge (X,Y,Z)=',x_edge,y_edge,z_edge
	print, 's=', s
	print, 'gamma=  ', gamma/!dtor
	print, 'ki_inc=  ', ki_uv
	print, 'kr=  ', kr
	print, 'polar_input, azi_input=      ', polar_angle, azi_angle
	print, 'pol_refracted, azi_refracted=', pol_refr/!dtor, azi_refr/!dtor
	print, 'kpar_vth=', kpar_vth
	print, 'R_res, Z_res=', r_res, z_res
endif

done:

return, {status:1, R:R_res, Z:Z_res}

end

;R_ant=2.3999
;Z_ant=0.6794
;polar_angle=104.0
;azi_angle=190. ;200.0
;ne_13=3.0 ;
;Te=0.969
;freq=110.
;nharm=2
;gstruct=readg(149708, 4580., runid='EFIT02', mode='MDS')

;rayref=snell(R_ant, Z_ant, polar_angle, azi_angle, ne_13, Te, freq, nharm, gstruct, $
;	/debug)
	
;end
