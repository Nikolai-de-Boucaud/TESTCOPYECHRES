function dot, VA, VB
return, total(VA*VB)
end

function mag, VC
return, sqrt(dot(VC,VC))
end

function xmfrac, R_ant, Z_ant, polar_angle, azi_angle, pol_id, azi_id, $
	app_incl, app_ell, freq, gstruct
; R_antenna = the antenna launch point major radius (m)
; Z_antenna = the antenna launch point elevation (m)
; polar_angle = polar launch angle (deg)
; azi_angle = azimuthal launch angle (deg)
; pol_id = polar angle of wave incident on steering mirror (deg)
; azi_id = azimuthal angle of wave incident on steering mirror (deg)
; app_incl = applied inclination of wave (deg)
; app_ell = applied ellipticity of wave (deg)
; freq = frequency (GHz)
; gstruct = g structure

; returns the x-mode fraction and polarization

polar_i=pol_id*!dtor
azimuthal_i=azi_id*!dtor
polar_r=polar_angle*!dtor
azimuthal_r=azi_angle*!dtor

dl=0.035	; separation between points on a ray, in m
ray=makeline_lite(R_ant, Z_ant, polar_r, $
	azimuthal_r, dl, max(gstruct.r), min(gstruct.r)+0.15, max(gstruct.z), min(gstruct.z))
nrp=n_elements(ray.r)
inl=min(where(ray.r eq R_ant))
if inl eq -1 then begin ; if mirror outside eqdsk boundary
	ray.r=[R_ant, ray.r[0:nrp-2]]	; add mirror point
	ray.z=[Z_ant, ray.z[0:nrp-2]]
	inl=0
endif

if inl gt 0 then begin
	rs=ray.r[inl-1:*]
	zs=ray.z[inl-1:*]
endif else if inl eq 0 then begin
	rs=ray.r
	zs=ray.z
endif

; find (r,z) where ray crosses separatrix
rhoi=rho_rz(gstruct,rs,zs,psival,/norm)
inds=where(rhoi lt 1.0, cinds)
if cinds gt 0 then ind=min(inds) else begin
	print, '  !!! Rays miss the plasma!'
	return, {success:0}
endelse
;ind=min(where(rhoi lt 1.0))
;if ind lt 0 then begin
;	print, '  !!! Rays miss the plasma!'
;	return, {success:0}
;endif

; home in using linear interpolation
frac=(rhoi[ind-1]-1.0)/(rhoi[ind-1]-rhoi[ind])
r_edge=rs(ind-1)-frac*(rs(ind-1)-rs(ind))
z_edge=zs(ind-1)-frac*(zs(ind-1)-zs(ind))
rs=[r_ant, r_edge, rs[ind:*]]
zs=[z_ant, z_edge, zs[ind:*]]

; get unit vector for B at plasma edge
; vector directions are (major radius, toroidal CCW from above, z)
bf=bfield_f(gstruct, r_edge, z_edge)
b_edge=[bf.r, bf.phi, bf.z]/bf.tot[0]

; form unit vectors for reflected and incident rays at steering mirror
kr_sm = [sin(polar_r)*cos(azimuthal_r), sin(polar_r)*sin(azimuthal_r), cos(polar_r)]
ki_sm = [sin(polar_i)*cos(azimuthal_i), sin(polar_i)*sin(azimuthal_i), cos(polar_i)]

; Now get local coordinate system (phi_sm,theta_sm) following steering mirror
phi_sm = crossp(ki_sm,kr_sm)/mag(crossp(ki_sm,kr_sm))
theta_sm = crossp(phi_sm, kr_sm)

; transform phi_sm, theta_sm, and kr_sm to plasma edge
; delta is angle of azimuthal rotation between mirror and edge
gammam = !PI - asin(kr_sm[1]/sin(acos(kr_sm[2])))
gammae = !PI - asin(sin(gammam)*R_ant/r_edge)
delta=gammam-gammae
rotmat2=[[cos(delta),sin(delta),0.],[-sin(delta),cos(delta),0.],[0.,0.,1.]]
k_edge=transpose(rotmat2##kr_sm)
theta_e=transpose(rotmat2##theta_sm)		; kedge == theta_e x phi_e

; generate ellipticity from low-density limit of Stix:
; For prop along B => RH ellipticity => beta_x > 0
; beta_x is the ellipticity at the plasma edge for pure x-mode
tau=acos(dot(k_edge,b_edge))	; angle between k and b
ye = freq/28./bf.tot		; y=(omega/omega_c) at plasma edge
beta_x = atan(abs((sqrt(sin(tau)^4 + 4.*(cos(tau)*ye)^2)-sin(tau)^2)/(2.*cos(tau)*ye)))
beta_x=reform(beta_x)
if tau GT !PI/2. then beta_x=-beta_x	; want LH if wave props opposite to B

; Now get the inclination alpha_x (between -pi/2 and +pi/2):
; For x-mode, want long axis of ellipse perp to B and k (|| y axis in Stix coords)
; if y_s x theta_edge is parallel to k_edge, then a negative rotation about 
; k_edge is needed to get the long axis of the E ellipse aligned with y_stix
y_s=crossp(b_edge,k_edge)/mag(crossp(b_edge,k_edge)) ;(y direction in Stix coords)
alpha_x = abs(acos(dot(y_s,theta_e)))
if dot(crossp(y_s, theta_e), k_edge) GT 0.0 then alpha_x=-alpha_x
if alpha_x LT -!PI/2. then alpha_x=alpha_x+!PI else $ ; get alpha bet 0 and pi
	if alpha_x GT !PI/2. then alpha_x=alpha_x-!PI 

; calculate per cent power in x-mode
P_x = x_frac(app_incl*!dtor, alpha_x, app_ell*!dtor, beta_x)

return, {success:1, x_frac:P_x, x_inclination:reform(alpha_x[0])/!dtor, $
	x_ellipticity:reform(beta_x[0])/!dtor}

end
