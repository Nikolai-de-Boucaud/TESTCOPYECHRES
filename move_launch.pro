; move_launch.pro
; args: starting (r0, z0), polar and azimuthal angles (deg), and distance dh
; returns new (r,z) and polar and azimuthal angles which produce the same ray
; but starting from the new location
; !!! Only correct for rays pointed inward (to smaller R)!!!
; Valid for dh > 0 or dh < 0

function move_launch, r0, z0, polar, azimuthal, dh
pol=polar*!dtor
azi=azimuthal*!dtor

z = z0 - dh * cos(pol)	; vertical
dx = dh * sin(pol) * cos(azi)	; radial
dy = dh * sin(pol) * sin(azi)	; toroidal

r = sqrt( (r0 - dx)^2 + dy^2) 

; use law of sines
new_azi=(!pi - asin( (r0/r) * sin(azi)))/!dtor

return, {r:r, z:z, pol:polar, azi:new_azi}
end
