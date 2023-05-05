;polazi_to_counts.pro
@ang
@antenna_angles

function polazi_to_counts, shot_num, antenna_number, pol_d, azi_d
; shot_num = shot number, to get correct calibration
; antenna_number = antenna number in ECHSysData
; pol_d = polar angle of incident ray in degrees
; azi_d = azimuthal angle of incident ray in degrees
; Returns {tiltcnts, facetcnts} = antenna counts to get the incident ray angles

; if it's a GA antenna, forget it
if antenna_number eq 0 or antenna_number eq 1 or antenna_number eq 4 or $
	antenna_number eq 5 then return, {success:0}

shot=shot_num
@$ECHRES_PATH/ECHSysData	; get system data and calibrations for shot

pol_i=Antenna_data[antenna_number].pol_id*!dtor
azi_i=Antenna_data[antenna_number].azi_id*!dtor
offset_angle=Antenna_data[antenna_number].offset_angle*!dtor
antenna_inclinationd=Antenna_data[antenna_number].antenna_inclinationd
antenna_style=Antenna_data[antenna_number].DwgNo

tiltd=60.
facetd=0.
pol_r=90.*!dtor
azi_r=180.*!dtor
i=0

while ((i lt 10) and ((abs(pol_r/!dtor-pol_d) gt 0.001) or $
	(abs(azi_r/!dtor - azi_d) gt 0.001))) do begin
	get_reflected_angles, tiltd, facetd, pol_i, azi_i, offset_angle, $
		pol_r, azi_r, antenna_inclinationd, antenna_style=antenna_style
	tiltd += (pol_d - pol_r/!dtor) * 0.5
	facetd += (azi_d - azi_r/!dtor)
	i++
endwhile

Antenna_counts, shot, antenna_number, tiltd, facetd, $
	tilt_counts, facet_counts, /antenna_num

return, {success:1, pol_d:pol_r/!dtor, azi_d:azi_r/!dtor, $
	tilt_counts:tilt_counts, facet_counts:facet_counts}

end
