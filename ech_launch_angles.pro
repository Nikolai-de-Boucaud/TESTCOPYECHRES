;************************************************
; add these files to compile ech_launch_angles alone:
;@$ECHRES_PATH/ang
;@$ECHRES_PATH/antenna_angles
;@$ECHRES_PATH/echcal

function ech_launch_angles, setupfile, shot

if n_params() lt 2 then shot=1000000L	; use current shot for timcon

fh=file_test(setupfile)
if fh ne 1 then return, {error:1, error_msg:'No setup file:'+setupfile}

s=read_setup_file(infile=setupfile)
if not s.success then return, {error:1, error_msg:'Could not read setup file:'+setupfile}

; get gyrotron number, tank number, etc
params=resolve_setup(s, shotnumber=shot)
if not params.success then return, {error:1, error_msg:'Could not resolve setup'}

tilt_angle=fltarr(params.numbersystems)
facet_angle=fltarr(params.numbersystems)
poloidal_angle=fltarr(params.numbersystems)
toroidal_angle=fltarr(params.numbersystems)
portazi=fltarr(params.numbersystems)

for i=0, params.numbersystems-1 do begin

	z=echcal(shot, params.gyrotron[i])

	; get tilt and facet angles
	Antenna_Angles, shot, z.AntNum, ta, fa, $
		params.pol_counts[i], params.tor_counts[i], /antenna_num
		
	facet_angle[i]=fa
	tilt_angle[i]=ta
	
	get_reflected_angles, ta, fa, $
		z.Antenna_Data.pol_id*!dtor, z.Antenna_Data.azi_id*!dtor, $
		z.Antenna_Data.offset_angle*!dtor, $
		polar_r, azimuthal_r, $
		z.Antenna_Data.antenna_inclinationd, antenna_style='P2'
		
	poloidal_angle[i]=polar_r/!dtor
	toroidal_angle[i]=azimuthal_r/!dtor
	
	portazi[i]=float(strmid(z.Antenna_Data.port,0,5))

endfor

k=sort(portazi)

return, {error:0, number_systems:params.numbersystems, gyrotron:params.gyrotron[k], $
	gyrotron_number:params.gyronumber[k], antenna_number:params.antnumber[k], $
	port_number:params.portnumber[k], port_azi:portazi[k], $
	system_number:params.tanknumber[k], $
	poloidal_counts:params.pol_counts[k], toroidal_counts:params.tor_counts[k], $
	tilt_angle:tilt_angle[k], facet_angle:facet_angle[k], $
	poloidal_angle:poloidal_angle[k], toroidal_angle:toroidal_angle[k], $
	polarizer1:params.polarizer1[k], polarizer2:params.polarizer2[k], $
	powersupply:params.powersupply[k], $
	error_msg:''}

end
