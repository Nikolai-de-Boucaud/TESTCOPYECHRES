pro read_MDS_setup, shot, success=success
; Reads MDSplus for ECH system configuration for shot shot

Common ECHCOM

success=0

clear_com	; clear the common variables for new ones 

ecdat=get_ech(shot, PrgDat.time, $
	dt=PrgDat.Prefs.pow_avg_half_period, /alldata)

if not ecdat.stat then begin
	message, 'ECH data not available from MDSplus for shot '+strtrim(shot,2), /info
	return
endif

if ecdat.num_systems eq 0 then begin
	message, 'No ECH systems in MDSplus for shot '+strtrim(shot,2), /info
	return
end

ECHSys.NumTanks=ecdat.num_systems
ECHSys.NumAntennas=ecdat.num_antennas

print, '  *** read_MDS_setup: Averaging period (plus and minus) for power = ' + $
	string(PrgDat.Prefs.pow_avg_half_period, format='(f4.1)') + ' msec'

for ii=0, ecdat.Num_Systems-1 do begin

	gyroname=ecdat.systems[ii].gyrotron
	if strpos(gyroname, '"') ge 0 then gyroname=strmid(gyroname, 1, strlen(gyroname)-2)
	if strpos(strupcase(gyroname), 'ROW') gt 0 then gyroname='SCARECROW'

	z=echcal(shot, gyroname, id_type='name')	; get calibration constants

	ECHSys.Antenna[ii].z_launch=ecdat.systems[ii].antenna.z_pivot
	ECHSys.Antenna[ii].r_launch=ecdat.systems[ii].antenna.r_pivot
	ECHSys.Antenna[ii].pol_id=ecdat.systems[ii].antenna.pol_id
	ECHSys.Antenna[ii].azi_id=ecdat.systems[ii].antenna.azi_id
	ECHSys.Antenna[ii].offset_angle=ecdat.systems[ii].antenna.offset_angle
	ECHSys.Antenna[ii].antenna_inclinationd=ecdat.systems[ii].antenna.antenna_inclinationd
	ECHSys.Antenna[ii].DwgNo=ecdat.systems[ii].drawing_num
	ECHSys.Antenna[ii].pol_ang=ecdat.systems[ii].antenna.tilt_angle
	ECHSys.Antenna[ii].tor_ang=ecdat.systems[ii].antenna.facet_angle
	ECHSys.Antenna[ii].divergence=ecdat.systems[ii].antenna.dispersion
	ECHSys.Antenna[ii].alpha_a=ecdat.systems[ii].transmission.applied_inclination
	ECHSys.Antenna[ii].beta_a =ecdat.systems[ii].transmission.applied_ellipticity
	ECHSys.Tank[ii].TankNo=ecdat.systems[ii].tank_number
	ECHSys.Tank[ii].Antenna=ecdat.systems[ii].drawing_num
	ECHSys.Tank[ii].gyroname=strupcase(gyroname)
	ECHSys.Tank[ii].AntNum=ecdat.systems[ii].antenna_number
	ECHSys.Tank[ii].AntPort=ecdat.systems[ii].port
	if ecdat.systems[ii].freq gt 120. then $
		ECHSys.Tank[ii].freq=ecdat.systems[ii].freq/1.e9 $
		else ECHSys.Tank[ii].freq=110.
	ECHSys.Tank[ii].power_MW=ecdat.systems[ii].pinjs/1.e6

	ECHSys.InputTable[ii].gyroname=gyroname
	ECHSys.InputTable[ii].xfrac=ecdat.systems[ii].xmfracs
	if ecdat.systems[ii].xmfracs gt 0.5 then ECHSys.Tank[ii].xfrac=1 $
		else ECHSys.Tank[ii].xfrac=0

;IF strmatch(ECHSys.InputTable[ii].Gyroname,'TLeia',/FOLD_CASE) THEN BEGIN 
;ecdat.systems[ii].ray_polars=162	;160.016
;ecdat.systems[ii].ray_azis=241		;237.933
;ENDIF

;IF strmatch(ECHSys.InputTable[ii].Gyroname,'TLuke',/FOLD_CASE) THEN BEGIN 
;ecdat.systems[ii].ray_polars=162
;ecdat.systems[ii].ray_azis=241
;ENDIF
	ECHSys.InputTable[ii].PolarAng=ecdat.systems[ii].ray_polars
	ECHSys.InputTable[ii].AziAng=ecdat.systems[ii].ray_azis
	ECHSys.InputTable[ii].PolCts=ecdat.systems[ii].poloidal_cnts
	ECHSys.InputTable[ii].TorCts=ecdat.systems[ii].toroidal_cnts
	ECHSys.Antenna[ii].Tor_Ang=ecdat.systems[ii].facet_angles
	ECHSys.Antenna[ii].Pol_Ang=ecdat.systems[ii].tilt_angles
	ECHSys.InputTable[ii].pol1=ecdat.systems[ii].polarizer1
	ECHSys.InputTable[ii].pol2=ecdat.systems[ii].polarizer2
	ECHSys.InputTable[ii].f_abs=0.
	ECHSys.InputTable[ii].rho_peak=0.
	ECHSys.InputTable[ii].eccd=0.

	ECHSys.XLine[ii].gammasd[*]=0.0
	ECHSys.XLine[ii].gammasd=ecdat.systems[ii].transmission.gamma
	ECHSys.XLine[ii].mir_type[*]=0
	ECHSys.XLine[ii].mir_type=ecdat.systems[ii].transmission.mirror_type
	ECHSys.XLine[ii].gm1[*]=0.0
	ECHSys.XLine[ii].gm1=ecdat.systems[ii].transmission.grvmirror_C1
	ECHSys.XLine[ii].gm2[*]=0.0
	ECHSys.XLine[ii].gm2=ecdat.systems[ii].transmission.grvmirror_C2
	ECHSys.XLine[ii].alpha0=ecdat.systems[ii].transmission.inclination 
	ECHSys.XLine[ii].beta0=ecdat.systems[ii].transmission.ellipticity 

	; set power in input table
	if ecdat.systems[ii].pinjs[0] gt 0.02 then $
		ECHSys.Tank[ii].Power_MW=ecdat.systems[ii].pinjs[0]/1.e6 $
		else ECHSys.Tank[ii].Power_MW=0.0

	ECHSys.Antenna[ii].Ant_Num=z.AntNum
	ECHSys.Tank[ii].PowerSupply=z.PowerSupply
	
	PrgDat.Status.PlotRays[ii]=	1
	
endfor

PrgDat.Status.ArchivedData=1
PrgDat.Status.RayLength=5.0 ;1.8
PrgDat.Status.OutputsValid=0

success=1

return

end
