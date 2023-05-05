; enter the data from the setup file into Common blocks
; sdata is structure returned by resolve_setup
pro fill_com, sdata, success=success

Common ECHCOM

success=0

; first, check if inputs are from namelist file
if PrgDat.Prefs.nlinputfile ne '' then begin
	clear_struct, ECHSys
	return
endif

if sdata.shot ge PrgDat.First_Shot_Newest_Config then $
	PrgDat.config_shot=PrgDat.First_Shot_Newest_Config+1 $
	else PrgDat.config_shot=sdata.shot

Clear_Com	; clear the common variables for new ones
NumSystems=sdata.NumberSystems
ECHSys.NumTanks=NumSystems

; get tilt, facet, polar, and azimuthal angles
for ii=0, NumSystems-1 do begin

;	Antenna_Angles, sdata.shot, sdata.AntNumber[ii], tilt, facet, $
	Antenna_Angles, PrgDat.config_shot, sdata.AntNumber[ii], tilt, facet, $
		 sdata.pol_counts[ii], sdata.tor_counts[ii], /antenna_num

	z=echcal(PrgDat.config_shot, sdata.gyrotron[ii], id_type='name')	
	get_reflected_angles, Tilt, Facet, $
		z.antenna_data.pol_id*!dtor, $
		z.antenna_data.azi_id*!dtor, $
		z.antenna_data.offset_angle*!dtor, $
		polar_r, azi_r, $	; outputs
		z.antenna_data.antenna_inclinationd, $
		antenna_style='P2001'	
	ECHSys.Tank[ii].gyroname=z.Gyro.GyroName
	ECHSys.Tank[ii].freq=z.Gyro.freq
	ECHSys.Tank[ii].Antenna=z.Antenna_data.DwgNo
	ECHSys.Tank[ii].TankNo=z.TankNum	; +1
	ECHSys.Tank[ii].AntNum=z.PortNum	; +1
	ECHSys.Tank[ii].AntPort=z.Antenna_data.port
	ECHSys.Tank[ii].PowerSupply=z.PowerSupply
	
	ECHSys.InputTable[ii].gyroname=z.Gyro.GyroName
	ECHSys.InputTable[ii].PolarAng=polar_r/!dtor
	ECHSys.InputTable[ii].AziAng=azi_r/!dtor
	ECHSys.InputTable[ii].pol1=sdata.polarizer1[ii]
	ECHSys.InputTable[ii].pol2=sdata.polarizer2[ii]
	ECHSys.InputTable[ii].PolCts=sdata.pol_counts[ii]
	ECHSys.InputTable[ii].TorCts=sdata.tor_counts[ii]
	
	;ind=where(strupcase(strmid(echsys.inputtable[ii].gyroname,0,3)) eq 'TLE', tmp)
	;IF tmp GT 0 THEN BEGIN
	;ECHSys.InputTable[ii].PolarAng=162	;160.016	;deg, prototype top launch fixed-injection
	;ECHSys.InputTable[ii].AziAng=241	;237.933	;deg
	;ECHSys.InputTable[ii].PolCts=0
	;ECHSys.InputTable[ii].TorCts=0	
	;tilt=-128.52		; the polar angle of the mirror normal, for existing outside launch it ranges ~0-90deg positive (inward)
	;facet=356.1-180.	; this assumes ECHSys.Antenna.TOR_ANG + 180 = Azi_n or Azi_normal (the mirror normal, 356.1degree from David su's measurements in file ECCD Mirror Normal Geometry - As installed 2019 3 28 (2).pdf)
	;Print, 'hardcoded mirror normal for 300deg top launch (deg):', tilt, facet
	;Print, 'hardcoded launch angle for 300deg top launch (deg):', ECHSys.InputTable[ii].PolarAng, ECHSys.InputTable[ii].AziAng
	;ENDIF

	IF z.antennaname EQ 'Top300' THEN BEGIN
	ECHSys.InputTable[ii].PolarAng=162.	;deg, prototype top launch fixed-injection
	ECHSys.InputTable[ii].AziAng=241.	;deg
	ECHSys.InputTable[ii].PolCts=0
	ECHSys.InputTable[ii].TorCts=0	
	tilt=-147.3	        ; vertical 34.75 degree from Ian's estimate sent on Feb 1 2022
	facet=344.6-180.	; toroidal 315.36 degree from Ian's estimate sent on Feb 1 2022, 360-(315.36-300)=344.6deg)	
;	Print, 'hardcoded mirror normal for 300deg top launch (deg):', tilt, facet
;	Print, 'hardcoded launch angle for 300deg top launch (deg):', ECHSys.InputTable[ii].PolarAng, ECHSys.InputTable[ii].AziAng
	ENDIF


	IF z.antennaname EQ 'Top90' THEN BEGIN
	ECHSys.InputTable[ii].PolarAng=162.	;deg, prototype top launch fixed-injection
	ECHSys.InputTable[ii].AziAng=241.	;deg
	ECHSys.InputTable[ii].PolCts=0
	ECHSys.InputTable[ii].TorCts=0	
	tilt=-147.6		; the polar angle of the mirror normal (outwards for top launch), for existing outside launch it ranges ~0-90deg positive (inward)
	facet=326.8-180.	; this assumes ECHSys.Antenna.TOR_ANG + 180 = Azi_n or Azi_normal (the mirror normal, 326.8degree from Ian Holmes's estimate sent on Feb 24 2022)
;	Print, 'hardcoded mirror normal for 90deg top launch (deg):', tilt, facet
;	Print, 'hardcoded launch angle for 90deg top launch (deg):', ECHSys.InputTable[ii].PolarAng, ECHSys.InputTable[ii].AziAng	
	ENDIF


;	ind=where(strupcase(strmid(echsys.inputtable[ii].gyroname,0,3)) eq 'TLU', tmp)
;	IF tmp GT 0 THEN BEGIN
;	ECHSys.InputTable[ii].PolarAng=162	;deg, prototype top launch fixed-injection
;	ECHSys.InputTable[ii].AziAng=241.	;deg
;	ECHSys.InputTable[ii].PolCts=0
;	ECHSys.InputTable[ii].TorCts=0	
;	tilt=-147.6		; the polar angle of the mirror normal (outwards for top launch), for existing outside launch it ranges ~0-90deg positive (inward)
;	facet=326.8-180.	; this assumes ECHSys.Antenna.TOR_ANG + 180 = Azi_n or Azi_normal (the mirror normal, 326.8degree from Ian Holmes's estimate sent on Feb 24 2022)
;	Print, 'hardcoded mirror normal for 90deg top launch (deg):', tilt, facet
;	Print, 'hardcoded launch angle for 90deg top launch (deg):', ECHSys.InputTable[ii].PolarAng, ECHSys.InputTable[ii].AziAng
;	ENDIF
	
	ECHSys.Antenna[ii].Ant_Num=z.AntNum
	ECHSys.Antenna[ii].Tor_Ang=Facet
	ECHSys.Antenna[ii].Pol_Ang=Tilt
	ECHSys.Antenna[ii].z_launch=z.Antenna_data.z_pivot
	ECHSys.Antenna[ii].r_launch=z.Antenna_data.r_pivot
	ECHSys.Antenna[ii].pol_id=z.Antenna_data.pol_id
	ECHSys.Antenna[ii].azi_id=z.Antenna_data.azi_id
	ECHSys.Antenna[ii].offset_angle=z.Antenna_data.offset_angle
	ECHSys.Antenna[ii].DwgNo=z.Antenna_data.DwgNo
	ECHSys.Antenna[ii].antenna_inclinationd=z.Antenna_data.antenna_inclinationd
	ECHSys.Antenna[ii].divergence=z.Antenna_data.divergence
	ECHSys.XLine[ii].alpha0=z.Gyro.PolInc
	ECHSys.XLine[ii].beta0=z.Gyro.PolEll
	ECHSys.XLine[ii].gm1=z.gm1
	ECHSys.XLine[ii].gm2=z.gm2
	ECHSys.XLine[ii].mir_type=z.mir_type
	ECHSys.XLine[ii].gammasd=z.gamma

	ib=where(ECHSys.XLine[ii].mir_type NE 0, NumBends)
	if NumBends gt 1 then begin
		mir_type=ECHSys.XLine[ii].mir_type[0:NumBends-1]
		gamma=ECHSys.XLine[ii].gammasd[0:NumBends-1]*!dtor
		nuu=nu(ECHSys.Antenna[ii].Pol_Ang*!dtor, $		
			ECHSys.Antenna[ii].Tor_Ang*!dtor, $
			ECHSys.Antenna[ii].pol_id*!dtor)
		find_pol, ECHSys.XLine[ii].alpha0*!dtor, ECHSys.XLine[ii].beta0*!dtor, $
			ECHSys.InputTable[ii].pol1*!dtor, ECHSys.InputTable[ii].pol2*!dtor, $
			ECHSys.XLine[ii].gm1, ECHSys.XLine[ii].gm2, $
			mir_type, gamma, fltarr(n_elements(gamma)), nuu, alpha_a, beta_a	
		ECHSys.Antenna[ii].alpha_a=alpha_a/!dtor
		ECHSys.Antenna[ii].beta_a=beta_a/!dtor
	
		pols=xmfrac( $
			ECHSys.Antenna[ii].r_launch, $
			ECHSys.Antenna[ii].z_launch, $
			ECHSys.InputTable[ii].PolarAng, $
			ECHSys.InputTable[ii].AziAng, $
			ECHSys.Antenna[ii].pol_id, $
			ECHSys.Antenna[ii].azi_id, $
			ECHSys.Antenna[ii].alpha_a, $
			ECHSys.Antenna[ii].beta_a, $
			ECHSys.Tank[ii].freq, $
			gstruct)
		if pols.success then begin
			ECHSys.Antenna[ii].alpha_x=pols.x_inclination
			ECHSys.Antenna[ii].beta_x=pols.x_ellipticity
			ECHSys.InputTable[ii].xfrac=pols.x_frac*100.
		endif

	endif

endfor

uniq_freq=ECHSys.Tank[uniq(ECHSys.Tank.freq, sort(ECHSys.Tank.freq))].freq
k=where(uniq_freq ne 0., count)
if count gt 1 then uniq_freq=uniq_freq[k]
PrgDat.n_freq=n_elements(uniq_freq)
PrgDat.unique_freq[*]=0.
PrgDat.unique_freq[0:PrgDat.n_freq-1]=uniq_freq
PrgDat.Status.ArchivedData=1
PrgDat.Status.RayLength=5.0		; maximum ray length, in m
for ii=0,NumSystems-1 do PrgDat.Status.PlotRays[ii]=1

success=1
return
end

