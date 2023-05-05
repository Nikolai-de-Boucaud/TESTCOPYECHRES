; Takes gyrotron name as input and enters the relevant data into the inputtable
; structure
; gyro_name is name of gyrotron, at least first 3 letters, case insensitive
; row_num is row number of table

pro enter_ECH_data, evy, empty=empty
; evy is the table row number that contains gyro_name
; set empty to place zero data in line
if not keyword_set(empty) then empty=0

common ECHCOM

; get all other data, completely update the system data
z=echcal(PrgDat.config_shot, ECHSys.Tank[evy].gyroname)

if (z.status and (not empty)) then begin

			nmir=n_elements(z.mir_type)
			ECHSys.Tank[evy].Gyroname=z.gyroname
			ECHSys.Tank[evy].freq=z.gyro.freq
			ECHSys.Tank[evy].Antenna=z.antenna_data.dwgno
			ECHSys.Tank[evy].AntPort=z.antenna_data.port
			ECHSys.Tank[evy].AntNum=z.PortNum
			ECHSys.Tank[evy].TankNo=z.TankNum
			ECHSys.Tank[evy].PowerSupply=z.PowerSupply
			ECHSys.Tank[evy].xfrac=1.
			ECHSys.Tank[evy].nharm=2
			
			ECHSys.InputTable[evy].Gyroname=z.gyroname
			ECHSys.InputTable[evy].f_abs=0.
			ECHSys.InputTable[evy].rho_peak=0.
			ECHSys.InputTable[evy].eccd=0.
			
			ECHSys.Antenna[evy].ant_num=z.AntNum
			ECHSys.Antenna[evy].r_launch=z.antenna_data.r_pivot
			ECHSys.Antenna[evy].z_launch=z.antenna_data.z_pivot
			ECHSys.Antenna[evy].pol_id=z.antenna_data.pol_id
			ECHSys.Antenna[evy].azi_id=z.antenna_data.azi_id
			ECHSys.Antenna[evy].offset_angle=z.antenna_data.offset_angle
			ECHSys.Antenna[evy].antenna_inclinationd=z.antenna_data.antenna_inclinationd
			ECHSys.Antenna[evy].DwgNo=z.antenna_data.DwgNo
			ECHSys.Antenna[evy].divergence=z.antenna_data.divergence
			
			ECHSys.XLine[evy].gammasd[*]=0.
			ECHSys.XLine[evy].gammasd[0:nmir-1]=z.gamma
			ECHSys.XLine[evy].mir_type=0
			ECHSys.XLine[evy].mir_type[0:nmir-1]=z.mir_type
			ECHSys.XLine[evy].gm1=z.gm1
			ECHSys.XLine[evy].gm2=z.gm2
			
;			IF strmatch(ECHSys.InputTable[evy].Gyroname,'TLeia',/FOLD_CASE) OR strmatch(ECHSys.InputTable[evy].Gyroname,'TLuke',/FOLD_CASE) ;THEN BEGIN 
;				ECHSys.InputTable[evy].PolarAng=162
;				ECHSys.InputTable[evy].AziAng=241
;			ENDIF

			IF strcmp(ECHSys.InputTable[evy].Gyroname,'T',1,/FOLD_CASE) THEN BEGIN 
				ECHSys.InputTable[evy].PolarAng=162
				ECHSys.InputTable[evy].AziAng=241
			ENDIF
			
		endif else begin
		
			ECHSys.Tank[evy].Antenna=''
			ECHSys.Tank[evy].AntPort=''
			ECHSys.Tank[evy].AntNum=0
			ECHSys.Tank[evy].TankNo=0
			ECHSys.Tank[evy].PowerSupply=''
			ECHSys.Tank[evy].xfrac=1.
			ECHSys.InputTable[evy].f_abs=0.
			ECHSys.InputTable[evy].rho_peak=0.
			ECHSys.InputTable[evy].eccd=0.
			ECHSys.Antenna[evy].ant_num=0
			ECHSys.Antenna[evy].r_launch=0.
			ECHSys.Antenna[evy].z_launch=0.
			ECHSys.Antenna[evy].pol_id=0.
			ECHSys.Antenna[evy].azi_id=0.
			ECHSys.Antenna[evy].offset_angle=0.
			ECHSys.Antenna[evy].antenna_inclinationd=0.
			ECHSys.Antenna[evy].DwgNo=''
			ECHSys.Antenna[evy].divergence=0.
			ECHSys.XLine[evy].gammasd[*]=0.
			ECHSys.XLine[evy].mir_type[*]=0
			ECHSys.XLine[evy].gm1=fltarr(6)
			ECHSys.XLine[evy].gm2=fltarr(6)
			
			ECHSys.InputTable[evy].xfrac=0.
			ECHSys.InputTable[evy].PolarAng=0
			ECHSys.InputTable[evy].AziAng=0
			ECHSys.InputTable[evy].PolCts=0
			ECHSys.InputTable[evy].TorCts=0
			ECHSys.Antenna[evy].alpha_a=0
			ECHSys.Antenna[evy].beta_a=0

			return
			
		endelse

		get_mir_angles, $	; mirror normals
			ECHSys.Antenna[evy].pol_id*!dtor, $
			ECHSys.Antenna[evy].azi_id*!dtor, $
			ECHSys.InputTable[evy].PolarAng*!dtor, $
			ECHSys.InputTable[evy].AziAng*!dtor, $
			ECHSys.Antenna[evy].offset_angle*!dtor, $
			pol_normal, azi_normal, $ 		; output mirror normals (rad)
			tilt, facet, scan, crank, $ 	; output mirror angles (deg)
			ECHSys.Antenna[evy].antenna_inclinationd

		; and store results
		ECHSys.Antenna[evy].pol_ang=scan
		ECHSys.Antenna[evy].tor_ang=crank
				
		; get scan and crank counts
		Antenna_Counts, PrgDat.config_shot, ECHSys.Antenna[evy].ant_num, $
			scan, crank, $				; input angles (deg)
			scan_cts, crank_cts, $		; outputs
			/antenna_num

		ECHSys.InputTable[evy].PolCts=scan_cts
		ECHSys.InputTable[evy].TorCts=crank_cts
		
		; calculate fractional power in x-mode
		
		; get applied inclination
		NumBends=max(where(ECHSys.XLine[evy].mir_type ne 0))
		mir_type=ECHSys.XLine[evy].mir_type[0:NumBends]
		gamma=ECHSys.XLine[evy].gammasd[0:NumBends]*!dtor
	
		nuu=nu(ECHSys.Antenna[evy].Pol_Ang*!dtor, $		
			ECHSys.Antenna[evy].Tor_Ang*!dtor, $
			ECHSys.Antenna[evy].pol_id*!dtor)
			
		find_pol, ECHSys.XLine[evy].alpha0*!dtor, ECHSys.XLine[evy].beta0*!dtor, $
			ECHSys.InputTable[evy].pol1*!dtor, ECHSys.InputTable[evy].pol2*!dtor, $
			ECHSys.XLine[evy].gm1, ECHSys.XLine[evy].gm2, mir_type, gamma, $
			fltarr(n_elements(gamma)), nuu, alpha_a, beta_a

		ECHSys.Antenna[evy].alpha_a=alpha_a/!dtor
		ECHSys.Antenna[evy].beta_a=beta_a/!dtor
		
		pols=xmfrac(ECHSys.Antenna[evy].r_launch, $
			ECHSys.Antenna[evy].z_launch, $
			ECHSys.InputTable[evy].PolarAng, $
			ECHSys.InputTable[evy].AziAng, $
			ECHSys.Antenna[evy].pol_id, $
			ECHSys.Antenna[evy].azi_id, $
			ECHSys.Antenna[evy].alpha_a, $
			ECHSys.Antenna[evy].beta_a, $
			ECHSys.Tank[evy].freq, $
			gstruct)
		if pols.success then begin
			ECHSys.Antenna[evy].alpha_x=pols.x_inclination
			ECHSys.Antenna[evy].beta_x=pols.x_ellipticity
			ECHSys.InputTable[evy].xfrac=pols.x_frac*100.
		endif

end
