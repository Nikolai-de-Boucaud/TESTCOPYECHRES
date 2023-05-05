; get new data for ECHSys when PrgDat.config_shot is new

pro update_echsys, shot, gyro

common ECHCOM

z=echcal(shot, gyro)

evy=where(strupcase(ECHSys.Tank.gyroname) eq strupcase(gyro))
if evy[0] lt 0 then begin
	print, '  !!! No gyrotron '+gyro+' in the systems list'
	;goto, cleanup
endif
evy=evy[0]
if z.status then begin
	ECHSys.Tank[evy].Antenna=z.antenna_data.dwgno
	ECHSys.Tank[evy].AntPort=z.antenna_data.port
	ECHSys.Tank[evy].AntNum=z.PortNum
	ECHSys.Tank[evy].TankNo=z.TankNum
	ECHSys.Tank[evy].PowerSupply=z.PowerSupply
	ECHSys.Tank[evy].xfrac=1.
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
	ECHSys.XLine[evy].gammasd=z.gamma
	ECHSys.XLine[evy].mir_type=z.mir_type
	ECHSys.XLine[evy].gm1=z.gm1
	ECHSys.XLine[evy].gm2=z.gm2
endif else begin
	print, '  !!! Gyrotron '+gyro+' not found for this shot;'+ $
		' some input data may be incorrect.'
endelse
;	cleanup:
;	ECHSys.Tank[evy].Antenna=''
;	ECHSys.Tank[evy].AntPort=''
;	ECHSys.Tank[evy].AntNum=0
;	ECHSys.Tank[evy].TankNo=0
;	ECHSys.Tank[evy].PowerSupply=''
;	ECHSys.Tank[evy].xfrac=1.
;	ECHSys.InputTable[evy].f_abs=0.
;	ECHSys.InputTable[evy].rho_peak=0.
;	ECHSys.InputTable[evy].eccd=0.
;	ECHSys.Antenna[evy].ant_num=0
;	ECHSys.Antenna[evy].r_launch=0.
;	ECHSys.Antenna[evy].z_launch=0.
;	ECHSys.Antenna[evy].pol_id=0.
;	ECHSys.Antenna[evy].azi_id=0.
;	ECHSys.Antenna[evy].offset_angle=0.
;	ECHSys.Antenna[evy].antenna_inclinationd=0.
;	ECHSys.Antenna[evy].DwgNo=''
;	ECHSys.Antenna[evy].divergence=0.
;	ECHSys.XLine[evy].gammasd[*]=0.
;	ECHSys.XLine[evy].mir_type[*]=0
;	ECHSys.XLine[evy].gm1=fltarr(6)
;	ECHSys.XLine[evy].gm2=fltarr(6)
;endelse

return

end
