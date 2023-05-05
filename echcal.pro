function echcal, shot, id, id_type=id_type, $
	present_port_limits=present_port_limits
;  if id_type = 'name' then id is gyrotron name (default)
;             = 'antenna' then id is antenna_number (0,1,...)
;             = 'tank' then id is tank number (1,2,...)
;             = 'gyro' then id is gyrotron number (0,1,...)
; if id_type not set, then id_type='name' -- gyrotron name

; returns a structure:
; status:		1 for success, 0 for failure
; First_Shot_Newest_Config: 
; PowerSupply:
; GyroNum:		gyrotron number in array below 0,1,...
; PortNum:		port number 1,2,...
; AntNum:		antenna number in array below  0,1,...
; TankNum:		tank (system) number 1,2,...
; GyroName: 	string with gyrotron name, eg, 'katya' or 'LION'
; Gyro_Prefix:	string containing, eg, 'KAT' or 'LIO'
; Gyro_Prefixes:string array containing all gyro_prefix values
; scan_coef:	cubic coefficients for scan (poloidal) steering mirror angle 
; crank_coef:	same for crank (toroidal) 
; antenna_data: structure with info on antenna
; gyro: 		structure with info on gyrotron
; gm1:			grooved mirror 1 fourier constants
; gm2:			grooved mirror 2 fourier constants
; mir_type:		mirror type (1=1st pol, 2=2nd pol, 4=plane)
; gamma:		waveguide joint angles
; np_lim:		number points in the port limit
; facet_limits: toroidal steering boundary (counts)
; tilt_limits:	poloidal steering boundary (counts)

if not keyword_set(id_type) then id_type='name'
id_type=strlowcase(strtrim(id_type,2))

; set present keyword to get present status
if not keyword_set(present_port_limits) then present_port_limits=0 

; if no arguments, then return global data


;************* global parameters ************************
compile_date='October 31, 2022'
NumberOfTanks=15        ; 14 ; to make the array work in the following though there might not be that many tanks physically
NumberOfAntennas=16     ; 16 ; total number of antennas
NumberOfGyros=19	; 18 ; total number of gyrotrons
NumberOfPorts=10	;
MaxNumberOfTanks=8	; number of tanks in ECH systems table (GUI upper table)
MaxNumMiters=20 	; array size for miter bend data
Max_Number_Raypts=1201	; max size of toray ray arrays
First_Shot_Newest_Config=192589L	; 

;****************** gyrotron data *********************************
; PolInc and PolEll are the polarization inclination and ellipticity
;   measured at the output of the gyrotron
GyroData={GyroData, GyroName:'', GyroSN:'', freq:0.0, PolInc:0.0, PolEll:0.0}
Gyro=replicate(GyroData, NumberOfGyros)

Gyro[0]={GyroData, 'Katya',    'Gycom 1',     	110.0, -86.0, 1.14576}
Gyro[1]={GyroData, 'Dorothy',  'CPI Dev1',      110.0, 90.0, 0.0}
Gyro[2]={GyroData, 'Toto',     'CPI Dev2',      110.0, 90.0, 0.0}
Gyro[3]={GyroData, 'Scarecrow','CPI Prod1',     110.0, 90.0, 0.0}
Gyro[4]={GyroData, 'Boris',    'TdeV1',         110.0, 90.0, 0.0}
Gyro[5]={GyroData, 'Natasha',  'TdeV2',         110.0, 90.0, 0.0}
Gyro[6]={GyroData, 'Tinman',   'CPI Prod2',     110.0, 90.0, 0.0}
Gyro[7]={GyroData, 'Lion',     'CPI Prod3',     110.0, 90.0, 0.0}
Gyro[8]={GyroData, 'Luke',	   'CPI Prod4',		110.0, 90.0, 0.0}
Gyro[9]={GyroData, 'Han',	   'CPI Prod5',	  	110.0, 90.0, 0.0};After shot 192589, this is technically HanR (refurbished) instead of Han
Gyro[10]={GyroData,'Leia',	   'CPI Prod6',	  	110.0, 90.0, 0.0}
Gyro[11]={GyroData,'Chewbacca','CPI DepColl',	110.0, 90.0, 0.0}
Gyro[12]={GyroData,'Vader',    'CPI 117.5',  	117.5, 90.0, 0.0}
Gyro[13]={GyroData,'Yoda',     'CPI 1.5 MW 2',	110, 90.0, 0.0}
Gyro[14]={GyroData,'R2D2',     'CPI 110', 	110, 90.0, 0.0}
Gyro[15]={GyroData,'NASA',     'CPI NASA',   	110.0, 90.0, 0.0}
Gyro[16]={GyroData,'TLeia',    'CPI Prod6',  	110.0, 90.0, 0.0}  ;Leia for top launch
Gyro[17]={GyroData,'TLuke',    'CPI Prod6',  	110.0, 90.0, 0.0}  ;Luke for top launch
Gyro[18]={GyroData,'THAN',     'CPI Prod5',     110.0, 90.0, 0.0}  ;After shot 192589, technically HanR, for top launch

gyro_prefixes=strmid(strupcase(Gyro.GyroName),0,3) 

if n_params() eq 0 then return, {$
NumberOfTanks:NumberOfTanks, $			
NumberOfAntennas:NumberOfAntennas, $	
NumberOfGyros:NumberOfGyros, $
NumberOfPorts:NumberOfPorts, $
MaxNumberOfTanks:MaxNumberOfTanks, $	
MaxNumMiters:MaxNumMiters, $	
Max_Number_Raypts:Max_Number_Raypts, $	; max number points allowed for toray
First_Shot_Newest_Config:First_Shot_Newest_Config, $
Gyro_prefixes:gyro_prefixes, $
compile_date:compile_date}	; last change in calibrations

;********************************************************************
;********************************************************************
; Below are data regarding the ECH transmission system which rarely change
; All angles are in degrees and all lengths in m
; Data are arranged by shot range due to periodic recalibrations
	
;************* antenna data *****************************
; Data for each antenna
; Z_piv and R_piv are the Z and R of the antenna pivot
; The ray incident on the steering mirror makes polar and azimuthal angles 
;	pol_id and azi_id, in deg. Its closes approach to the mirror pivot is
;   s_ant.  d_ant is the distance from the pivot to the mirror surface.
;   DwgNo refers to the GA drawing number for the antenna.
;	antenna_inclinationd is the angle from horizontal by which the waveguides
;		are tilted, upward being positive. Only used for the (tilt,facet) to 
;		(scan,crank) conversion; therefore, only relevant to P2001 launcher.
Antenna={AntennaData, port:'', port_num:0, z_pivot:0.0, r_pivot:0.0, d_ant:0.0, $
	s_ant:0.0, pol_id:0.0, azi_id:0.0, offset_angle:0.0, DwgNo:'', $
	antenna_inclinationd:0.0, fixed_facet:0.0, divergence:0.0}
Antenna_data=Replicate({AntennaData}, NumberOfAntennas)	; data about each antenna

; Parameters characterizing each antenna:
Antenna_data[0]={AntennaData, '', -1, 0.701, 2.407, 0.0176, 0.0139, $
	12.4, 180., -1.4,'GA1_M1', 0.0, 19.0, 1.7}
Antenna_data[1]={AntennaData, '', -1, 0.701, 2.407, 0.0176, 0.0139, $
	12.4,  180., 1.4, 'GA1_M2', 0.0, 19.0, 1.7}
Antenna_data[2]={AntennaData, '', -1, 0.6961, 2.394, 0.0, 0.0, $
	12.4, 180., -1.4, 'P1999_M1', 0.0, 0.0, 1.7}
Antenna_data[3]={AntennaData, '', -1, 0.6961, 2.394, 0.0, 0.0, $
	12.4,  180., 1.4, 'P1999_M2', 0.0, 0.0, 1.7}
Antenna_data[4]={AntennaData, '', -1, 0.701, 2.407, 0.0176, 0.0139, $
	12.4, 180., -1.4, 'GA2_M1', 0.0, 0.0, 1.7}
Antenna_data[5]={AntennaData, '', -1, 0.701, 2.407, 0.0176, 0.0139, $
	12.4,  180., 1.4, 'GA2_M2', 0.0, 0.0, 1.7}
Antenna_data[6]={AntennaData, '', -1, 0.6794, 2.3999, 0.0, 0.0, $
	12.2, 179.3, -1.4, 'P2001_M1', 22.3, 0.0, 1.7}
Antenna_data[7]={AntennaData, '', -1, 0.6794, 2.3999, 0.0, 0.0, $
	12.55, 179.9, 1.4, 'P2001_M2', 22.3, 0.0, 1.7}
Antenna_data[8]={AntennaData, '', -1, 0.6794, 2.3999, 0.0, 0.0, $
	12.13, 181.4, -1.4, 'P2002_M1', 22.3, 0.0, 1.7}
Antenna_data[9]={AntennaData, '', -1, 0.6794, 2.3999, 0.0, 0.0, $
	11.78, 181.5,  1.4, 'P2002_M2', 22.3, 0.0, 1.7}
Antenna_data[10]={AntennaData, '', -1, 0.6794, 2.3999, 0.0, 0.0, $
	12.6, 180.0,  -1.4, 'P2006_M1', 22.3, 0.0, 1.7}
Antenna_data[11]={AntennaData, '', -1, 0.6794, 2.3999, 0.0, 0.0, $
	12.6, 180.0,  1.4, 'P2006_M2', 22.3, 0.0, 1.7}
Antenna_data[12]={AntennaData, '', -1, 0.6794, 2.3999, 0.0, 0.0, $
	12.6, 180.0,  -1.4, 'P2012_M1', 22.2, 0.0, 1.7}
Antenna_data[13]={AntennaData, '', -1, 0.6794, 2.3999, 0.0, 0.0, $
	12.6, 180.0,  1.4, 'P2012_M2', 22.2, 0.0, 1.7}
;;;top launch at 300R+2;;;change d_ant from 0.07m=7cm to 0 does not change the polarization or ECCD for this case
;;;both Top300 and Top90 have mirror center at z=1.2m and R=1.572m
;;;but the focal point (110GHz, based on John Doane's design) is used as the launch location 
Antenna_data[14]={AntennaData, '', -1, 1.131, 1.561, 0.00, 0.0, $
	114.0, 7.15,  0, 'Top300', 0, 0.0, 2.3}
Antenna_data[15]={AntennaData, '', -1, 1.131, 1.561, 0.00, 0.0, $
	114.13, 0,  0, 'Top90', 0, 0.0, 2.3}

; R,Z data for mirror pivots
if (shot gt 108000) then begin	; better determinations of R and Z (m)
	Antenna_data[0:1].r_pivot=2.4099	; RP, 1/8/2002
	Antenna_data[0:1].z_pivot=0.7015
	Antenna_data[2:3].r_pivot=2.3973
	Antenna_data[2:3].z_pivot=0.6942
	Antenna_data[4:5].r_pivot=2.4099
	Antenna_data[4:5].z_pivot=0.7015
endif

; angles for incident rays
; installed new mirrors 11/2000
if (shot GT 104400) then begin	
	Antenna_data[0].pol_id=15.38
	Antenna_data[0].azi_id=180.3
	Antenna_data[1].pol_id=15.25
	Antenna_data[1].azi_id=185.0
endif

; changed fixed facet angles on GA1 antenna before 2001 campaign
if (shot gt 106000) then begin
	Antenna_data[0].fixed_facet=11.0
	Antenna_data[1].fixed_facet=13.0
endif

; calibrated on 30May2001
if (shot gt 107285) then begin	
	Antenna_data[4].pol_id=11.15
	Antenna_data[4].azi_id=182.5
	Antenna_data[5].pol_id=10.3
	Antenna_data[5].azi_id=177.5
	Antenna_data[4].fixed_facet=11.0
	Antenna_data[5].fixed_facet=13.0
endif

; recalibrated in 11/2001
if (shot gt 108000) then begin	
	Antenna_data[0].pol_id=15.31	; GA1
	Antenna_data[0].azi_id=179.22
	Antenna_data[1].pol_id=14.94
	Antenna_data[1].azi_id=181.67
	Antenna_data[2].pol_id=12.20	; P1999
	Antenna_data[2].azi_id=181.8
	Antenna_data[3].pol_id=12.50
	Antenna_data[3].azi_id=188.5
	Antenna_data[6].pol_id=12.20	; P2001
	Antenna_data[6].azi_id=179.3
	Antenna_data[7].pol_id=12.55
	Antenna_data[7].azi_id=179.9
	Antenna_data[0].fixed_facet=10.62
	Antenna_data[1].fixed_facet=12.32
endif

; recalibrated in 11/2002
if shot gt 111682 then begin
	Antenna_data[2].pol_id=12.12
	Antenna_data[2].azi_id=178.8
	Antenna_data[3].pol_id=12.30
	Antenna_data[3].azi_id=183.3
	Antenna_data[6].pol_id=13.20	; fit to laser/centerpost data
	Antenna_data[6].azi_id=178.3	; since no laser/screen data available
	Antenna_data[7].pol_id=11.90
	Antenna_data[7].azi_id=181.0
	Antenna_data[8].pol_id=12.13
	Antenna_data[8].azi_id=181.4
	Antenna_data[9].pol_id=11.78
	Antenna_data[9].azi_id=181.5
endif

; recalibrated in March 2006
if shot gt 123575 then begin
    Antenna_data[6].pol_id=12.4500
    Antenna_data[6].azi_id=172.000
	Antenna_data[7].pol_id=12.1
	Antenna_data[7].azi_id=168.0
	Antenna_data[8].pol_id=13.35
	Antenna_data[8].azi_id=178.5
	Antenna_data[9].pol_id=13.7
	Antenna_data[9].azi_id=180.5
	Antenna_data[10].pol_id=12.85
	Antenna_data[10].azi_id=171.5
	Antenna_data[11].pol_id=14.4
	Antenna_data[11].azi_id=180.5
endif

; recalibrated in November 2006
if shot gt 127586 then begin
    Antenna_data[6].pol_id=12.4500
    Antenna_data[6].azi_id=179.80
	Antenna_data[7].pol_id=12.75
	Antenna_data[7].azi_id=178.5
	Antenna_data[8].pol_id=13.65
	Antenna_data[8].azi_id=181.2
	Antenna_data[9].pol_id=13.7
	Antenna_data[9].azi_id=179.9
	Antenna_data[10].pol_id=12.325
	Antenna_data[10].azi_id=171.650
	Antenna_data[11].pol_id=12.6
	Antenna_data[11].azi_id=180.8
endif

; recalibrated 9Oct2007
if shot gt 130216 then begin
    Antenna_data[6].pol_id=13.0
    Antenna_data[6].azi_id=178.0
	Antenna_data[7].pol_id=12.5
	Antenna_data[7].azi_id=182.5
	Antenna_data[8].pol_id=13.0
	Antenna_data[8].azi_id=176.5
	Antenna_data[9].pol_id=10.6
	Antenna_data[9].azi_id=179.0
	Antenna_data[10].pol_id=10.0
	Antenna_data[10].azi_id=178.0
	Antenna_data[11].pol_id=11.75
	Antenna_data[11].azi_id=178.0
endif

; recalibration 22-23 November 2008
if shot gt 134790L then begin
    Antenna_data[6].pol_id=13.6
    Antenna_data[6].azi_id=183.2
	Antenna_data[7].pol_id=11.9
	Antenna_data[7].azi_id=182.6
	Antenna_data[8].pol_id=12.6
	Antenna_data[8].azi_id=182.0
	Antenna_data[9].pol_id=11.3
	Antenna_data[9].azi_id=175.0
	Antenna_data[10].pol_id=12.1
	Antenna_data[10].azi_id=179.4
	Antenna_data[11].pol_id=13.3
	Antenna_data[11].azi_id=177.2
endif

; The focus mirror on P2001_M1 was replaced in situ 16 September 2009
if shot gt 139102 then begin
	Antenna_data[6].pol_id=13.1
	Antenna_data[6].azi_id=173.3
endif

; recalibration 22-26 February 2011
if shot gt 143020 then begin
    Antenna_data[6].pol_id=13.5
    Antenna_data[6].azi_id=178.5
	Antenna_data[7].pol_id=13.375
	Antenna_data[7].azi_id=181.0
	Antenna_data[8].pol_id=12.5
	Antenna_data[8].azi_id=184.0
	Antenna_data[9].pol_id=12.0
	Antenna_data[9].azi_id=181.0
	Antenna_data[10].pol_id=13.625
	Antenna_data[10].azi_id=178.0
	Antenna_data[11].pol_id=12.8
	Antenna_data[11].azi_id=175.3
endif

; recalibrations in February 2012
if shot gt 147778 then begin
	Antenna_data[6].pol_id=13.75
	Antenna_data[6].azi_id=180.8
	Antenna_data[7].pol_id=13.0
	Antenna_data[7].azi_id=180.6
	Antenna_data[8].pol_id=12.5
	Antenna_data[8].azi_id=176.6
	Antenna_data[9].pol_id=12.15
	Antenna_data[9].azi_id=183.8
	Antenna_data[10].pol_id=12.95
	Antenna_data[10].azi_id=182.0
	Antenna_data[11].pol_id=13.1
	Antenna_data[11].azi_id=178.6
	Antenna_data[12].pol_id=12.45
	Antenna_data[12].azi_id=183.8
endif

; recalibration in May 2015
if shot gt 161700 then begin
	Antenna_data[6].pol_id=13.2
	Antenna_data[6].azi_id=179.2
	Antenna_data[7].pol_id=12.9
	Antenna_data[7].azi_id=175.6
	Antenna_data[8].pol_id=12.15
	Antenna_data[8].azi_id=177.8
	Antenna_data[9].pol_id=11.85
	Antenna_data[9].azi_id=178.2
	Antenna_data[10].pol_id=11.35
	Antenna_data[10].azi_id=180.4
	Antenna_data[11].pol_id=11.4
	Antenna_data[11].azi_id=180.4
	Antenna_data[12].pol_id=12.05
	Antenna_data[12].azi_id=180.6
	Antenna_data[13].pol_id=12.5
	Antenna_data[13].azi_id=187.0	
endif

; recalibration Oct 2016
if shot gt 168000 then begin
	Antenna_data[6].pol_id=13.4
	Antenna_data[6].azi_id=180.4
	Antenna_data[7].pol_id=12.85
	Antenna_data[7].azi_id=175.4
	Antenna_data[8].pol_id=13.5
	Antenna_data[8].azi_id=176.4
	Antenna_data[9].pol_id=13.3
	Antenna_data[9].azi_id=180
	Antenna_data[10].pol_id=11.35
	Antenna_data[10].azi_id=180.4
	Antenna_data[11].pol_id=13.8
	Antenna_data[11].azi_id=183.5
	Antenna_data[12].pol_id=12.85
	Antenna_data[12].azi_id=180.0
	Antenna_data[13].pol_id=12.5
	Antenna_data[13].azi_id=187.0	
endif


; add top launch launcher, June 2019
if shot gt 178730L then begin
	Antenna_data[6].pol_id=13.4
	Antenna_data[6].azi_id=180.4
	Antenna_data[7].pol_id=12.85
	Antenna_data[7].azi_id=175.4
	Antenna_data[8].pol_id=13.5
	Antenna_data[8].azi_id=176.4
	Antenna_data[9].pol_id=13.3
	Antenna_data[9].azi_id=180
	Antenna_data[10].pol_id=11.35
	Antenna_data[10].azi_id=180.4
	Antenna_data[11].pol_id=13.8
	Antenna_data[11].azi_id=183.5
	Antenna_data[12].pol_id=12.85
	Antenna_data[12].azi_id=180.0
	Antenna_data[13].pol_id=12.5
	Antenna_data[13].azi_id=187.0	
	Antenna_data[14].pol_id=180.-66.0    ; for 300deg top launch
	Antenna_data[14].azi_id=7.15
endif


; add top launch launcher, June 2019
if shot gt 187865L then begin
	Antenna_data[6].pol_id=13.4
	Antenna_data[6].azi_id=180.4
	Antenna_data[7].pol_id=12.85
	Antenna_data[7].azi_id=175.4
	Antenna_data[8].pol_id=13.5
	Antenna_data[8].azi_id=176.4
	Antenna_data[9].pol_id=13.3
	Antenna_data[9].azi_id=180
	Antenna_data[10].pol_id=11.35
	Antenna_data[10].azi_id=180.4
	Antenna_data[11].pol_id=13.8
	Antenna_data[11].azi_id=183.5
	Antenna_data[12].pol_id=12.85
	Antenna_data[12].azi_id=180.0
	Antenna_data[13].pol_id=12.5
	Antenna_data[13].azi_id=187.0	
	Antenna_data[14].pol_id=114.    ; for 300deg top launch
	Antenna_data[14].azi_id=7.15
	Antenna_data[15].pol_id=114.13    ; for 90deg top launch
	Antenna_data[15].azi_id=0	; Ian and Yuri have confirmed there is no toroidal component for 90deg last waveguide
endif

; antenna angle/encoder calibration constants
; angle = coef[0] + coef[1]*counts + coef[2]*counts^2 + coef[3]*counts^3
scan_coef=fltarr(NumberOfAntennas,4)
crank_coef=fltarr(NumberOfAntennas,4)
case 1 of
	(shot le 123575): begin
		scan_coef[6,*]=[-117.023, 0.0363287, -2.66806e-06, 9.79287e-11]
		crank_coef[6,*]=[-40.6720, 0.00597997, -1.46940e-07, 6.55138e-12]
		scan_coef[7,*]=[-86.4538, 0.0302116, -2.42632e-06, 1.08048e-10]
		crank_coef[7,*]=[-42.3306, 0.00615450, -1.57566e-07, 6.65702e-12]
		scan_coef[8,*]=[-99.7201, 0.0320952, -2.38234e-06, 9.49135e-11]
		crank_coef[8,*]=[-39.8277, 0.00603236, -1.50411e-07, 6.52550e-12]
		scan_coef[9,*]=[-59.6221, 0.0262853, -2.05883e-06, 1.01251e-10]
		crank_coef[9,*]=[-40.4407, 0.00610274, -1.53842e-07, 6.72505e-12]
	end
	((shot gt 123575) and (shot lt 127586)): begin
		scan_coef[6,*]=[-117.023, 0.0363287, -2.66806e-06, 9.79287e-11]
		crank_coef[6,*]=[-42.7055, 0.00599594, -1.49261e-07, 6.65561e-12]
		scan_coef[7,*]=[-86.4538, 0.0302116, -2.42632e-06, 1.08048e-10]
		crank_coef[7,*]=[-45.0128, 0.00617061, -1.59917e-07, 6.76179e-12]
		scan_coef[8,*]=[-99.7201, 0.0320952, -2.38234e-06, 9.49135e-11]
		crank_coef[8,*]=[-40.9214, 0.00603057, -1.50281e-07, 6.52451e-12]
		scan_coef[9,*]=[-59.6221, 0.0262853, -2.05883e-06, 1.01251e-10]
		crank_coef[9,*]=[-40.4407, 0.00610274, -1.53842e-07, 6.72505e-12]
		scan_coef[10,*]=[-169.773, 0.0427135, -3.06106e-06, 1.02495e-10]
		crank_coef[10,*]=[-56.4383, 0.00684362, -1.97443e-07, 6.82671e-12]
		scan_coef[11,*]=[16.3627, 0.0147048, -8.52979e-07, 9.57169e-11]
		crank_coef[11,*]=[33.1422, -0.00567169, 1.23229e-07, -6.64673e-12]
	end
	((shot ge 127586) and (shot lt 130216)): begin
		scan_coef[6,*]=[-117.023, 0.0363287, -2.66806e-06, 9.79287e-11]
		crank_coef[6,*]=[-41.0806, 0.00598453, -1.47646e-07, 6.58393e-12]
		scan_coef[7,*]=[-110.002, 0.0353983, -2.66144e-06, 1.03248e-10]
		crank_coef[7,*]=[-43.2520, 0.00615411, -1.57647e-07, 6.66523e-12]
		scan_coef[8,*]=[-98.1431, 0.0319894, -2.45009e-06, 1.01467e-10]
		crank_coef[8,*]=[-40.2043, 0.00602854, -1.49892e-07, 6.50447e-12]
		scan_coef[9,*]=[-61.6058, 0.0267257, -2.07925e-06, 1.00859e-10]
		crank_coef[9,*]=[-41.1335, 0.00610701, -1.54538e-07, 6.75842e-12]
		scan_coef[10,*]=[-174.244, 0.0439652, -3.18649e-06, 1.06615e-10]
		crank_coef[10,*]=[-56.4389, 0.00684373, -1.97442e-07, 6.82615e-12]
		scan_coef[11,*]=[13.9695, 0.0157966, -9.85287e-07, 9.69709e-11]
		crank_coef[11,*]=[-32.7405, 0.00567404, -1.23512e-07, 6.65621e-12]
	end
	((shot ge 130216) and (shot lt 143020)): begin
		scan_coef[6,*]=[-43.3054, 0.0112724, 1.17633e-07, -3.25501e-12]
		crank_coef[6,*]=[-39.2787, 0.00488447, 1.01282e-08, 2.66140e-15]
		scan_coef[7,*]=[-41.1323, 0.0117596, 2.98265e-08, 1.18633e-12]
		crank_coef[7,*]=[-40.8736, 0.00505133, 1.88529e-09, 6.55107e-15]
		scan_coef[8,*]=[-44.5386, 0.0117101, 7.77441e-08, -2.72038e-12]
		crank_coef[8,*]=[-40.2043, 0.00602854, -1.49892e-07, 6.50447e-12]
		scan_coef[9,*]=[-18.6713, 0.00927638, 2.48061e-07, -1.85739e-12]
		crank_coef[9,*]=[-41.1335, 0.00610701, -1.54538e-07, 6.75842e-12]
		scan_coef[10,*]=[-63.5631, 0.0114018, -1.49719e-08, 4.16066e-12]
		crank_coef[10,*]=[-56.4389, 0.00684373, -1.97442e-07, 6.82615e-12]
		scan_coef[11,*]=[19.1890, 0.0114235, 1.84819e-07, -4.13311e-12]
		crank_coef[11,*]=[-32.7405, 0.00567404, -1.23512e-07, 6.65621e-12]
	end
	((shot ge 143020) and (shot lt 147778)): begin
		scan_coef[6,*]=[-44.5343, 0.0123743, -4.39881e-08, 3.41327e-12]
		crank_coef[6,*]=[-31.2249, 0.00590110, -1.91947e-07, 1.05548e-11]
		scan_coef[7,*]=[-39.9525, 0.0115992, 5.91303e-08, -1.12841e-12]
		crank_coef[7,*]=[-39.2041, 0.00600184, -1.41691e-07, 6.11162e-12]
		scan_coef[8,*]=[-43.4404, 0.0119333, -1.84404e-08, 3.84689e-12]
		crank_coef[8,*]=[-36.7147, 0.00640224, -2.48667e-07, 1.23399e-11]
		scan_coef[9,*]=[-25.4927, 0.0111929, 5.53309e-08, 2.99862e-12]
		crank_coef[9,*]=[-38.5203, 0.00570008, -9.66534e-08, 4.11774e-12]
		scan_coef[10,*]=[-69.4507, 0.0123205, 7.69257e-09, -1.57182e-13]
		crank_coef[10,*]=[-39.2763, 0.00635517, -2.09386e-07, 9.50505e-12]
		scan_coef[11,*]=[19.2273, 0.0123219, 1.96289e-08, 7.45654e-13]
		crank_coef[11,*]=[-38.3065, 0.00599672, -1.52217e-07, 7.23657e-12]
	end
	((shot ge 147778) and (shot lt 156610)): begin
		scan_coef[6,*]=[-44.5343, 0.0123743, -4.39881e-08, 3.41327e-12]
		crank_coef[6,*]=[-31.2249, 0.00590110, -1.91947e-07, 1.05548e-11]
		scan_coef[7,*]=[-39.9525, 0.0115992, 5.91303e-08, -1.12841e-12]
		crank_coef[7,*]=[-39.2041, 0.00600184, -1.41691e-07, 6.11162e-12]
		scan_coef[8,*]=[-43.4404, 0.0119333, -1.84404e-08, 3.84689e-12]
		crank_coef[8,*]=[-36.7147, 0.00640224, -2.48667e-07, 1.23399e-11]
		scan_coef[9,*]=[-25.4927, 0.0111929, 5.53309e-08, 2.99862e-12]
		crank_coef[9,*]=[-38.5203, 0.00570008, -9.66534e-08, 4.11774e-12]
		scan_coef[10,*]=[-69.4507, 0.0123205, 7.69257e-09, -1.57182e-13]
		crank_coef[10,*]=[-39.2763, 0.00635517, -2.09386e-07, 9.50505e-12]
		scan_coef[11,*]=[19.2273, 0.0123219, 1.96289e-08, 7.45654e-13]
		crank_coef[11,*]=[-38.3065, 0.00599672, -1.52217e-07, 7.23657e-12]
		scan_coef[12,*]=[-22.394, 0.010695, 1.2383e-7, 0.0]
		crank_coef[12,*]=[-45.418, 5.9760e-3, -1.3409e-7, 5.6409e-12]
		scan_coef[13,*]=[-30.216, 0.010662, 1.1030e-7, 0.0]
		crank_coef[13,*]=[-45.951, 7.2355e-3, -3.0194e-7, 1.2089e-11]
	end
; new encoders starting with shot 156610
	((shot ge 156610) and (shot lt 157012)): begin
        scan_coef[6,*]=[-40.9738, 0.0119706, -3.85575e-08, 3.10844e-12]
        crank_coef[6,*]=[-31.0703, 0.00586743, -1.89587e-07, 1.04283e-11]
        scan_coef[7,*]=[-40.8883, 0.0116884, 6.04203e-08, -1.15750e-12]
        crank_coef[7,*]=[-39.6653, 0.00600682, -1.42297e-07, 6.06044e-12]
        scan_coef[8,*]=[-44.1429, 0.0120000, -1.93270e-08, 3.90961e-12]
        crank_coef[8,*]=[-37.4492, 0.00648361, -2.54785e-07, 1.24784e-11]        
        scan_coef[9,*]=[-24.1801, 0.0110234, 5.45626e-08, 2.85441e-12]
        crank_coef[9,*]=[-38.9331, 0.00573619, -9.83019e-08, 4.16563e-12]
        scan_coef[10,*]=[-63.4457, 0.0120543, 7.13524e-09, -1.46948e-13]
        crank_coef[10,*]=[-36.2387, 0.00620513, -1.98509e-07, 9.72235e-12]
        scan_coef[11,*]=[19.5754, 0.0121955, 1.92866e-08, 7.22741e-13]
        crank_coef[11,*]=[-38.0441, 0.00600023, -1.52117e-07, 7.29771e-12]
        scan_coef[12,*]=[-21.9642, 0.0107446, 1.24750e-07,  0.00000]
        crank_coef[12,*]=[-45.4437, 0.00595932, -1.33363e-07, 5.59055e-12]
        scan_coef[13,*]=[-13.8246, 0.0110099, 1.10580e-07, 0.00000]
        crank_coef[13,*]=[-46.5560, 0.00731388, -3.07294e-07, 1.22280e-11]
	end
	((shot ge 157012) and (shot lt 157683)): begin
        scan_coef[6,*]=[-37.9025, 0.0119347, -3.60620e-08, 3.09544e-12]
        crank_coef[6,*]=[-31.0703, 0.00586743, -1.89587e-07, 1.04283e-11]
        scan_coef[7,*]=[-40.8883, 0.0116884, 6.04203e-08, -1.15750e-12]
        crank_coef[7,*]=[-39.6653, 0.00600682, -1.42297e-07, 6.06044e-12]
        scan_coef[8,*]=[-28.9329, 0.0101617, -3.198546e-09, 2.392042e-12]
         crank_coef[8,*]=[-37.4492, 0.00648361, -2.54785e-07, 1.24784e-11]       
        scan_coef[9,*]=[-24.1801, 0.0110234, 5.45626e-08, 2.85441e-12]
        crank_coef[9,*]=[-38.9331, 0.00573619, -9.83019e-08, 4.16563e-12]
        scan_coef[10,*]=[-59.708248,0.012051257,6.9899648e-09,-1.4667674e-13]
        crank_coef[10,*]=[-36.2387, 0.00620513, -1.98509e-07, 9.72235e-12]
        scan_coef[11,*]=[19.5754, 0.0121955, 1.92866e-08, 7.22741e-13]
        crank_coef[11,*]=[-38.0441, 0.00600023, -1.52117e-07, 7.29771e-12]
        scan_coef[12,*]=[-21.9642, 0.0107446, 1.24750e-07,  0.00000]
        crank_coef[12,*]=[-45.4437, 0.00595932, -1.33363e-07, 5.59055e-12]
        scan_coef[13,*]=[-13.8246, 0.0110099, 1.10580e-07, 0.00000]
        crank_coef[13,*]=[-46.5560, 0.00731388, -3.07294e-07, 1.22280e-11]
    end
      ((shot ge 157683) and (shot lt 158160)): begin
        scan_coef[6,*]=[-37.9025, 0.0119347, -3.60620e-08, 3.09544e-12]
        crank_coef[6,*]=[-29.886630, 0.0058292012, -1.8561317e-07, 1.0632052e-11]
        scan_coef[7,*]=[-40.8883, 0.0116884, 6.04203e-08, -1.15750e-12]
        crank_coef[7,*]=[-36.178748, 0.0058425996, -1.3146145e-07, 6.0509449e-12]
        scan_coef[8,*]=[0.0, 0.0, 0.0, 0.0]
        crank_coef[8,*]=[-37.4492, 0.00648361, -2.54785e-07, 1.24784e-11]
        scan_coef[9,*]=[-24.1801, 0.0110234, 5.45626e-08, 2.85441e-12]
        crank_coef[9,*]=[-38.9331, 0.00573619, -9.83019e-08, 4.16563e-12]
        scan_coef[10,*]=[-59.708248,0.012051257,6.9899648e-09,-1.4667674e-13]
        crank_coef[10,*]=[-34.589247, 0.0060617828, -1.8825895e-07, 9.5370602e-12]
        scan_coef[11,*]=[19.5754, 0.0121955, 1.92866e-08, 7.22741e-13]
        crank_coef[11,*]=[-44.575675, 0.0063201697, -1.7382380e-07, 7.2060345e-12]
        scan_coef[12,*]=[-21.9642, 0.0107446, 1.24750e-07,  0.00000]
        crank_coef[12,*]=[-48.672563, 0.0061331008, -1.4356492e-07, 5.6628427e-12]
        scan_coef[13,*]=[-13.8246, 0.0110099, 1.10580e-07, 0.00000]
        crank_coef[13,*]=[-46.5560, 0.00731388, -3.07294e-07, 1.22280e-11]        
    end
      ((shot ge 158160) and (shot lt 158287)): begin
        scan_coef[6,*]=[-41.338586,0.012359148,-4.1387473e-08,3.4189971e-12]
        crank_coef[6,*]=[-64.095923,0.0089958357,-3.9101874e-07,1.3016642e-11]
        scan_coef[7,*]=[-40.8883, 0.0116884, 6.04203e-08, -1.15750e-12]
        crank_coef[7,*]=[ -33.563984,0.0060585778,-1.3791914e-07,7.1622498e-12]
        scan_coef[8,*]=[0.0, 0.0, 0.0, 0.0]
        crank_coef[8,*]=[-37.4492, 0.00648361, -2.54785e-07, 1.24784e-11]
        scan_coef[9,*]=[-24.1801, 0.0110234, 5.45626e-08, 2.85441e-12]
        crank_coef[9,*]=[-38.9331, 0.00573619, -9.83019e-08, 4.16563e-12]
        scan_coef[10,*]=[-59.708248,0.012051257,6.9899648e-09,-1.4667674e-13]
        crank_coef[10,*]=[-39.238649,0.0063497132,-2.0902103e-07,9.4916958e-12]
        scan_coef[11,*]=[19.5754, 0.0121955, 1.92866e-08, 7.22741e-13]
        crank_coef[11,*]=[-38.288510,0.0059958068,-1.5215187e-07,7.2365699e-12]
        scan_coef[12,*]=[-21.9642, 0.0107446, 1.24750e-07,  0.00000]
        crank_coef[12,*]=[-48.672563, 0.0061331008, -1.4356492e-07, 5.6628427e-12]
        scan_coef[13,*]=[-13.8246, 0.0110099, 1.10580e-07, 0.00000]
        crank_coef[13,*]=[-46.5560, 0.00731388, -3.07294e-07, 1.22280e-11]        
    end   
      ((shot ge 158287) and (shot lt 158797)): begin
        scan_coef[6,*]=[-41.338586,0.012359148,-4.1387473e-08,3.4189971e-12]
        crank_coef[6,*]=[-53.322334,0.0075221751,-2.9687001e-07,1.0543139e-11]
        scan_coef[7,*]=[-40.8883, 0.0116884, 6.04203e-08, -1.15750e-12]
        crank_coef[7,*]=[-33.238028,0.0057246069,-1.2269825e-07,6.0863956e-12]
        scan_coef[8,*]=[-111.23281,0.0123281,-8.0572069e-08,3.6925196e-12]         
        crank_coef[8,*]=[-37.4492, 0.00648361, -2.54785e-07, 1.24784e-11]              
        scan_coef[9,*]=[-24.1801, 0.0110234, 5.45626e-08, 2.85441e-12]
        crank_coef[9,*]=[-38.9331, 0.00573619, -9.83019e-08, 4.16563e-12]
        scan_coef[10,*]=[-59.708248,0.012051257,6.9899648e-09,-1.4667674e-13]
        crank_coef[10,*]=[-39.238649,0.0063497132,-2.0902103e-07,9.4916958e-12]
        scan_coef[11,*]=[19.5754, 0.0121955, 1.92866e-08, 7.22741e-13]
        crank_coef[11,*]=[-38.288510,0.0059958068,-1.5215187e-07,7.2365699e-12]
        scan_coef[12,*]=[-21.9642, 0.0107446, 1.24750e-07,  0.00000]
        crank_coef[12,*]=[-48.672563, 0.0061331008, -1.4356492e-07, 5.6628427e-12]
        scan_coef[13,*]=[-13.8246, 0.0110099, 1.10580e-07, 0.00000]
        crank_coef[13,*]=[-46.5560, 0.00731388, -3.07294e-07, 1.22280e-11]        
    end    
      ((shot ge 158797) and (shot lt 158850)): begin
        scan_coef[6,*]=[-41.338586,0.012359148,-4.1387473e-08,3.4189971e-12]
        crank_coef[6,*]=[-53.322334,0.0075221751,-2.9687001e-07,1.0543139e-11]
        scan_coef[7,*]=[-40.8883, 0.0116884, 6.04203e-08, -1.15750e-12]
        crank_coef[7,*]=[-33.238028,0.0057246069,-1.2269825e-07,6.0863956e-12]
        scan_coef[8,*]=[-111.23281,0.0123281,-8.0572069e-08,3.6925196e-12]                 
        crank_coef[8,*]=[-31.894817,0.0060366805,-2.1980332e-07,1.2326218e-11]             
        scan_coef[9,*]=[-24.1801, 0.0110234, 5.45626e-08, 2.85441e-12]        
        crank_coef[9,*]=[-39.986617,0.0057482585,-9.9742966e-08,4.1131405e-12]
        scan_coef[10,*]=[-59.708248,0.012051257,6.9899648e-09,-1.4667674e-13]
        crank_coef[10,*]=[-39.238649,0.0063497132,-2.0902103e-07,9.4916958e-12]
        scan_coef[11,*]=[19.5754, 0.0121955, 1.92866e-08, 7.22741e-13]
        crank_coef[11,*]=[-38.288510,0.0059958068,-1.5215187e-07,7.2365699e-12]
        scan_coef[12,*]=[-21.9642, 0.0107446, 1.24750e-07,  0.00000]
        crank_coef[12,*]=[-48.672563, 0.0061331008, -1.4356492e-07, 5.6628427e-12]
        scan_coef[13,*]=[-13.8246, 0.0110099, 1.10580e-07, 0.00000]
        crank_coef[13,*]=[-46.5560, 0.00731388, -3.07294e-07, 1.22280e-11]        
    end  
      ((shot ge 158850) and (shot lt 159086)): begin
        scan_coef[6,*]=[-41.338586,0.012359148,-4.1387473e-08,3.4189971e-12]
        crank_coef[6,*]=[-49.674631,0.0072457087,-2.8195950e-07,1.0584058e-11]        
        scan_coef[7,*]=[-40.8883, 0.0116884, 6.04203e-08, -1.15750e-12]
        crank_coef[7,*]=[-34.714705,0.0057956785,-1.2768691e-07,6.1082492e-12]
        scan_coef[8,*]=[-118.12056,0.012731585,-9.1121245e-08,3.9759283e-12]               
        crank_coef[8,*]=[-31.894817,0.0060366805,-2.1980332e-07,1.2326218e-11]             
        scan_coef[9,*]=[-24.1801, 0.0110234, 5.45626e-08, 2.85441e-12]        
        crank_coef[9,*]=[-39.986617,0.0057482585,-9.9742966e-08,4.1131405e-12]
        scan_coef[10,*]=[-59.708248,0.012051257,6.9899648e-09,-1.4667674e-13]
        crank_coef[10,*]=[-37.626662,0.0062445606,-2.0173141e-07,9.4916958e-12]
        scan_coef[11,*]=[19.5754, 0.0121955, 1.92866e-08, 7.22741e-13]
        crank_coef[11,*]=[-38.288510,0.0059958068,-1.5215187e-07,7.2365699e-12]
        scan_coef[12,*]=[-21.9642, 0.0107446, 1.24750e-07,  0.00000]
        crank_coef[12,*]=[-48.672563, 0.0061331008, -1.4356492e-07, 5.6628427e-12]
        scan_coef[13,*]=[-13.8246, 0.0110099, 1.10580e-07, 0.00000]
        crank_coef[13,*]=[-46.5560, 0.00731388, -3.07294e-07, 1.22280e-11]        
    end  
      ((shot ge 159086) and (shot lt 161700)): begin
        scan_coef[6,*]= [-44.449799,     0.012370241,  -4.3893624e-08,   3.4104090e-12]
        crank_coef[6,*]= [-46.013375,    0.0069647653,  -2.6569133e-07,   1.0589925e-11]                
        scan_coef[7,*]=  [-40.311114,     0.011640033,   5.9690437e-08,  -1.1414494e-12]
        crank_coef[7,*]= [-34.714705,    0.0057956785,  -1.2768691e-07,   6.1082492e-12]        
        scan_coef[8,*]=[-118.12056,0.012731585,-9.1121245e-08,3.9759283e-12]               
        crank_coef[8,*]=[-31.894817,0.0060366805,-2.1980332e-07,1.2326218e-11]             
        scan_coef[9,*]=[-24.1801, 0.0110234, 5.45626e-08, 2.85441e-12]        
        crank_coef[9,*]=[-39.986617,0.0057482585,-9.9742966e-08,4.1131405e-12]
        scan_coef[10,*]=[-59.708248,0.012051257,6.9899648e-09,-1.4667674e-13]
        crank_coef[10,*]=[-37.626662,0.0062445606,-2.0173141e-07,9.4916958e-12]
        scan_coef[11,*]=[19.5754, 0.0121955, 1.92866e-08, 7.22741e-13]
        crank_coef[11,*]=[-38.288510,0.0059958068,-1.5215187e-07,7.2365699e-12]
        scan_coef[12,*]=[-21.9642, 0.0107446, 1.24750e-07,  0.00000]
        crank_coef[12,*]=[-48.672563, 0.0061331008, -1.4356492e-07, 5.6628427e-12]
        scan_coef[13,*]=[-13.8246, 0.0110099, 1.10580e-07, 0.00000]
        crank_coef[13,*]=[-46.5560, 0.00731388, -3.07294e-07, 1.22280e-11]        
    end 
      ((shot ge 161700) and (shot lt 162676)): begin    ;antenna refurbishment Apr 2015
        scan_coef[6,*] =  [- 9.19316297E+01, 2.96454027E-02, - 2.12939736E-06, 8.69367769E-11]  
        crank_coef[6,*] = [- 1.21682112E+02, 1.03376975E-02, - 3.13872686E-07, 6.04174977E-12]
        scan_coef[7,*] =  [- 9.53696294E+01, 3.13730094E-02, - 2.33358966E-06, 9.46336682E-11]   
        crank_coef[7,*] = [- 4.37846761E+01, 6.54936464E-03, - 2.11374219E-07, 8.77720173E-12]
        scan_coef[8,*] =  [- 4.01891252E+02, 7.12484832E-02, - 4.10604928E-06, 9.52470743E-11]  
        crank_coef[8,*] = [- 4.84533448E+01, 7.02710944E-03, - 2.60759056E-07, 1.00887344E-11]
        scan_coef[9,*] =  [- 2.62699545E+01, 1.16843672E-02,   3.65664187E-09, 4.13341134E-12]  
        crank_coef[9,*] = [- 4.57193487E+01, 6.82783693E-03, - 2.42735124E-07, 9.80067941E-12]
        scan_coef[10,*] = [- 3.70432558E+01, 1.19655177E-02, - 6.67715767E-08, 7.86736235E-12]  
        crank_coef[10,*] =[- 6.73785156E+01, 8.62372136E-03, - 3.48505086E-07, 1.03790622E-11]
        scan_coef[11,*] = [- 2.53057293E+02, 5.32398316E-02, - 3.43823532E-06, 9.59974051E-11]   
        crank_coef[11,*] =[- 4.74969094E+01, 6.80180192E-03, - 2.16693824E-07, 8.06325111E-12]
        scan_coef[12,*] = [- 2.34977648E+02, 5.87289635E-02, - 4.43068828E-06, 1.40305201E-10]  
        crank_coef[12,*] =[- 5.18396476E+01, 6.23336592E-03, - 1.43280660E-07, 5.31682119E-12]
        scan_coef[13,*] = [- 1.75442012E+02, 3.50634357E-02, - 1.96150296E-06, 5.62170311E-11]  
        crank_coef[13,*] =[- 5.96847592E+01, 8.41412890E-03, - 3.61976419E-07, 1.17766092E-11]   
    end 
      ((shot ge 162676) and (shot lt 166218)): begin	;after toroidal encoder skips, new calibration with everything already installed
        scan_coef[6,*] =  [- 9.19316297E+01, 2.96454027E-02, - 2.12939736E-06, 8.69367769E-11]  
        crank_coef[6,*] = [  -118.93347 ,    0.010164172 , -3.0861583e-07 ,  6.0300732e-12]
        scan_coef[7,*] =  [- 9.53696294E+01, 3.13730094E-02, - 2.33358966E-06, 9.46336682E-11]   
        crank_coef[7,*] = [ -45.473572 ,   0.0066585876,  -2.1806813e-07 ,  8.7747819e-12]
        scan_coef[8,*] =  [- 4.01891252E+02, 7.12484832E-02, - 4.10604928E-06, 9.52470743E-11]  
        crank_coef[8,*] = [- 4.84533448E+01, 7.02710944E-03, - 2.60759056E-07, 1.00887344E-11]
        scan_coef[9,*] =  [- 2.62699545E+01, 1.16843672E-02,   3.65664187E-09, 4.13341134E-12]  
        crank_coef[9,*] = [  -47.463927,    0.0069487638 , -2.4990058e-07,   9.7842696e-12]
        scan_coef[10,*] = [- 3.70432558E+01, 1.19655177E-02, - 6.67715767E-08, 7.86736235E-12]  
        crank_coef[10,*] =[- 6.73785156E+01, 8.62372136E-03, - 3.48505086E-07, 1.03790622E-11]
        scan_coef[11,*] = [- 2.53057293E+02, 5.32398316E-02, - 3.43823532E-06, 9.59974051E-11]   
        crank_coef[11,*] =[- 4.74969094E+01, 6.80180192E-03, - 2.16693824E-07, 8.06325111E-12]
        scan_coef[12,*] = [- 2.34977648E+02, 5.87289635E-02, - 4.43068828E-06, 1.40305201E-10]  
        crank_coef[12,*] =[  -47.050234 ,   0.0060190289,  -1.3080740e-07,   5.3168212e-12]
        scan_coef[13,*] = [- 1.75442012E+02, 3.50634357E-02, - 1.96150296E-06, 5.62170311E-11]  
        crank_coef[13,*] =[ -61.854100 ,   0.0086010344 , -3.7098554e-07 ,  1.1776609e-11]     
    end     
      ((shot ge 166218) and (shot lt 168000)): begin ;new toroidal encoder installed on 255 R
        scan_coef[6,*] =  [- 9.19316297E+01, 2.96454027E-02, - 2.12939736E-06, 8.69367769E-11]  
        crank_coef[6,*] = [  -118.93347 ,    0.010164172 , -3.0861583e-07 ,  6.0300732e-12]
        scan_coef[7,*] =  [- 9.53696294E+01, 3.13730094E-02, - 2.33358966E-06, 9.46336682E-11]   
        crank_coef[7,*] = [ -45.473572 ,   0.0066585876,  -2.1806813e-07 ,  8.7747819e-12]
        scan_coef[8,*] =  [- 4.01891252E+02, 7.12484832E-02, - 4.10604928E-06, 9.52470743E-11]  
        crank_coef[8,*] = [- 4.84533448E+01, 7.02710944E-03, - 2.60759056E-07, 1.00887344E-11]
        scan_coef[9,*] =  [- 2.62699545E+01, 1.16843672E-02,   3.65664187E-09, 4.13341134E-12]  
        crank_coef[9,*] = [  -47.463927,    0.0069487638 , -2.4990058e-07,   9.7842696e-12]
        scan_coef[10,*] = [- 3.70432558E+01, 1.19655177E-02, - 6.67715767E-08, 7.86736235E-12]  
        crank_coef[10,*] =[- 6.73785156E+01, 8.62372136E-03, - 3.48505086E-07, 1.03790622E-11]
        scan_coef[11,*] = [- 2.53057293E+02, 5.32398316E-02, - 3.43823532E-06, 9.59974051E-11]          
        crank_coef[11,*] =[- 4.74730260E+01, 6.79522800E-03, - 2.16287140E-07, 8.04529400E-12]
        scan_coef[12,*] = [- 2.34977648E+02, 5.87289635E-02, - 4.43068828E-06, 1.40305201E-10]  
        crank_coef[12,*] =[  -47.050234 ,   0.0060190289,  -1.3080740e-07,   5.3168212e-12]
        scan_coef[13,*] = [- 1.75442012E+02, 3.50634357E-02, - 1.96150296E-06, 5.62170311E-11]  
        crank_coef[13,*] =[ -61.854100 ,   0.0086010344 , -3.7098554e-07 ,  1.1776609e-11]     
    end 
      ((shot ge 168000) and (shot lt 178730)): begin ;After refurbishment/repair Fall 2016
 	scan_coef[6,*]  = [-5.40663433E+01,  1.48002613E-02, -2.19589751E-07,  5.95957332E-12] ;240L 
 	crank_coef[6,*]  = [-1.58680462E+02, 1.66130661E-02, -6.57223014E-07,  1.21290149E-11]
 	scan_coef[7,*]  = [-1.29519731E+02,  4.45836849E-02, -4.01093466E-06,  1.64312280E-10] ;240R
 	crank_coef[7,*]  = [-4.38507402E+01, 6.89220272E-03, -2.57358407E-07,  1.06725543E-11]
 	scan_coef[8,*]  = [-4.26661299E+02,  7.78468400E-02, -4.63859366E-06,  1.09166829E-10] ;270L
 	crank_coef[8,*]  = [-4.73417469E+01, 6.67493981E-03, -2.31349438E-07,  9.41683875E-12]
 	scan_coef[9,*]  = [-4.05452520E+01,  1.71732956E-02, -6.67354158E-07,  2.98446664E-11] ;270R
 	crank_coef[9,*]  = [-4.53925069E+01, 6.79459228E-03, -2.39412072E-07,  9.64454918E-12]
 	scan_coef[10,*] = [-1.10435538E+02,  4.02329419E-02, -3.63807674E-06,  1.56117114E-10] ;255L
 	crank_coef[10,*] = [-6.03728857E+01, 6.32846570E-03, -1.11230474E-07,  2.88160489E-12]
 	scan_coef[11,*] = [-3.29888512E+02,  7.15292859E-02, -4.87613823E-06,  1.33377527E-10] ;255R
 	crank_coef[11,*] = [-4.55266477E+01, 5.99736183E-03, -1.10151142E-07,  3.90324271E-12]
 	scan_coef[12,*] = [-7.11114288E+01,  1.09291214E-02, 2.10522608E-07, -9.46815233E-12]  ;285L
 	crank_coef[12,*] = [-5.62235250E+01, 7.86098706E-03, -3.15313004E-07,  1.08547470E-11]
 	scan_coef[13,*] = [-2.77598556E+02,  6.03870598E-02, -4.02881343E-06,  1.11991912E-10] ;285R
 	crank_coef[13,*] = [-6.11386409E+01, 8.83014214E-03, -4.00651490E-07,  1.29285093E-11]    
    end 
      ((shot ge 178730) and (shot lt 187865)): begin  ;add 300deg top launch launcher
 	scan_coef[6,*]  = [-5.40663433E+01,  1.48002613E-02, -2.19589751E-07,  5.95957332E-12] ;240L 
 	crank_coef[6,*]  = [-1.58680462E+02, 1.66130661E-02, -6.57223014E-07,  1.21290149E-11]
 	scan_coef[7,*]  = [-1.29519731E+02,  4.45836849E-02, -4.01093466E-06,  1.64312280E-10] ;240R
 	crank_coef[7,*]  = [-4.38507402E+01, 6.89220272E-03, -2.57358407E-07,  1.06725543E-11]
 	scan_coef[8,*]  = [-4.26661299E+02,  7.78468400E-02, -4.63859366E-06,  1.09166829E-10] ;270L
 	crank_coef[8,*]  = [-4.73417469E+01, 6.67493981E-03, -2.31349438E-07,  9.41683875E-12]
 	scan_coef[9,*]  = [-4.05452520E+01,  1.71732956E-02, -6.67354158E-07,  2.98446664E-11] ;270R
 	crank_coef[9,*]  = [-4.53925069E+01, 6.79459228E-03, -2.39412072E-07,  9.64454918E-12]
 	scan_coef[10,*] = [-1.10435538E+02,  4.02329419E-02, -3.63807674E-06,  1.56117114E-10] ;255L
 	crank_coef[10,*] = [-6.03728857E+01, 6.32846570E-03, -1.11230474E-07,  2.88160489E-12]
 	scan_coef[11,*] = [-3.29888512E+02,  7.15292859E-02, -4.87613823E-06,  1.33377527E-10] ;255R
 	crank_coef[11,*] = [-4.55266477E+01, 5.99736183E-03, -1.10151142E-07,  3.90324271E-12]
 	scan_coef[12,*] = [-7.11114288E+01,  1.09291214E-02, 2.10522608E-07, -9.46815233E-12]  ;285L
 	crank_coef[12,*] = [-5.62235250E+01, 7.86098706E-03, -3.15313004E-07,  1.08547470E-11]
 	scan_coef[13,*] = [-2.77598556E+02,  6.03870598E-02, -4.02881343E-06,  1.11991912E-10] ;285R
 	crank_coef[13,*] = [-6.11386409E+01, 8.83014214E-03, -4.00651490E-07,  1.29285093E-11]   
	scan_coef[14,*] = [0,  0, 0,  0] ;300 top launch
 	crank_coef[14,*] = [0, 0, 0,  0]
    end   
      (shot ge 187865): begin  ;add 90deg top launch launcher
 	scan_coef[6,*]  = [-5.40663433E+01,  1.48002613E-02, -2.19589751E-07,  5.95957332E-12] ;240L 
 	crank_coef[6,*]  = [-1.58680462E+02, 1.66130661E-02, -6.57223014E-07,  1.21290149E-11]
 	scan_coef[7,*]  = [-1.29519731E+02,  4.45836849E-02, -4.01093466E-06,  1.64312280E-10] ;240R
 	crank_coef[7,*]  = [-4.38507402E+01, 6.89220272E-03, -2.57358407E-07,  1.06725543E-11]
 	scan_coef[8,*]  = [-4.26661299E+02,  7.78468400E-02, -4.63859366E-06,  1.09166829E-10] ;270L
 	crank_coef[8,*]  = [-4.73417469E+01, 6.67493981E-03, -2.31349438E-07,  9.41683875E-12]
 	scan_coef[9,*]  = [-4.05452520E+01,  1.71732956E-02, -6.67354158E-07,  2.98446664E-11] ;270R
 	crank_coef[9,*]  = [-4.53925069E+01, 6.79459228E-03, -2.39412072E-07,  9.64454918E-12]
 	scan_coef[10,*] = [-1.10435538E+02,  4.02329419E-02, -3.63807674E-06,  1.56117114E-10] ;255L
 	crank_coef[10,*] = [-6.03728857E+01, 6.32846570E-03, -1.11230474E-07,  2.88160489E-12]
 	scan_coef[11,*] = [-3.29888512E+02,  7.15292859E-02, -4.87613823E-06,  1.33377527E-10] ;255R
 	crank_coef[11,*] = [-4.55266477E+01, 5.99736183E-03, -1.10151142E-07,  3.90324271E-12]
 	scan_coef[12,*] = [-7.11114288E+01,  1.09291214E-02, 2.10522608E-07, -9.46815233E-12]  ;285L
 	crank_coef[12,*] = [-5.62235250E+01, 7.86098706E-03, -3.15313004E-07,  1.08547470E-11]
 	scan_coef[13,*] = [-2.77598556E+02,  6.03870598E-02, -4.02881343E-06,  1.11991912E-10] ;285R
 	crank_coef[13,*] = [-6.11386409E+01, 8.83014214E-03, -4.00651490E-07,  1.29285093E-11]   
	scan_coef[14,*] = [0,  0, 0,  0] ;300 top launch
 	crank_coef[14,*] = [0, 0, 0,  0]  
	scan_coef[15,*] = [0,  0, 0,  0] ;90 top launch
 	crank_coef[15,*] = [0, 0, 0,  0]  
    end               
endcase

;*************** tank connection data ******************************
; names of ports; port on the LEFT has a LARGER toroidal angle, ie, 271.4R+1 is referred to as 270L
; port  number:  1             2            3           4            5       
Port_Names=['271.4 R+1', '268.6 R+1', '256.4 R+1', '253.6 R+1', '241.4 R+1', $
;       6            7             8		9	   10
	'238.6 R+1', '286.4 R+1', '284.6 R+1', '300 R+2', '90 R+2']

; data for which gyrotron in which tank
; -1=none, 0=Kat, 1=Dor, 2=Tot, 3=Sca, 4=Bor, 5=Nat, 6=Tin, 7=Lion, 8=Luke, 9=Han, 10=Leia, 11=Chewbacca, 12=Vader, 14=R2D2, 15=NASA, 16=TLeia/TVader, 17=TLuke, 13=YODA, 18=THan
;		                      Tank:     1    2    3   4    5    6    7    8   9   10
; switch a different gyrotron into top launch line does NOT change Gyro_in_Tank
case 1 of
	(shot lt 123575): Gyro_in_Tank=  [0,-1,-1,6,3,7,4,5,-1,-1,-1,-1]	
	((shot ge 123575) and (shot lt 127856)): Gyro_in_Tank=[-1,-1,-1,-1,3,7,8,-1,-1,-1,-1,-1]
	((shot ge 127586) and (shot lt 134000)): Gyro_in_Tank=[-1,-1,-1,10,3,7,8,9,11,-1,-1,-1]
	((shot ge 134000) and (shot lt 147778)): Gyro_in_Tank=[-1,-1,-1,10,3,7,8,9,6,-1,-1,-1]	; Tinman swapped for Chewie
	((shot ge 147778) and (shot lt 151260)): Gyro_in_Tank=[-1,-1,-1,10,3,7,8,9,6,11,-1,-1]; Chewbacca into tank 10
	((shot ge 151260) and (shot lt 155914)): Gyro_in_Tank=[-1,-1,-1,10,3,7,8,6,-1,11,-1,-1]; Tinman replaces Han in tank 8
	((shot ge 155914) and (shot lt 157164)): Gyro_in_Tank=[-1,-1,-1,10,3,7,8,6,-1,11,-1,-1]; Lion was removed from tank, not MDS	
	((shot ge 157164) and (shot lt 157866)): Gyro_in_Tank=[-1,-1,-1,10,3,-1,8,6,-1,11,-1,-1]; Lion removed from Tank 6 for repair
        ((shot ge 157866) and (shot lt 163600)): Gyro_in_Tank=[-1,-1,-1,10,3,-1,8,6,15,11,-1,-1]; NASA gyrotron added	
        ((shot ge 163600) and (shot lt 164024)): Gyro_in_Tank=[-1,-1,-1,10,3,7,8,6,15,11,-1,-1]; Lion gyrotron repaired
	((shot ge 164024) and (shot lt 172980)): Gyro_in_Tank=[-1,-1,-1,10,3,-1,8,6,15,11,-1,-1]; Lion removed
	((shot ge 172980) and (shot lt 175090)): Gyro_in_Tank=[-1,-1,-1,10,3,-1,8,6,15,11,12,-1]; Vader added
	((shot ge 175090) and (shot lt 178730)): Gyro_in_Tank=[-1,-1,-1,10,-1,-1,8,6,15,11,12,-1]; SCA removed
	((shot ge 178730) and (shot lt 186122)): Gyro_in_Tank=[-1,-1,-1,10,-1,-1,8,6,15,-1,12,16]; CHE removed, Leia/Vader connected to Top launch
	((shot ge 186122) and (shot lt 187865)): Gyro_in_Tank=[-1,-1,-1,10,14,-1,8,-1,-1,-1,-1,-1]; R2D2 added to tank 5
	((shot ge 187865) and (shot lt 192589)): Gyro_in_Tank=[-1,-1,-1,10,14,-1,8,13,-1,-1,-1,16,17]; YODA added to tank 8, Luke connected to Top launch
	(shot ge 192589): Gyro_in_Tank=[-1,-1,-1,10,14,-1,8,13,-1,-1,9,-1,17,18]; HANR added to tank 11, THANR connected to Top launch, TLEIA removed from top launch
	else:
endcase

; data for which antenna is in which port
; Ant_in_Port is the antenna sequence number n in Antenna_data[n] in each port location
; Port sequence: 271.4, 268.6, 256.4, 253.6, 241.4, 238.6, 286.4, 284.6, 300, 90 ; 285 added in 2012 campaign, 300 added in 2019, 90 added in 2022
case 1 of 
	(shot lt 107739): Ant_in_Port=[0,1,2,3,4,5]
	((shot ge 107739) and (shot lt 111392)): Ant_in_Port=[0,1,2,3,6,7]	; start 2002 campaign
	((shot ge 111392) and (shot lt 123575)): Ant_in_Port= [8,9,2,3,6,7]	; start 2003 campaign
	((shot ge 123575) and (shot lt 147778)): Ant_in_Port=[8,9,10,11,6,7] ; start 2006 campaign P02,P06,P01
	((shot ge 147778) and (shot lt 178730)): Ant_in_Port=[8,9,10,11,6,7,12,13] ; added PPPL11 antenna in 285 deg port
	((shot ge 178730) and (shot lt 187865)): Ant_in_Port=[8,9,10,11,6,7,12,13,14] ; added top launch antenna in 300 deg port
	(shot ge 187865): Ant_in_Port=[8,9,10,11,6,7,12,13,14,15] ; added top launch antenna in 90 deg port
	else:
endcase

; data for which tank is connected to which port
; Port_by_Tank is array containing the port number to which each tank is connected
Port_by_Tank=intarr(NumberOfTanks+1)
case 1 of
	(shot lt 105795): begin	; 2001 campaign
		Port_by_Tank[7]=3	; eg, tank 7 connected to antenna port 3
		Port_by_Tank[8]=4
		Port_by_Tank[4]=6
	end
	((shot ge 105795) and (shot lt 106042)): begin
		Port_by_Tank[1]=3
		Port_by_Tank[4]=6
		Port_by_Tank[7]=5
		Port_by_Tank[8]=4
	end
	((shot ge 106042) and (shot lt 107109)): begin
		Port_by_Tank[4]=1
		Port_by_Tank[7]=2
		Port_by_Tank[1]=3
		Port_by_Tank[8]=4
	end
	((shot ge 107109) and (shot lt 107348)): begin
		Port_by_Tank[1]=3
		Port_by_Tank[4]=4
		Port_by_Tank[7]=5
		Port_by_Tank[8]=6
	end
	((shot ge 107348) and (shot lt 107739)): begin
		Port_by_Tank[1]=1
		Port_by_Tank[4]=2
		Port_by_Tank[7]=5
		Port_by_Tank[8]=6
	end
	((shot ge 107739) and (shot lt 109869)): begin
		Port_by_Tank[1]=1
		Port_by_Tank[4]=6
		Port_by_Tank[5]=2
		Port_by_Tank[6]=5
		Port_by_Tank[7]=3
		Port_by_Tank[8]=4
	end
	((shot ge 109869) and (shot lt 111392)): begin	; 2002 campaign
		Port_by_Tank[1]=1
		Port_by_Tank[4]=6
		Port_by_Tank[6]=5
		Port_by_Tank[7]=3
		Port_by_Tank[8]=4
	end
	((shot ge 111392) and (shot lt 117305)): begin	; 2003 campaign
		Port_by_Tank[1]=1
		Port_by_Tank[4]=6
		Port_by_Tank[5]=3
		Port_by_Tank[6]=2
		Port_by_Tank[7]=5
		Port_by_Tank[8]=4
	end
	((shot ge 117305) and (shot lt 121020)): begin	; file polar_angles_04.xls from Lohr 17Mar04
		Port_by_Tank[1]=1
		Port_by_Tank[4]=2
		Port_by_Tank[5]=3
		Port_by_Tank[7]=4
	end
	((shot ge 121020) and (shot lt 123575)): begin	; Lion in sys6, back to ant2; Tinman moved from ant2 to ant6
		Port_by_Tank[1]=1
		Port_by_Tank[4]=6
		Port_by_Tank[5]=3
		Port_by_Tank[7]=4
		Port_by_Tank[6]=2
	end
	((shot ge 123575) and (shot lt 127586)): begin	; from 5/2006; file from Gorelov polar_angles_06.xls
		Port_by_Tank[5]=3	; Scarecrow in tank5 to 255L
		Port_by_Tank[6]=2	; Lion in tank6 to 270R
		Port_by_Tank[7]=4	; Luke in tank7 to 255R
	end
	((shot ge 127586) and (shot lt 143020)): begin	; from Gorelov polar_angles_07.xls, March 2007
		Port_by_Tank[4]=6	; Leia in tank 4 to 240R
		Port_by_Tank[5]=3	; Scarecrow in tank 5 to 255L
		Port_by_Tank[6]=2	; Lion in tank 6 to 270R
		Port_by_Tank[7]=4	; Luke in tank 7 to 255R
		Port_by_Tank[8]=5	; Han in tank 8 to 240L
		Port_by_Tank[9]=1	; Chewbacca in tank 9 to 270L; Tinman after 134000
	end
	((shot ge 143020) and (shot lt 147778)): begin ; everything replumbed to have fewer bends in LT02
		Port_by_Tank[4]=5 	; Leia in tank 4 to ant 5
		Port_by_Tank[5]=4 	; Scarecrow in tank 5 to ant 4
		Port_by_Tank[6]=3 	; Lion in tank 6 to ant 3
		Port_by_Tank[7]=6 	; Luke in tank 7 to ant 6
		Port_by_Tank[8]=1 	; Han in tank 8 to ant 1
		Port_by_Tank[9]=2 	; Tinman in tank 9 to ant 2
	end
	((shot ge 147778) and (shot lt 151260)): begin
		Port_by_Tank[4]=5 	; Leia in tank 4 to ant 5
		Port_by_Tank[5]=4 	; Scarecrow in tank 5 to ant 4
		Port_by_Tank[6]=3 	; Lion in tank 6 to ant 3
		Port_by_Tank[7]=6 	; Luke in tank 7 to ant 6
		Port_by_Tank[8]=1 	; Han in tank 8 to ant 1
		Port_by_Tank[9]=2 	; Tinman in tank 9 to ant 2
		Port_by_Tank[10]=7	; Chewbacca in tank 10 to ant 7
	end
	 ((shot ge 151260) and (shot lt 157164)): begin
		Port_by_Tank[4]=5 	; Leia in tank 4 to ant 5
		Port_by_Tank[5]=4 	; Scarecrow in tank 5 to ant 4
		Port_by_Tank[6]=3 	; Lion in tank 6 to ant 3
		Port_by_Tank[7]=6 	; Luke in tank 7 to ant 6
		Port_by_Tank[8]=1 	; Tinman in tank 8 to ant 1 
		Port_by_Tank[9]=2 	; no gyrotron in tank 9 to ant 2
		Port_by_Tank[10]=7	; Chewbacca in tank 10 to ant 7
	end	
    ((shot ge 157164) and (shot lt 157866)): begin
        Port_by_Tank[4]=5   ; Leia in tank 4 to ant 5
        Port_by_Tank[5]=4   ; Scarecrow in tank 5 to ant 4
        Port_by_Tank[6]=0   ; Lion at CPI
        Port_by_Tank[7]=6   ; Luke in tank 7 to ant 6
        Port_by_Tank[8]=3   ; Tinman in tank 8 to ant 3 that was Lion's line
        Port_by_Tank[9]=0   ; no gyrotron in tank 9 to ant 2
        Port_by_Tank[10]=7  ; Chewbacca in tank 10 to ant 7      
    end	
    ((shot ge 157866) and (shot lt 158287)): begin
        Port_by_Tank[4]=5   ; Leia in tank 4 to ant 5
        Port_by_Tank[5]=4   ; Scarecrow in tank 5 to ant 4
        Port_by_Tank[6]=0   ; Lion at CPI
        Port_by_Tank[7]=6   ; Luke in tank 7 to ant 6
        Port_by_Tank[8]=3   ; Tinman in tank 8 to ant 3 
        Port_by_Tank[9]=2   ; NASA in tank 9 to ant 2          
        Port_by_Tank[10]=7  ; Chewbacca in tank 10 to ant 7      
    end     
    ((shot ge 158287) and (shot lt 161700)): begin
        Port_by_Tank[4]=5   ; Leia in tank 4 to ant 5
        Port_by_Tank[5]=4   ; Scarecrow in tank 5 to ant 4
        Port_by_Tank[6]=0   ; Lion at CPI
        Port_by_Tank[7]=6   ; Luke in tank 7 to ant 6
        Port_by_Tank[8]=3   ; Tinman in tank 8 to ant 3 
        Port_by_Tank[9]=2   ; NASA in tank 9 to ant 2          
        Port_by_Tank[10]=1  ; Chewbacca in tank 10 to ant 1 repaired      
    end  
    ((shot ge 161700) and (shot lt 163600)): begin
        Port_by_Tank[4]=5   ; Leia in tank 4 to ant 5
        Port_by_Tank[5]=4   ; Scarecrow in tank 5 to ant 4
        Port_by_Tank[6]=0   ; Lion at CPI
        Port_by_Tank[7]=6   ; Luke in tank 7 to ant 6
        Port_by_Tank[8]=1   ; Tinman in tank 8 to ant 1 
        Port_by_Tank[9]=2   ; NASA in tank 9 to ant 2          
        Port_by_Tank[10]=7  ; Chewbacca in tank 10 to ant 7      
    end    
    ((shot ge 163600) and (shot lt 164024)): begin
        Port_by_Tank[4]=5   ; Leia in tank 4 to ant 5
        Port_by_Tank[5]=4   ; Scarecrow in tank 5 to ant 4
        Port_by_Tank[6]=3   ; Lion in tank 6 to ant 3
        Port_by_Tank[7]=6   ; Luke in tank 7 to ant 6
        Port_by_Tank[8]=1   ; Tinman in tank 8 to ant 1 
        Port_by_Tank[9]=2   ; NASA in tank 9 to ant 2          
        Port_by_Tank[10]=7  ; Chewbacca in tank 10 to ant 7      
    end     
    ((shot ge 164024) and (shot lt 172980)): begin
        Port_by_Tank[4]=5   ; Leia in tank 4 to ant 5
        Port_by_Tank[5]=4   ; Scarecrow in tank 5 to ant 4
        Port_by_Tank[6]=0   ; Lion at CPI
        Port_by_Tank[7]=6   ; Luke in tank 7 to ant 6
        Port_by_Tank[8]=1   ; Tinman in tank 8 to ant 1 
        Port_by_Tank[9]=2   ; NASA in tank 9 to ant 2          
        Port_by_Tank[10]=7  ; Chewbacca in tank 10 to ant 7      
    end 
    ((shot ge 172980) and (shot lt 175090)): begin
        Port_by_Tank[4]=5   ; Leia in tank 4 to ant 5
        Port_by_Tank[5]=4   ; Scarecrow in tank 5 to ant 4
        Port_by_Tank[6]=0   ; Lion at CPI
        Port_by_Tank[7]=6   ; Luke in tank 7 to ant 6
        Port_by_Tank[8]=1   ; Tinman in tank 8 to ant 1 
        Port_by_Tank[9]=2   ; NASA in tank 9 to ant 2          
        Port_by_Tank[10]=7  ; Chewbacca in tank 10 to ant 7  
	Port_by_tank[11]=8  ; Vader in tank 11 to ant 8
    end 
    ((shot ge 175090) and (shot lt 177468)): begin
        Port_by_Tank[4]=5   ; Leia in tank 4 to ant 5
        Port_by_Tank[5]=0   ; Scarecrow disconnected
        Port_by_Tank[6]=0   ; Lion at CPI
        Port_by_Tank[7]=6   ; Luke in tank 7 to ant 6
        Port_by_Tank[8]=1   ; Tinman in tank 8 to ant 1 
        Port_by_Tank[9]=2   ; NASA in tank 9 to ant 2          
        Port_by_Tank[10]=7  ; Chewbacca in tank 10 to ant 7  
	Port_by_tank[11]=4  ; Vader patched to old Scarecrow's line (ant 4)
    end  
    ((shot ge 177468) and (shot lt 178730)): begin  
        Port_by_Tank[4]=5   ; Leia in tank 4 to ant 5
        Port_by_Tank[5]=0   ; Scarecrow disconnected
        Port_by_Tank[6]=0   ; Lion at CPI
        Port_by_Tank[7]=6   ; Luke in tank 7 to ant 6
        Port_by_Tank[8]=1   ; Tinman in tank 8 to ant 1 
        Port_by_Tank[9]=0   ; NASA at CPI          
        Port_by_Tank[10]=0  ; Chewbacca at CPI  
	Port_by_tank[11]=7  ; Vader patched to old Chewbacca's line (ant 7)
    end
    ((shot ge 178730) and (shot lt 178960)): begin
        Port_by_Tank[4]=5   ; Leia in tank 4 to ant 5
        Port_by_Tank[5]=0   ; Scarecrow disconnected
        Port_by_Tank[6]=0   ; Lion at CPI
        Port_by_Tank[7]=6   ; Luke in tank 7 to ant 6
        Port_by_Tank[8]=1   ; Tinman in tank 8 to ant 1 
        Port_by_Tank[9]=0   ; NASA at CPI          
        Port_by_Tank[10]=0  ; Chewbacca at CPI  
	Port_by_tank[11]=7  ; Vader patched to old Chewbacca's line (ant 7)
    	Port_by_tank[12]=9  ; TVader in tank 12 to top launch (ant 9)    	
    end  
    ((shot ge 178960) and (shot lt 179540)): begin
        Port_by_Tank[4]=7   ; Leia patched to 285L/300 (ant 4)
        Port_by_Tank[5]=0   ; Scarecrow disconnected
        Port_by_Tank[6]=0   ; Lion at CPI
        Port_by_Tank[7]=6   ; Luke in tank 7 to ant 6
        Port_by_Tank[8]=1   ; Tinman in tank 8 to ant 1 
        Port_by_Tank[9]=0   ; NASA at CPI          
        Port_by_Tank[10]=0  ; Chewbacca at CPI  
	Port_by_tank[11]=5  ; Vader in tank 11 to ant 5 replacing Leia
    	Port_by_tank[12]=9  ; TLeia in tank 12 to top launch (ant 9)
    end
    ((shot ge 179540) and (shot lt 180410)): begin
        Port_by_Tank[4]=5   ; Leia back to ant 5 
        Port_by_Tank[5]=0   ; Scarecrow disconnected
        Port_by_Tank[6]=0   ; Lion at CPI
        Port_by_Tank[7]=6   ; Luke in tank 7 to ant 6
        Port_by_Tank[8]=1   ; Tinman in tank 8 to ant 1 
        Port_by_Tank[9]=0   ; NASA at CPI          
        Port_by_Tank[10]=0  ; Chewbacca at CPI  
	Port_by_tank[11]=7  ; Vader to 285L/300
    end 
    ((shot ge 180410) and (shot lt 181650)): begin
        Port_by_Tank[4]=7   ; Leia patched to 285L/300 (ant 4)
        Port_by_Tank[5]=0   ; Scarecrow disconnected
        Port_by_Tank[6]=0   ; Lion at CPI
        Port_by_Tank[7]=6   ; Luke in tank 7 to ant 6
        Port_by_Tank[8]=1   ; Tinman in tank 8 to ant 1 
        Port_by_Tank[9]=0   ; NASA at CPI          
        Port_by_Tank[10]=0  ; Chewbacca at CPI  
	Port_by_tank[11]=0  ; Vader back to CPI
    	Port_by_tank[12]=9  ; TLeia in tank 12 to top launch (ant 9)
    end               
    ((shot ge 181650) and (shot lt 182350)): begin
        Port_by_Tank[4]=5   ; Leia to ant 5 
        Port_by_Tank[5]=0   ; Scarecrow disconnected
        Port_by_Tank[6]=0   ; Lion at CPI
        Port_by_Tank[7]=6   ; Luke in tank 7 to ant 6
        Port_by_Tank[8]=1   ; Tinman in tank 8 to ant 1 
        Port_by_Tank[9]=0   ; NASA at CPI          
        Port_by_Tank[10]=0  ; Chewbacca at CPI  
	Port_by_tank[11]=7  ; Vader to 285 L 
    end  
    ((shot ge 182350) and (shot lt 182689)): begin    ;need to update the shot 182689 after they make the switch
        Port_by_Tank[4]=5   ; Leia to ant 5 
        Port_by_Tank[5]=0   ; Scarecrow disconnected
        Port_by_Tank[6]=0   ; Lion at CPI
        Port_by_Tank[7]=6   ; Luke in tank 7 to ant 6
        Port_by_Tank[8]=1   ; Tinman in tank 8 to ant 1 
        Port_by_Tank[9]=2   ; NASA repaired          
        Port_by_Tank[10]=0  ; Chewbacca at CPI  
	Port_by_tank[11]=7  ; Vader to 285 L 
    end  
    ((shot ge 182690) and (shot lt 184631)): begin
	Port_by_Tank[4]=7   ; Leia patched to 285L/300 (ant 4)
        Port_by_Tank[5]=0   ; Scarecrow disconnected
        Port_by_Tank[6]=0   ; Lion at CPI
        Port_by_Tank[7]=6   ; Luke in tank 7 to ant 6
        Port_by_Tank[8]=1   ; Tinman in tank 8 to ant 1 
        Port_by_Tank[9]=2   ; NASA repaired         
        Port_by_Tank[10]=0  ; Chewbacca at CPI  
	Port_by_tank[11]=5  ; Vader in tank 11 to ant 5 replacing Leia
    	Port_by_tank[12]=9  ; TLeia in tank 12 to top launch (ant 9)
    end   
        ((shot ge 184631) and (shot lt 186122)): begin
        Port_by_Tank[4]=5   ; Leia to ant 5 
        Port_by_Tank[5]=0   ; Scarecrow disconnected
        Port_by_Tank[6]=0   ; Lion at CPI
        Port_by_Tank[7]=6   ; Luke in tank 7 to ant 6
        Port_by_Tank[8]=0   ; Tinman die 
        Port_by_Tank[9]=0   ; NASA at CPI          
        Port_by_Tank[10]=0  ; Chewbacca at CPI  
	Port_by_tank[11]=0  ; Vader at CPI
    end  
        ((shot ge 186122) and (shot lt 187329)): begin
        Port_by_Tank[4]=5   ; Leia to ant 5 
        Port_by_Tank[5]=4   ; R2D2 added 255R
        Port_by_Tank[6]=0   ; Lion at CPI
        Port_by_Tank[7]=6   ; Luke in tank 7 to ant 6
        Port_by_Tank[8]=0   ; Tinman die 
        Port_by_Tank[9]=0   ; NASA at CPI          
        Port_by_Tank[10]=0  ; Chewbacca at CPI  
	Port_by_tank[11]=0  ; Vader at CPI
    end  
        ((shot ge 187329) and (shot lt 187865)): begin
        Port_by_Tank[4]=7   ; Leia to ant 7, 285L 
        Port_by_Tank[5]=4   ; R2D2 added 255R
        Port_by_Tank[6]=0   ; Lion at CPI
        Port_by_Tank[7]=6   ; Luke in tank 7 to ant 6
        Port_by_Tank[8]=0   ; Tinman die 
        Port_by_Tank[9]=0   ; NASA at CPI          
        Port_by_Tank[10]=0  ; Chewbacca at CPI  
	Port_by_tank[11]=0  ; Vader at CPI
    end      
        ((shot ge 187865) and (shot lt 192589)): begin    
        Port_by_Tank[4]=7   ; Leia to ant 7, 285L 
        Port_by_Tank[5]=4   ; R2D2 added 255R
        Port_by_Tank[6]=0   ; Lion at CPI
        Port_by_Tank[7]=6   ; Luke in tank 7 to ant 6
        Port_by_Tank[8]=1   ; Yoda in tank 8 to ant 1 
        Port_by_Tank[9]=0   ; NASA at CPI          
        Port_by_Tank[10]=0  ; Chewbacca at CPI  
	Port_by_tank[11]=0  ; Vader at CPI
    	Port_by_tank[12]=9  ; TLeia in tank 12 to top launch (ant 9)
    	Port_by_tank[13]=10  ; TLuke in tank 13 to top launch (ant 10)
    end
        (shot ge 192589): begin    ;;;all these information need to be checked
        Port_by_Tank[4]=5   ; Leia to ant 5, 240L 
        Port_by_Tank[5]=4   ; R2D2 on 255R
        Port_by_Tank[6]=0   ; N/A
        Port_by_Tank[7]=6   ; Luke in tank 7 to ant 6, 240R
        Port_by_Tank[8]=1   ; Yoda in tank 8 to ant 1, 270L 
        Port_by_Tank[9]=0   ; N/A          
        Port_by_Tank[10]=0  ; N/A  
	Port_by_tank[11]=7  ; HANR in tank 11 to ant 7, 285L
    	Port_by_tank[12]=0  ; TLeia (tank 12) no longer in 300 top launch
    	Port_by_tank[13]=10 ; TLuke in tank 13 to 90 top launch (ant 10)
        Port_by_tank[14]=9  ; THANR in tank 14 to 300 top launch (ant 9) ; double check Gyro_in_Tank
    end            
	else:
endcase

Port_by_Tank=Port_by_Tank[1:n_elements(Port_by_Tank)-1]-1

;****** quantities derived from connection data above *********
; enter port name and number into Antenna structure
for kk=0, NumberOfAntennas-1 do begin
	loc=where(Ant_in_Port eq kk, count)
	if count eq 1 then begin
		Antenna_data[kk].Port=Port_Names[loc[0]]
		Antenna_data[kk].Port_Num=loc[0]+1
	endif else begin
		Antenna_data[kk].Port=''
		Antenna_data[kk].Port_Num=-1
	endelse
endfor
 
; AntInPort is the name of the launcher which is in each antenna location
AntInPort=Antenna_data[Ant_in_Port].DwgNo

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; GyroToAnt is the gyro name which is plumbed to each antenna location

; Name of gyrotron connected to each port
GyroToAnt=strarr(NumberOfPorts)
for ix=0, NumberOfPorts-1 do begin				; ix is port number 0-7 which seesm to be the same as Ant #
	iy=where(Port_by_Tank eq ix)				; tank index iy is connected to port index ix
	if iy[0] ge 0 then $					; iy = -1 means no tank connecting to that port
		if gyro_in_tank[iy[0]] ge 0 then $		; gyro_in_tank = -1 means no gyro in that tank
			GyroToAnt[ix]=Gyro[gyro_in_tank[iy[0]]].GyroName
endfor

;**************************************************************
; Power supply data
; By tank: tank 1, tank2, ..., tank9
case 1 of
 	(shot le 134790): PowerSupply=['', '', '', '', '', '', '', '', '']
	((shot gt 134790) and (shot le 155914)): PowerSupply=['', '', '', '3C solo', '1B slave', '2B slave', '1A master', $
		'2A master', '3A solo', '4B solo'] 
	((shot gt 155914) and (shot lt 163600)): PowerSupply=['', '', '', '3C solo', '1B slave', '', '1A master', $
		'2A solo', '4A Solo', '4B solo']
	((shot ge 163600) and (shot lt 172980)): PowerSupply=['', '', '', '3C solo', '1B slave', '2A slave', '1A master', $
		'2A master', '4A solo', '4B solo']
	((shot ge 172980) and (shot lt 178730)): PowerSupply=['', '', '', '3C solo', '1B slave', '2A slave', '1A master', $
		'2A master', '4A solo', '4B solo', '3A solo']
	((shot ge 178730) and (shot lt 180870)): PowerSupply=['', '', '', '3C solo', '1B slave', '2A slave', '1A master', $
		'2A master', '4A solo', '4B solo', '3A solo', '3Atop solo']	
	((shot ge 180870) and (shot lt 186122)): PowerSupply=['', '', '', '3B solo', '1B slave', '2A slave', '1A ', $
		'2A ', '4A solo', '4B solo', '3A solo', '3Atop solo']	
	((shot ge 186122) and (shot lt 187329)): PowerSupply=['', '', '', '3B solo', '4B solo', '2A slave', '1A ', $
		'2A ', '4A solo', '', '3A solo', '3Atop solo']
	((shot ge 187865) and (shot lt 192589)) : PowerSupply=['', '', '', '4A solo', '4B solo', '', '1A ', $
		'2A', '', '', '', '4A solo T', '1A solo T'] 
        (shot ge 192589) : PowerSupply=['', '', '', '1B', '2B', '', '1A', $
       		 '2A', '', '', '3A', '', '1A leader T','3A'] 
endcase

;**************************************************************
; Data for the bends associated with a gyrotron tank.
; gamma are the angle of a bend relative to the plane of the previous bend, per
;   Doane convention.
; mir_type is the type of mirror: 1=first grooved mirror, 2=second grooved mirror,
;   3=delay due to anisotopy, 4=flat mirror, 0=dummy used to fill array
; delay are phase delay in deg associated with each anisotropy
; gmc* are the calculated Fourier coefficients associated with the grooved mirrors,
;   and gmc*corr are the corrections due to measurements 
; gm*SN are the serial numbers for the installed grooved mirrors
TankMir={TankMirData, gmc1:fltarr(6), gmc1corr:fltarr(6), gmc2:fltarr(6), gmc2corr:fltarr(6), $
	gm1SN:'', gm2SN:''}
TankMir=Replicate({TankMirData}, NumberOfTanks)

;****************************************************************
; Data characterizing the grooved mirrors associated with each tank:

TankMir[*].gmc1=[3.33223,-0.426187,-0.0607445,-0.00494972,0.000453017,0.000479512]
TankMir[*].gmc2=[1.87714,-0.377887,-0.00367177,0.00335997,0.000177044,-5.03162e-05]
TankMir[0].gmc1corr=[0.20, 0.20, 0.0, 0.0, 0.0, 0.0]
TankMir[1].gmc1corr=[0.18, 0.10, 0.0, 0.0, 0.0, 0.0]
TankMir[2].gmc1corr=[0.18, 0.05, 0.0, 0.0, 0.0, 0.0]
TankMir[3].gmc1corr=[0.00, 0.00, 0.0, 0.0, 0.0, 0.0]
TankMir[4].gmc1corr=[0.00, 0.00, 0.0, 0.0, 0.0, 0.0]
TankMir[5].gmc1corr=[0.00, 0.00, 0.0, 0.0, 0.0, 0.0]
TankMir[6].gmc1corr=[0.00, 0.00, 0.0, 0.0, 0.0, 0.0]
TankMir[7].gmc1corr=[0.00, 0.00, 0.0, 0.0, 0.0, 0.0]
TankMir[8].gmc1corr=[0.00, 0.00, 0.0, 0.0, 0.0, 0.0]

TankMir[0].gmc2corr=[0.12, 0.12, 0.0, 0.0, 0.0, 0.0]
TankMir[1].gmc2corr=[0.00, 0.00, 0.0, 0.0, 0.0, 0.0]
TankMir[2].gmc2corr=[-.15, 0.10, -.1, 0.0, 0.0, 0.0]
TankMir[3].gmc2corr=[0.11, 0.00, 0.0, 0.0, 0.0, 0.0]
TankMir[4].gmc2corr=[0.00, 0.00, 0.0, 0.0, 0.0, 0.0]
TankMir[5].gmc2corr=[0.00, 0.00, 0.0, 0.0, 0.0, 0.0]
TankMir[6].gmc2corr=[0.11, 0.00, 0.0, 0.0, 0.0, 0.0]
TankMir[7].gmc2corr=[0.00, 0.00, 0.0, 0.0, 0.0, 0.0]
TankMir[8].gmc2corr=[0.00, 0.00, 0.0, 0.0, 0.0, 0.0]

TankMir[0].gm1SN='1-1'
TankMir[1].gm1SN='2-1'
TankMir[2].gm1SN='3-1'
TankMir[3].gm1SN='4-1'
TankMir[4].gm1SN='5-1'
TankMir[5].gm1SN='6-1'
TankMir[6].gm1SN='7-1'
TankMir[7].gm1SN='8-1'
TankMir[8].gm1SN=''

TankMir[0].gm2SN='1-2'
TankMir[1].gm2SN='2-2'
TankMir[2].gm2SN='3-2'
TankMir[3].gm2SN='4-2'
TankMir[4].gm2SN='5-2'
TankMir[5].gm2SN='6-2'
TankMir[6].gm2SN='7-2'
TankMir[7].gm2SN='8-2'
TankMir[8].gm2SN=''

case 1 of 
	(shot gt 109500): begin
		; moved polarizers from Dorothy (Tank 2) to Lion (Tank 6) 2/2002
		TankMir[5].gmc1corr=[0.18, 0.10, 0.0, 0.0, 0.0, 0.0]
		TankMir[5].gmc2corr=[0.00, 0.00, 0.0, 0.0, 0.0, 0.0]
		TankMir[5].gm1SN='2-1'
		TankMir[5].gm2SN='2-2'
		; moved polarizers from Toto (Tank 3) to Scarecrow (Tank 5) 2/2002
		TankMir[4].gmc1corr=[0.18, 0.05, 0.0, 0.0, 0.0, 0.0]
		TankMir[4].gmc2corr=[-.15, 0.10, -.1, 0.0, 0.0, 0.0]
		TankMir[4].gm1SN='3-1'
		TankMir[4].gm2SN='3-2'
	end
	else:
endcase

;**************** transmission line data *******************
; nomenclature for mir_type_vals is 1=1st grooved mirror, 2=2nd grooved mirror,
; 3=phase delay due to anisotropy (deg), 4=flat mirror, 0=do nothing
; gamma_vals (deg) are the planes of the mitre bends

; 20111202: forget about anisotropy 'delays', set them all to 0

mir_type_vals=intarr(NumberOfTanks, NumberOfPorts, MaxNumMiters)
gamma_vals=   fltarr(NumberOfTanks, NumberOfPorts, MaxNumMiters)
delay_vals=   fltarr(NumberOfTanks, NumberOfPorts, MaxNumMiters)

; Tank 1 (Katya) to Antenna 1
case 1 of
	(shot lt 111689): begin 
		nmir=8
		mir_type_vals[0,0,0:nmir-1] = [4,     1,     2 ,    4,    4,    4,    4,   4 ]
		gamma_vals[0,0,0:nmir-1] =    [-2.,  180.,  272.,  180., 247.6, 90., 180., 180.]
	end
	(shot ge 111689): begin
		nmir=8	
		mir_type_vals[0,0,0:nmir-1] = [4,     1,     2 ,    4,     4,   4,    4,   4 ]
		gamma_vals[0,0,0:nmir-1] =    [0.,  180.,  270.,  180.,  246.8, 91., 180., 180.]
	end	
endcase	

; Tank 1 to Antenna 3
nmir=8
mir_type_vals[0,2,0:nmir-1] = [4,     1,     2 ,    4,     4,     4,    4,    4 ]
gamma_vals[0,2,0:nmir-1] =   [-2.,  180.,  270.,    180., 247.2, 75.3, 180., 180.]

; Tank 2 (Dorothy) to Antenna 2
nmir=8
mir_type_vals[1,1,0:nmir-1] = [4,     1,     2,    4,      4,   4,    4,   4 ]
gamma_vals[1,1,0:nmir-1] =    [0., 180.,  270., 180.,  247.6, 90., 180., 180.]

; Tank 3 (Toto) to Antenna 2
nmir=9
mir_type_vals[2,1,0:nmir-1] = [4,     1,     4 ,    2,     4,     4,    4,    4,   4 ]
gamma_vals[2,1,0:nmir-1] =    [0.,  270.,   90.,   180.,  180., 247.6, 90.0, 180., 180.]

; Tank 3 to Antenna 3
nmir=9
mir_type_vals[2,2,0:nmir-1] = [4,     1,     4 ,    2,     4,     4,    4,    4,   4 ]
gamma_vals[2,2,0:nmir-1] =    [0.,  270.,   90.,  180.,  180., 247.6, 75.3, 180., 180.]

; Tank 4 (Scarecrow, later Tinman) to Antenna 1
nmir=15
mir_type_vals[3,0,0:nmir-1] = [4, 1, 4, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4]
gamma_vals[3,0,0:nmir-1] = [0., 270., 90., 262., 180., 278., 130.6, 180., 224.5, 180., 184.9, 247.6, 90., 180., 180.]

; Tank 4 to Antenna 2
case 1 of
	(shot lt 107235): begin
		nmir=15
		mir_type_vals[3,1,0:nmir-1] = [4, 1, 4, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4]
		gamma_vals[3,1,0:nmir-1]= [0., 270., 90., 262., 180., 278., 130.6, 180., 224.5, 180., 184.9, 247.6, 90., 180., 180.]
	end
	((shot ge 107235) and (shot lt 117305)): begin
		nmir=15
		mir_type_vals[3,1,0:nmir-1] = [4, 1, 4, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4]
		gamma_vals[3,1,0:nmir-1] = [0., 270., 90., 262., 180., 278., 90., 180., 270., 180., 180., 247.6, 90., 180., 180.]
		if shot le 107852 then begin	; mistakenly had polarizers from Toto in line for June 2001
			mir_type_vals[3,1,7] = 5	; third pol of type 1	
			mir_type_vals[3,1,9] = 6	; fourth pol of type 2
		endif
	end
	(shot ge 117305): begin
		nmir=15
		mir_type_vals[3,1,0:nmir-1] = [4, 1, 4, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4]
		gamma_vals[3,1,0:nmir-1] = [0., 270., 90., 261.7, 180., 278.3, 130.7, 180., 223.9, 180., 185.3, 246.4, 90., 180., 180.]
	end
endcase

; Tank 4 to Antenna 4
nmir=13
mir_type_vals[3,3,0:nmir-1]=[4,     1,     4 ,    2,     4,     4,     4,    4,     4,     4,    4,    4,    4]
gamma_vals[3,3,0:nmir-1] =  [0.,  270.,   90., 262.,  180., 278.0, 127.9, 180., 232.1, 247.6, 75.3, 180., 180.]

; Tank 4 to Antenna 5, per Yuri/Mirela on Dec 16 2010
nmir=11
mir_type_vals[3,4,0:nmir-1] = [4,   4,   1,    2,    4,    4,   4,     4,    4,    4,    4]
gamma_vals[3,4,0:nmir-1] =    [0., 270., 90., 270., 164.4, 0., 164.4, 154.5, 62., 180., 180.] 

; Tank 4 to Antenna 6
case 1 of
	(shot lt 105688): begin
		nmir=13
		mir_type_vals[3,5,0:nmir-1] = [ 4,    1,   4,    2,    4,    4,     4,    4,     4,     4,    4,    4,    4]
		gamma_vals[3,5,0:nmir-1] =  [0., 270., 90., 262., 180., 278., 130.6, 180., 229.4, 247.6,  61., 180., 180.]
	end
	((shot ge 105688) and (shot lt 111689)) : begin
		nmir=13
		mir_type_vals[3,5,0:nmir-1] = [ 4,    1,   4,    2,    4,    4,     4,    4,     4,     4,    4,    4,    4]
		gamma_vals[3,5,0:nmir-1]=  [0., 270., 90., 262., 180., 278., 130.6, 180., 229.4, 246.0,  61., 180., 180.]
	end
	((shot ge 111689) and (shot lt 127586)): begin
		nmir=13
		mir_type_vals[3,5,0:nmir-1] = [ 4,    1,   4,    2,    4,    4,     4,    4,     4,     4,    4,    4,    4]
		gamma_vals[3,5,0:nmir-1] =  [0., 270., 90., 261.7, 180., 278.3, 130.7, 180., 229.3, 247.9,  60., 180., 180.]
	end
	(shot ge 127586): begin
		nmir=13
		gamma_vals[3,5,0:nmir-1] =  [0., 270., 90., 261.9, 180., 278.1, 130.9, 180., 229.1, 247.6,  60., 180., 180.]
		mir_type_vals[3,5,0:nmir-1] = [ 4,    1,   4,    2,    4,    4,     4,    4,     4,     4,    4,    4,    4]
	end
endcase

; Tank 4 to Antenna 7
case 1 of
	(shot lt 187865): begin 
		nmir=12
		mir_type_vals[3,6,0:nmir-1] = [4,   4,   1,    2,    4,    4,   4,     4,    4,   4,    4,    4]
		gamma_vals[3,6,0:nmir-1] =    [0., 270., 90., 270., 160., 0., 70., 90., 68., 105., 180., 180.] ; this was used for all LEIA top launch days
		;;gamma_vals[3,6,0:nmir-1] =    [0., 270., 90., 270., 164.4, 0., 69.5, 90., 68., 105., 180., 180.] ; 160 was 164.4 for same initial run; before March 2022
	end
	(shot ge 187865): begin   ; Leia to 285L, info from Yuri on March 2, 2022
		nmir=12
		mir_type_vals[3,6,0:nmir-1] = [4,   4,   1,    2,    4,    4,   4,     4,    4,   4,    4,    4]
		;gamma_vals[3,6,0:nmir-1] =    [0., 270., 90., 270., 160., 0., 70., 90., 68., 115., 180., 180.]
		gamma_vals[3,6,0:nmir-1] =    [0., 270., 90., 270., 160., 0., 75, 90., 68., 105., 180., 180.]  ;info from Mike Ross on March 17, 2022; need to change the shot range starting from 189512 for this new gamma_vals once varified experimentally
	end
endcase


; Tank 12 to antenna 9 Top Launch 300,
case 1 of
	(shot lt 187865): begin 
		; 164.4 and 69.5 were used on July 8,9,16,17 2019; but then Mirela sent out a new calib on 16th with 160. and 70. without testing.
		nmir=14
		mir_type_vals[11,8,0:nmir-1] = [4,   4,   1,    2,    4,    4,   4,     4,   4,    4,   4,  4,  4,    4]
		gamma_vals[11,8,0:nmir-1] =    [0., 270., 90., 270., 164.4, 0., 69.5, 90., 68., 105.,272.,93.,83., 180.] ; this was used for all LEIA top launch days
		;;gamma_vals[11,8,0:nmir-1] =    [0., 270., 90., 270., 164.4, 0., 69.5, 90., 68., 105.,272.,93.,83., 180.] ; 160 was 164.4 for same initial run
		;;tested Top launch run for Vader had the pit miters 158,  105, 272,  93, 83 - used the 115 from table instead of 105
		;;Yuri's table has Leia's Top Launch run             68,   115, 272, 93, 75
	end
	(shot ge 187865): begin   ; Leia to 300 top launch, info from Yuri on March 2, 2022
		nmir=14
		mir_type_vals[11,8,0:nmir-1] = [4,   4,   1,    2,    4,    4,   4,     4,   4,    4,   4,  4,  4,    4]
		;gamma_vals[11,8,0:nmir-1] =    [0., 270., 90., 270., 160, 0., 69.5, 90., 68., 115.,272.,93.,75., 180.] ; info from Yuri on March 2, 2022
		gamma_vals[11,8,0:nmir-1] =    [0., 270., 90., 270., 160, 0., 75, 90., 68., 105.,272.,93.,89., 180.];info from Mike Ross on March 17, 2022; need to change the shot range starting from 189512 for this new gamma_vals once varified experimentally
	end
endcase


; Tank 5 to antenna 3
case 1 of
	(shot lt 127586): begin
		nmir=14
		gamma_vals[4,2,0:nmir-1]=[0., 180., 90., 0., 262.2, 180., 277.8, 135., 180., 225., 247.2, 78., 180., 180.]
		mir_type_vals[4,2,0:nmir-1] = [4, 1, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4]
	end
	(shot gt 127586): begin
		nmir=14
		gamma_vals[4,2,0:nmir-1] =    [0., 180., 90., 0., 262.2, 180., 277.8, 134.7, 180., 225.3, 246.6, 78., 180., 180.]
		mir_type_vals[4,2,0:nmir-1] =  [4,   1,    4,  4,   2,     4,     4,     4,    4,     4,    4,     4,   4,      4]
	end
endcase

; Tank 5 to antenna 4, per Yuri/Mirela on Dec 16 2010
nmir=11
mir_type_vals[4,3,0:nmir-1] = [4,   4,   1,    2,    4,    4,   4,     4,     4,   4,    4]
gamma_vals[4,3,0:nmir-1] =    [0., 270., 90., 270., 164.4, 0., 164.4, 157.4, 76., 180., 180.] 

; Tank 6 to Antenna 3, per Yuri/Mirela on Dec 16, 2010   ;*Check the waveguide angles for Lion to 255L for 2015 installation after repair
nmir=12
gamma_vals[5,2,0:nmir-1] = [  0., 180., 90., 180., 90., 164.4, 0., 164.4, 156.6, 78., 180.,  180.]
; Revised 4/7 per email from Mirela 
mir_type_vals[5,2,0:nmir-1]=[4,   4,   1,    4,    2,     4,    4,   4,    4,    4,   4,     4 ] 

; Tank 6 to Antenna 5
case 1 of
	(shot lt 127586): begin
		; measured 25Feb2002 by Lohr
		nmir=16
		gamma_vals[5,4,0:nmir-1] =    [0., 180., 90., 180., 82.3, 180., 20.5, 180., 77.2, 2.8, 180., 357.2, 248., 62., 180., 180.]
		mir_type_vals[5,4,0:nmir-1] = [4,   1,    4,    4,    2,    4,   4,    4,    4,    4,   4,     4,    4,    4,   4,     4 ] 
	end
	(shot gt 127586): begin
		nmir=16
		gamma_vals[5,4,0:nmir-1] = [0., 180., 90., 180., 82.3, 180., 20.5, 180., 77.2, 2.8, 180., 357.2, 248., 62., 180., 180.]
		mir_type_vals[5,4,0:nmir-1] = [4,   1,    4,    4,    2,    4,   4,    4,    4,    4,   4,     4,    4,    4,   4,     4 ] 
	end
endcase

; Tank 6 to Antenna 2
nmir=14
gamma_vals[5,1,0:nmir-1] =    [0., 180., 90., 180., 82.1, 180., 21.3, 180., 76.6, 180., 246.4, 90., 180., 180.]
mir_type_vals[5,1,0:nmir-1] = [4,   1,    4,    4,    2,    4,   4,    4,    4,    4,   4,     4,    4,    4]

; Tank 7 to Antenna 1
nmir=16
gamma_vals[6,0,0:nmir-1]=     [0., 270., 270., 180., 259.3, 180., 280.7, 123.5, 180., 223.4, 180., 193.1, 247.6, 90., 180., 180.] 
mir_type_vals[6,0,0:nmir-1] = [4,   1,    4,    4,    2,     4,     4,     4,    4,     4,    4,     4,    4,    4,    4,     4 ]

; Tank 7 to Antenna 2 (268.6 deg)
nmir=16
gamma_vals[6,1,0:nmir-1] =    [0., 270., 270., 180., 259.3, 180., 280.7, 123.5, 180., 250.2, 180., 166.3, 248., 90., 180., 180.] 
mir_type_vals[6,1,0:nmir-1] = [4,   1,    4,    4,    2,     4,     4,     4,    4,     4,    4,     4,    4,    4,    4,     4 ]

; Tank 7 to Antenna 3
case 1 of
	(shot lt 109500): begin
		nmir=16
		gamma_vals[6,2,0:nmir-1] =    [0., 270., 270., 180., 259.3, 180., 280.7, 123.5, 180., 223.4, 180., 193.1, 247.6, 75.3, 180., 180.] 
		mir_type_vals[6,2,0:nmir-1] = [4,   1,    4,    4,    2,     4,     4,     4,    4,     4,    4,     4,     4,    4,    4,     4 ]
	end
	(shot ge 109500): begin
		nmir=16
		gamma_vals[6,2,0:nmir-1] =    [0., 270., 270., 180., 259.3, 180., 280.7, 123.5, 180., 250., 180., 166., 247., 78., 180., 180.]
		mir_type_vals[6,2,0:nmir-1] = [4,   1,    4,    4,    2,     4,     4,     4,    4,    4,    4,    4,   4,    4,    4,     4 ]
	end
endcase 

; Tank 7 to Antenna 4
case 1 of 
	(shot lt 123575): begin	; per Lohr file polar_angles_04.xls, 17March2004
		nmir=14
		gamma_vals[6,3,0:nmir-1] =    [0., 270., 270., 180., 259.7, 180., 280.3, 123.7, 180., 236.3, 247.7, 76., 180., 180.]
		mir_type_vals[6,3,0:nmir-1] = [4,   1,    4,    4,    2,     4,     4,     4,    4,     4,      4,   4,   4,    4  ]
	end
	((shot ge 123575) and (shot lt 127586)): begin	; per polar_angles.xls by Gorelov 5/17/2006
		nmir=14
		gamma_vals[6,3,0:nmir-1] =    [0., 180., 270., 180., 259.7, 180., 280.3, 123.7, 180., 236.3, 247.7, 76., 180., 180.]
		mir_type_vals[6,3,0:nmir-1] = [4,   4,    1,    4,    2,     4,     4,     4,    4,     4,      4,   4,   4,    4  ]
	end
	(shot ge 127586): begin
		nmir=14
		gamma_vals[6,3,0:nmir-1] = [0., 180., 270., 180., 259.8, 180., 280.2, 123.7, 180., 236.3, 247.4, 76., 180., 180.]
		mir_type_vals[6,3,0:nmir-1] = [4,   4,    1,    4,    2,     4,     4,     4,    4,     4,      4,   4,   4,    4  ]
	end
endcase

; Tank 7 to Antenna 5
case 1 of
	(shot lt 105688): begin
		nmir=14
		gamma_vals[6,4,0:nmir-1]=     [0., 270., 270., 180., 259.3, 180., 280.7, 123.5, 180., 236.5, 247.6, 60.46, 180., 180.]
		mir_type_vals[6,4,0:nmir-1] = [4,   1,    4,    4,    2,     4,     4,     4,    4,     4,      4,    4,    4,    4  ]
	end
	((shot ge 105688) and (shot lt 106735)): begin
		nmir=14
		gamma_vals[6,4,0:nmir-1] =    [0., 270., 270., 180., 259.3, 180., 280.7, 123.5, 180., 236.5, 243.0, 60.46, 180., 180.]
		mir_type_vals[6,4,0:nmir-1] = [4,   1,    4,    4,    2,     4,     4,     4,    4,     4,      4,    4,    4,    4  ]
	end
	((shot ge 106735) and (shot lt 111689)): begin
		nmir=14
		gamma_vals[6,4,0:nmir-1]=     [0., 270., 270., 180., 259.3, 180., 280.7, 123.5, 180., 236.5, 247.6, 60.46, 180., 180.]
		mir_type_vals[6,4,0:nmir-1] = [4,   1,    4,    4,    2,     4,     4,     4,    4,     4,      4,    4,    4,    4  ]
	end
	(shot gt 111689): begin	; From Gorelov, 9/2002
		nmir=14
		gamma_vals[6,4,0:nmir-1] =    [0., 270., 270., 180., 259.7, 180., 280.3, 124.3, 180., 235.7, 244.9, 62., 180., 180.]
		mir_type_vals[6,4,0:nmir-1] = [4,   1,    4,    4,    2,     4,     4,     4,    4,     4,     4,   4,    4,    4  ]
	end
endcase

; Tank 7 to Antenna 6 ;;;per Yuri/Mirela on Dec 16 2010 ; 
;;;Luke tank 7 to ant 6 240R, per Yuri/Mirela on March 2, 2022 
nmir=12
gamma_vals[6,5,0:nmir-1] =    [0.0,  180., 270.,  180., 270., 164.4, 0.0, 164.4, 157.6, 60., 180., 180.]
mir_type_vals[6,5,0:nmir-1] = [ 4,    4,    1,     4,    2,    4,     4,    4,    4,    4,    4,    4  ]

;Luke to Top launch
;; Tank 13 to antenna 10 Top Launch 90deg, info from Yuri on March 2, 2022
nmir=18
mir_type_vals[12,9,0:nmir-1] = [4,   4,    1,     4,    2,    4,     4,   4,    4,    4,    4,    4,   4,    4,    4,    4,    4,   4]
gamma_vals[12,9,0:nmir-1] =    [0.0, 180., 270.,  180., 270., 164., 0.0, 86.3, 177.,  168., 93.2, 94., 180., 176., 270., 274,  202, 180.]


; Tank 8 to Antenna 1, per Yuri/Mirela on Dec 16 2010
nmir=12
gamma_vals[7,0,0:nmir-1] =    [354.8, 180., 275.2, 180., 270., 164.4, 0.0, 164.4, 156.8, 90., 180., 180.]
mir_type_vals[7,0,0:nmir-1] = [  4,    4,    1,     4,    2,    4,     4,     4,     4,   4,   4,    4  ]

; Tank 8 to Antenna 1, per Yuri /Mike Ross on March 28, 2022
nmir=12
gamma_vals[7,0,0:nmir-1] =    [354.8, 180., 275.2, 180., 90., 164.4, 0.0, 164.4, 156.8, 90., 180., 180.]
mir_type_vals[7,0,0:nmir-1] = [  4,    4,    1,     4,    2,    4,     4,     4,     4,   4,   4,    4  ]

; Tank 8 to Antenna 2
nmir=16
gamma_vals[7,1,0:nmir-1] =    [0., 270., 270., 180., 260.5, 180., 279.5, 120.5, 180., 234.6, 180., 184.9, 247.6, 90., 180., 180.]
mir_type_vals[7,1,0:nmir-1] = [4,   1,    4,    4,     2,    4,     4,     4,    4,     4,    4,     4,     4,   4,    4,     4 ]

;Tank 8 to antenna 3 (rerouted to go from Tinman to old Lion's line in May 2014)
nmir=12
gamma_vals[7,2,0:nmir-1] =    [354.8, 180., 275.2, 180., 270., 164.4, 0.0, 164.4, 156.8, 78., 180., 180.]
mir_type_vals[7,2,0:nmir-1] = [  4,    4,    1,     4,    2,    4,     4,     4,     4,   4,   4,    4  ]

; Tank 8 to Antenna 4
case 1 of
	(shot lt 105688): begin
		nmir=15
		mir_type_vals[7,3,0:nmir-1] = [4,   1,    4,    4,     2,    4,     4,     4,    4,     4,     4,     4,    4,    4,    4  ]
		gamma_vals[7,3,0:nmir-1] =    [0., 270., 270., 180., 260.5, 180., 279.5,  38.4,  90.,   90., 231.8, 247.6, 75.3, 180., 180.]
	end
	((shot ge 105688) and (shot lt 109500)): begin
		nmir=15
		mir_type_vals[7,3,0:nmir-1] = [4,   1,    4,    4,     2,    4,     4,     4,    4,     4,     4,     4,    4,    4,    4]
		gamma_vals[7,3,0:nmir-1]  =   [0., 270., 270., 180., 260.5, 180., 279.5,  38.4,  90.,   90., 231.8, 248.0, 75.3, 180., 180.]
	end
	((shot ge 109500) and (shot lt 111689)): begin
		nmir=14
		gamma_vals[7,3,0:nmir-1] =    [0., 270., 270., 180., 260.5, 180., 279.5, 118., 180., 242., 248., 76., 180., 180.]
		mir_type_vals[7,3,0:nmir-1] = [4,   1,    4,    4,     2,    4,     4,    4,    4,    4,    4,    4,   4,    4  ]
	end
	(shot ge 111689): begin	; from Gorelov, 9.2002
		nmir=14
		gamma_vals[7,3,0:nmir-1] =    [0., 270., 270., 180., 260.1, 180., 279.9, 118.2, 180., 241.8, 247.7, 76., 180., 180.]
		mir_type_vals[7,3,0:nmir-1] = [4,   1,    4,    4,     2,    4,     4,    4,     4,     4,     4,    4,   4,    4  ]
	end
endcase

; Tank 8 to antenna 5
nmir=14
gamma_vals[7,4,0:nmir-1] =   [355., 180., 275., 180., 260.4, 180., 279.6, 119.2, 180., 240.8, 244.5, 62., 180., 180.]
mir_type_vals[7,4,0:nmir-1] =  [4,    4,    1,    4,     2,    4,     4,     4,    4,     4,     4,    4,   4,    4  ]

; Tank 8 to Antenna 6 (241.4 deg)
nmir=14
gamma_vals[7,5,0:nmir-1] =    [0., 270., 270., 180., 260.5, 180., 279.5, 120.5, 180., 239.5, 247.6, 60.95, 180., 180.]
mir_type_vals[7,5,0:nmir-1] = [4,   1,    4,    4,     2,    4,     4,    4,     4,     4,     4,     4,    4,    4  ]

; Tank 9 to antenna 1
case 1 of
	(shot lt 134000): begin
		nmir=15
		gamma_vals[8,0,0:nmir-1] =    [0., 270., 90., 0., 180., 83.1, 180., 276.9, 130., 180., 230., 246.8, 91., 180., 180.]
		mir_type_vals[8,0,0:nmir-1] = [4,   4,    4,  4,   1,    2,    4,    4,     4,    4,    4,     4,    4,    4,   4  ]
	end
	(shot ge 134000): begin	; per Mirela spreadsheet 20080724; first value updated from 0 on 20090512
		nmir=14
		gamma_vals[8,0,0:nmir-1] =    [345.8, 0.0, 74.1, 179.0, 84.8, 180.0, 276.9, 130.0, 180.0, 230.0, 246.8, 90.0, 180.0, 180.0]
		mir_type_vals[8,0,0:nmir-1] = [  4,    4,    4,    1,     2,    4,     4,     4,     4,     4,     4,    4,     4,     4  ]
	end
endcase

; Tank 9 to Antenna 2, per Yuri/Mirela on Dec 16 2010
case 1 of
  (shot lt 157866): begin
    nmir=12
    gamma_vals[8,1,0:nmir-1] =    [345.8,  0.0,  74.6,  180.,  91.6,  164.4,  0.0,  164.4,  156.4,  90.,  180.,  180.]
    mir_type_vals[8,1,0:nmir-1] = [ 4,      4,     4,    1,     2,      4,     4,     4,     4,      4,    4,     4 ]
  end
  (shot ge 157866): begin  ; NASA gyrotron in tank 9 June 2014
    nmir=12
    gamma_vals[8,1,0:nmir-1] =    [348,  0.0,  76.8,  180.,  91.6,  164.4,  0.0,  164.4,  156.4,  90.,  180.,  180.]
    mir_type_vals[8,1,0:nmir-1] = [ 4,      4,     4,    1,     2,      4,     4,     4,     4,      4,    4,     4 ]  
  end
endcase

; Tank 10 to Antenna 7, w/g run completed, May 30 2012
nmir=10
gamma_vals[9,6,0:nmir-1]=   [0.0,  180.0,  0.0,  159.53,  0.0,  159.53,  158.3,  105.0,  180.0,  180.0] 
mir_type_vals[9,6,0:nmir-1]=[ 4,     1,     2,     4,      4,      4,      4,       4,     4,      4 ]

; Tank 10 to Antenna 1, Chewbacca launcher meltdown, rerouted in the pit on July 10 2014 to previous Tinman launcher
nmir=10
gamma_vals[9,0,0:nmir-1]=   [0.0,  180.0,  0.0,  159.53,  0.0,  159.53,  158.3,  105.0,  180.0,  180.0] 
mir_type_vals[9,0,0:nmir-1]=[ 4,     1,     2,     4,      4,      4,      4,       4,     4,      4 ]

; Tank 11 to Antenna 8, w/g run completed, Oct 3 2017  - Never used, never checked polarization with these values
nmir=10
gamma_vals[10,7,0:nmir-1]=   [0.0,  103.75,  0.0,  173.1,  0.0,  159.34,  158.0,  105.0,  180.0,  180.0] 
mir_type_vals[10,7,0:nmir-1]=[ 4,     1,     2,     4,      4,      4,      4,       4,     4,      4 ]

; Tank 11 to antenna 4 replacing Scarecrow
nmir=12
gamma_vals[10,3,0:nmir-1]=   [0.0,  103.75,  0.0,  187.0,  0.0,  159.34,  84.3,  180.0,	74.1,	67.9,  180.0,  180.0] 
mir_type_vals[10,3,0:nmir-1]=[ 4,     1,     2,     4,      4,      4,      4,       4,	 4,	 4,     4,      4 ]

; Tank 11 (Vader) to antenna 7 replacing Chewbacca, R+1 285
; using the same valves for Tank 11 (HanR replacing Vader) routed to R+1285 starting Jan 2023
nmir=10
;gamma_vals[10,6,0:nmir-1]=   [0.0,  103.75,  0.0,  187.1,  0.0,  159.34,  158,  105.0,  180.0,  180.0]  
gamma_vals[10,6,0:nmir-1]=   [0.0,  103.75,  0.0,  173.,  0.0,  159.34,  158,  105.0,  180.0,  180.0]  ; changed 187.1 to 173 after confirming by MR - Jan 27 2023
mir_type_vals[10,6,0:nmir-1]=[ 4,     1,     2,     4,      4,      4,      4,       4,	 4,	 4 ]  

; Tank 14 (THan) to antenna 9 Top Launch 300
if shot ge 192589 then begin   ; HanR to 300 top launch
	nmir=12
	mir_type_vals[13,8,0:nmir-1] = [ 4,    1,    2,    4,     4,      4,      4,       4,   4,  4,  4,    4]
	gamma_vals[13,8,0:nmir-1] =    [0.0,  103.75,  0.0,  173.,  0.0,  159.34,  158,  105.0, 272.,93.,89., 180.]
endif


;; Tank 12 (TVader) to antenna 9 Top Launch 300
;; 187 was used on June 25, 26 2019 but then Mirela sent out new calib on July 16 with 173 without testing but said from a better measurement
;; need to be checked before new top launch exp. with Vader
;nmir=12
;gamma_vals[11,8,0:nmir-1]=   [0.0,  103.75,  0.0,  187.0,  0.0,  159.34,  158,  105,	272,	93,	83,  180.0] 
;mir_type_vals[11,8,0:nmir-1]=[ 4,     1,     2,     4,      4,      4,      4,    4,	  4,	 4,      4,      4]

; Tank 11 (Vader) to antenna 5 replacing Leia, 240 port
nmir=12
gamma_vals[10,4,0:nmir-1]=   [0.0,  104.,  0.0,  173.0,  0.0,  159.,  90.,  180.0, 247, 62,  180.0,  180.0] 
mir_type_vals[10,4,0:nmir-1]=[ 4,     1,     2,     4,      4,      4,      4,       4,  4,  4,     4,      4 ]
;247 correct value instead of 154 in the initial table 93 degrees
;159 new was 164.4 in initial table 4.4 degrees
;173 in Yuri table was 187 in an older table 

; w/g efficiencies, per John Doane 10/10/2001
eta_mitrebend=0.9905			; mostly mode conversion 
eta_polarizing_mb=0.985			; larger mc for polarizers

; centerpost tile dimensions
mhsize=132.73   ; mm horizontal
mvsize=135.64   ; mm vertical

; get data about system
case id_type of
	'antenna': begin	; id is antenna number [0,1,...]
		AN=id
		PN=Antenna_data[AN].port_num
		k=where(Port_by_Tank eq PN-1)+1
		if k[0] ge 0 then TN=k[0] else return, {status:0}
		GN=Gyro_in_Tank[TN-1]
		GName=Gyro[GN].GyroName
	end
	'tank': begin ; id is tank number [1,2,...]
		TN=id
		PN=Port_by_Tank[TN-1]+1	; port number
		AN=Ant_in_Port[PN-1]	; antenna number
		GName=GyroToAnt[PN-1]	; gyrotron number
		k=where(Gyro.GyroName eq GName)
		if k[0] ge 0 then GN=k[0] else return, {status:0}
	end
	'gyro': begin	; id is gyrotron number [0,1,...]
		GN=id
		TN=where(Gyro_in_Tank eq GN)+1
		PN=Port_by_Tank[TN-1]+1
		AN=Ant_in_Port[PN-1]
		GName=Gyro[GN].GyroName
	end
	'name': begin	; id is gyrotron name -- only first 3 letters needed, case insensitive
		GName=id
		k=where(strupcase(strmid(Gyro.GyroName,0,3)) eq strupcase(strmid(GName,0,3)))
		if k[0] ge 0 then GN=k[0] else return, {status:0}
		k=where(Gyro_in_Tank eq GN)
		if k[0] ge 0 then TN=k[0]+1 else return, {status:0}
		PN=Port_by_Tank[TN-1]+1
		AN=Ant_in_Port[PN-1]
	end
	else:
endcase

;********** Port limits data **********************************
; ALWAYS return the LATEST port maps; earlier ones irrelevant

Use_angles_directly=0
MaxNumberOfPortPoints=20
tilt_limits=lonarr(NumberOfAntennas, MaxNumberOfPortPoints)
facet_limits=lonarr(NumberOfAntennas, MaxNumberOfPortPoints)
np_lim=intarr(NumberOfAntennas) 	; number of points in port boundary

if not Use_angles_directly then begin	; use encoder counts directly

	; P2001_M1 
	np_lim[6]=9
	facet_limits[6,0:np_lim[6]-1]=[13468,	13468,	20467,	23126,	23126,	22133,	22133,	20226,	13468]
	tilt_limits[6,0:np_lim[6]-1] =[6704,	9414,	9763,	9196,	7993,	7504,	7272,	6704,	6704]
	
	; P2001_M2 
	np_lim[7]=9
	facet_limits[7,0:np_lim[7]-1]=[13100,	5999,	5050, 4200,	4100,	7991, 10250,	13100,	13100]
	tilt_limits[7,0:np_lim[7]-1] =[6800,	6800,	7107,	7695,	8523, 9200, 9200,	  8800,	  6800]
			
	; P2002_M1 
	np_lim[8]=9
	facet_limits[8,0:np_lim[8]-1]=[3652,	10695,	11906,	11906,	13808,	13808,	13400,	3652,	3652]
	tilt_limits[8,0:np_lim[8]-1] =[12924,	12924,	12987,	13097,	13900,	14997,	15732,	15732,	12924]

	; P2002_M2 
	np_lim[9]=10
	facet_limits[9,0:np_lim[9]-1]=[13502,	13496,	12001,	7002,	3152,	3157,	5001,	5899,	5899,	13502]
	tilt_limits[9,0:np_lim[9]-1] =[5589,	7694,	7898,	8093,	7494,	7195,	5698,	5707,	5589,	5589]

	; P2006_M1  
	np_lim[10]=9
	facet_limits[10,0:np_lim[10]-1]=[14999,	15091,	11987,	10011,	6092,	6092,	11008,	12991,	14999]
	tilt_limits[10,0:np_lim[10]-1] =[7600,	8469,	8898,	8898,	8299,	6473,	6473,	6604,	7600]
			
	; P2006_M2 
	np_lim[11]=10
	facet_limits[11,0:np_lim[11]-1]=[13925,	4598,	4598,	4298,	3551,	3551,	7003,	9003,	13925,	13925]
	tilt_limits[11,0:np_lim[11]-1] =[10762, 10762, 11099,  11099,  11998,  13266,  13568,  13523,   12601,  10762]

	; P2012_M1 
	np_lim[12]=11
	facet_limits[12,0:np_lim[12]-1]=[4499,	10992,	11551,	12190,	13178,	14294,	14264,	11910,	6082,	4499,	4499]
	tilt_limits[12,0:np_lim[12]-1] =[9105,	9105,	9160,	9405,	9405,	11198,	11198,	11595,	11595,	11166,	9105]

	; P2012_M2 
	np_lim[13]=8
	facet_limits[13,0:np_lim[13]-1]=[15180,	11321,	9110,	6764,	6764,	8881,	15180,	15180]
	tilt_limits[13,0:np_lim[13]-1] =[12334,	12928,	12928,	12606,	10872,	10321,	10321,	12334]


	; 300degree Top 
	np_lim[14]=4
	facet_limits[14,0:np_lim[14]-1]=[-1,	1,	1,	-1]
	tilt_limits[14,0:np_lim[14]-1] =[-1,	-1,	1,	1]


	; 90degree Top 
	np_lim[15]=4
	facet_limits[15,0:np_lim[15]-1]=[-1,	1,	1,	-1]
	tilt_limits[15,0:np_lim[15]-1] =[-1,	-1,	1,	1]

	;*******************************************************************************************
	; get the angles corresponding to the counts to insert into facet_lim_ang, tilt_lim_ang
	; run portlim_angle.pro 
	; Only have to do this when a port map changes
	; Then add the new angles to the angles table below
	; Then change Use_Angles_Directly to 1
	;********************************************************************************************
endif else begin	; use angles and convert to counts

	facet_lim_ang=fltarr(NumberOfAntennas, MaxNumberOfPortPoints)
	tilt_lim_ang=fltarr(NumberOfAntennas, MaxNumberOfPortPoints)
	
	; P2001_M1
	np_lim[6]=9
	facet_lim_ang[6,0:np_lim[6]-1]=[-24.5163, -24.5163, 10.0199, 24.0361, 24.0361, 18.5702, 18.5702, 8.83120, -24.5163]
	tilt_lim_ang[6,0:np_lim[6]-1] =[36.8804,   70.5830, 74.8469, 67.9107, 53.0419, 46.9388, 44.0313, 36.8804,  36.8804]

	; P2001_M2
	np_lim[7]=7
	facet_lim_ang[7, 0:np_lim[7]-1]=[-26.8893, -80.0516, -99.5998, -99.7054, -44.7325, -27.1330, -26.8893]
	tilt_lim_ang[7, 0:np_lim[7]-1] =[38.2689,   38.2689,  49.3267,  59.6181,  67.9598,  63.0391,  38.2689]
	
	; P2002_M1
	np_lim[8]=9
	facet_lim_ang[8, 0:np_lim[8]-1]=[-106.184, -41.3403, -33.5774, -33.5774, -22.6617, -22.6617, -24.8918, -106.184, -106.184]
	tilt_lim_ang[8, 0:np_lim[8]-1] =[112.673,   112.673,  113.412,  114.702,  124.063,  136.703,  145.075,  145.075,  112.673]

	; P2002_M2
	np_lim[9]=10
	facet_lim_ang[9, 0:np_lim[9]-1]=[-24.3291, -24.3621, -32.9979, -70.4136, -112.465, -112.401, -90.5180, -81.0597, -81.0597, -24.3291]
	tilt_lim_ang[9, 0:np_lim[9]-1] =[22.7087,   49.3142,  51.8589,  54.2858,   46.8136,  43.0646, 24.1020,  24.2169,  22.7087,  22.7087]

	; P2006_M1
	np_lim[10]=9
	facet_lim_ang[10, 0:np_lim[10]-1]=[-16.4284, -15.9616, -33.0831, -46.0642, -79.1219, -79.1219, -39.2637, -27.1840, -16.4284]
	tilt_lim_ang[10, 0:np_lim[10]-1] =[ 48.1397,  58.9499,  64.2468,  64.2468,  56.8436,  33.9590,  33.9590,  35.6167,  48.1397]

	; P2006_M2
	np_lim[11]=10
	facet_lim_ang[11, 0:np_lim[11]-1]=[-22.0319, -95.0087, -95.0087, -98.4547, -107.431, -107.431, -70.4044,  -53.5319, -22.0319,  -22.0319]
	tilt_lim_ang[11, 0:np_lim[11]-1] =[86.9558,   86.9558,  91.0084,  91.0084,  101.740,  116.679,  120.204,   119.679, 108.873,    86.9558]
	
	; P2012_M1
	np_lim[12]=11
	facet_lim_ang[12, 0:np_lim[12]-1]=[-96.1360, -39.3686, -35.7792, -31.8568,  -26.1286, -20.0720, -20.2299, -33.5529, -79.2215, -96.1360, -96.1360]
	tilt_lim_ang[12, 0:np_lim[12]-1] =[66.7932,   66.7932,  67.4688,  70.4728,   70.4728,  92.1959,  92.1959,  96.9435,  96.9435,  91.8122,  66.7932]

	; P2012_M2 
	np_lim[13]=8
	facet_lim_ang[13,0:np_lim[13]-1]=[-15.5118,  -37.2374, -52.7087, -72.6246,  -72.6246, -54.4797, -15.5118, -15.5118]	
	tilt_lim_ang[13,0:np_lim[13]-1] =[105.721,    112.720,  112.720,  108.932,   88.2804,  81.6280,  81.6280,  105.721]


	; 300degree Top  
	np_lim[14]=4
	facet_lim_ang[14,0:np_lim[14]-1]=[-1,	1,	1,	-1]	
	tilt_lim_ang[14,0:np_lim[15]-1] =[-1,   -1,     1,       1]


	; 90degree Top  
	np_lim[15]=4
	facet_lim_ang[15,0:np_lim[15]-1]=[-1,	1,	1,	-1]	
	tilt_lim_ang[15,0:np_lim[15]-1] =[-1,   -1,     1,       1]

	;*******************************************************************************************		
	; now convert from angles to encoder counts
	;j=where(abs(tilt_lim_ang[*,0]) gt 1.e-8, numpor)
	;if numpor gt 0 then for i=0, numpor-1 do begin
	;	k=j[i]
	;	for n=0, np_lim[k]-1 do begin
	;		Antenna_Counts, 999999, AN, tilt_lim_ang[k,n], facet_lim_ang[k,n], $
	;			tilt_lim, facet_lim, /antenna_num, $
	;			scan_coefs=reform(scan_coef[AN,*]), $
	;			crank_coefs=reform(crank_coef[AN,*])
	;		tilt_limits[k,n]=tilt_lim	; these are the counts to use for the port limits
	;		facet_limits[k,n]=facet_lim 
	;	endfor
	;endfor
	;*******************************************************************************************
endelse

; get the tank, port, etc numbers
gyro_prefix=gyro_prefixes[GN]

i=where(mir_type_vals[TN-1, PN-1, *] ne 0, count)
if count gt 0 then begin
	nmir=max(i)
	mir_types=reform(mir_type_vals[TN-1, PN-1, 0:nmir])
	gammas=reform(gamma_vals[TN-1, PN-1, 0:nmir])
	gm1=tankmir[TN-1].gmc1+tankmir[TN-1].gmc1corr
	gm2=tankmir[TN-1].gmc2+tankmir[TN-1].gmc2corr
endif else begin
	nmir=0
	mir_types=[0]
	gammas=[0.]
	gm1=[0.]
	gm2=[0.]
	np_lim[AN]=1
endelse

d=create_struct({ status:1, $
	First_Shot_Newest_Config:First_Shot_Newest_Config, $
	PowerSupply:PowerSupply[TN-1], $
	GyroNum:fix(reform(GN)), PortNum:fix(reform(PN)), $ ; gyro, port, antenna, tank numbers
	AntNum:fix(reform(AN)), TankNum:fix(reform(TN)), $ ; AN and GN start at 0, PN and TN at 1
	GyroName:Gyro[GN].GyroName, $		; gyrotron name
	Gyro_prefix:gyro_prefix, $
	gyro_prefixes:gyro_prefixes, $
	PortName:Port_Names[PN-1], $	; name of port
	AntennaName:antenna_data[AN].dwgno, $ 
	scan_coef:reform(scan_coef[AN,*]), $
	crank_coef:reform(crank_coef[AN,*]), $
	antenna_data:antenna_data[AN], $
	gyro:gyro[GN], $
	gm1:gm1, gm2:gm2, $
	mir_type:mir_types, $
	gamma:gammas, $
	delay:fltarr(n_elements(gammas)), $ ; dummy, anisotropy no longer in vogue
	np_lim:np_lim[AN]})

; Find CURRENT antenna number for this gyrotron to get port limits
if shot lt first_shot_newest_config and present_port_limits then begin
	ecpresent=echcal(1000000L, id, id_type=id_type, /present_port_limits)
	AN_PRESENT=ecpresent.antnum
	present_port_limits=0
endif else AN_PRESENT=AN

if np_lim[AN_PRESENT] gt 2 then d=create_struct( d, $
	{facet_limits:reform(facet_limits[AN_PRESENT, 0:np_lim[AN_PRESENT]-1]), $
	tilt_limits:reform(tilt_limits[AN_PRESENT, 0:np_lim[AN_PRESENT]-1])})

arrsca, d, dclean
return, dclean

end
