;;;;routine to return a data structure that contains the parameters needed by PCS and mirror control
;;;;Last modified by Xi Chen, 09/13/2015; 
; Added get_poloidal_limits, Ron Prater, 12/2/2015

@$ECHRES_PATH/echcal.pro
@$ECHRES_PATH/read_setup_file.pro
@$ECHRES_PATH/arrsca.pro
@$ECHRES_PATH/test_counts_valid.pro

FUNCTION writeECcalfile4pcs, gyroname_array, tor_count_array, pol_count_array

; gyroname_array=['SCA', 'LIO', 'NAS', ...]
; tor_count_array is an array of toroidal counts, in the order of the gyroname_array
;  toroidal counts are assumed fixed in time
; pol_count_array is an array of poloidal counts, in same order as gyroname_array
;  The poloidal counts are used to have a starting point that is within the port limits
; If the initial pol and tor counts are outside the allowed port space, the ECdata.status
;  is set to 0 and the ECdata.pol_cnts_upper_limit is set to -1

; Calls $ECHRES_PATH/test_counts_valid

; Returns 0 in case of failure, otherwise returns ECdata structure

if n_elements(gyroname_array) ne n_elements(tor_count_array) then begin
	print, '  !!! Gyrotron array MUST be same size as toroidal count array; aborting'
	return, 0
endif
if n_elements(gyroname_array) ne n_elements(pol_count_array) then begin
	print, '  !!! Gyrotron array MUST be same size as poloidal count array; aborting'
	return, 0
endif

;;;;always use the present hardware described in $ECHRES_PATH/present.setup
sd=read_setup_file(infile='$ECHRES_PATH/present.setup',setup_dir='$ECHRES_PATH/')

facet_limits=lonarr(16)
tilt_limits=lonarr(16)
scan_coef=fltarr(4)
crank_coef=fltarr(4)
ports=[240, 255, 270, 285]
ECdata=Replicate({ $
        status:0, $ 			; 0 for failure, 1 for success
		launcher:'', $			; ex. 240L
		gyroname:'', $			; ex. LEI
		tanknum:0, $			; same as gyrotron socket #, ex. 4
		powersupply:'', $		; ex. PS3C solo
		np_lim:0, $				; number of points in facet_limits and tilt_limits, ex. 9
		facet_limits:facet_limits, $	;array with 16 elemnts, port limits
		tilt_limits:tilt_limits, $	;array with 16 elemnts, port limits
		scan_coef:scan_coef, $	;array with 4 elements to convert poloidal counts to scan angle	
		crank_coef:scan_coef, $	;array with 4 elements to convert poloidal counts to scan angle
		pol_id:0., $			;pol angle of incident ray to steering mirror, ex. 13.2deg
		azi_id:0., $			;azi angle of incident ray to steering mirror, ex. 179.2deg	
		antenna_inclinationd:0., $ ;antenna_inclination angle, ex. 22.3deg
		offset_angle:0., $		;offset angle, ex. -1.4deg
		r_pivot:0., $			;radius of mirror, ex. 2.3999 m
		z_pivot:0., $			;height of mirror, ex. 0.6794 m
		phi_pivot:0., $			;toroidal angle of mirror position, ex. 241.4
		freq:0., $				;gyrotron frequecy, ex. 110 GHz	
		tor_cnts:0L, $			;toroidal counts for which the poloidal steering limits are calculated
		pol_cnts_upper_lim:0L, $;upper limit allowed on poloidal counts due to port limits
 		pol_cnts_lower_lim:0L $	;lower limit allowed on poloidal counts due to port limits
	},sd.num_sys)
	
FOR i=0, sd.num_sys-1 DO BEGIN

	gy=echcal(sd.shot,sd.gyroname[i])	
	IF gy.status eq 0 then begin
		print, 'Something is wrong between the present.setup and echcal.pro; gyro not found: '+sd.gyroname[i]
		break
	ENDIF

	a=uint(strmid(gy.portname,0,3))
	tmp=min(abs(ports-a),ind)
	b=ports(ind)
	IF a GT b THEN ECdata[i].launcher=strtrim(string(b),1)+'L' $
	ELSE ECdata[i].launcher=strtrim(string(b),1)+'R'

	ECdata[i].gyroname=gy.gyro_prefix
	ECdata[i].tanknum=gy.tanknum
	ECdata[i].powersupply='PS'+gy.powersupply
	ECdata[i].np_lim=gy.np_lim
	ECdata[i].facet_limits=gy.facet_limits
	ECdata[i].tilt_limits=gy.tilt_limits
	ECdata[i].scan_coef=gy.scan_coef	
	ECdata[i].crank_coef=gy.scan_coef
	ECdata[i].pol_id=gy.antenna_data.pol_id
	ECdata[i].azi_id=gy.antenna_data.azi_id
	ECdata[i].antenna_inclinationd=gy.antenna_data.antenna_inclinationd
	ECdata[i].offset_angle=gy.antenna_data.offset_angle
	ECdata[i].r_pivot=gy.antenna_data.r_pivot
	ECdata[i].z_pivot=gy.antenna_data.z_pivot
	ECdata[i].phi_pivot=float(strmid(gy.portname,0,5))
	ECdata[i].freq=gy.gyro.freq
	
	j=where(gyroname_array eq ECdata[i].gyroname, ng)
	if ng eq 1 then begin
		ECdata[i].tor_cnts=tor_count_array[j]
		cnt_limits=get_poloidal_limits(sd.shot, pol_count_array[j], ECdata[i].tor_cnts, ECdata[i].gyroname)
		if n_elements(cnt_limits) eq 1 then begin
			print, '  !!! poloidal count limit calculation failed for gyrotron '+ECData[i].gyroname
			ECdata[i].status=0
			ECdata[i].pol_cnts_upper_lim=-1
			ECdata[i].pol_cnts_lower_lim=-1
		endif else begin
			ECdata[i].pol_cnts_upper_lim=cnt_limits[1]
			ECdata[i].pol_cnts_lower_lim=cnt_limits[0]
			ECdata[i].status=1
		endelse
	endif

ENDFOR

return, ECdata

END


function get_poloidal_limits, shot, pol_cnt, tor_cnt, gyro
; calls test_counts_valid to find limits of poloidal counts for fixed toroidal counts
; pol_cnt is the initial value of poloidal counts

; first test that the initial location is in bounds
s=test_counts_valid(shot, pol_cnt, tor_cnt, gyro, -1, 1, /noplot)
if not s then begin
	print, '  !!! Initial position (' + strtrim(pol_cnt,2)+', ' + strtrim(tor_cnt,2) + $
		') is out of bounds for gyro '+gyro
	return, 0
endif

; get lower limit
dcount=100
pc=pol_cnt
for i=0, 100 do begin
	pc-=dcount
	s=test_counts_valid(shot, pc, tor_cnt, gyro, -1, 1, /noplot)
	if not s then begin
		dcount=10	; half the dead band for mirror movement
		for i=0, 12 do begin
			pc+=dcount
			s=test_counts_valid(shot, pc, tor_cnt, gyro, -1, 1, /noplot)
			if s then begin
				dcount=1
				for i=0,12 do begin
					pc-=dcount
					s=test_counts_valid(shot, pc, tor_cnt, gyro, -1, 1, /noplot)
					if not s then begin			
						pol_lower_lim=pc
						goto, get_upper_lim
					endif
				endfor
			endif
		endfor
	endif
endfor
print, '  !!! Get_poloidal_limits failed to find lower limit for gyro '+gyro
return, 0

get_upper_lim:

dcount=100
pc=pol_cnt
for i=0, 100 do begin
	pc+=dcount
	s=test_counts_valid(shot, pc, tor_cnt, gyro, -1, 1, /noplot)
	if not s then begin
		dcount=10	; 1/4 the dead band for mirror movement
		for i=0, 12 do begin
			pc-=dcount
			s=test_counts_valid(shot, pc, tor_cnt, gyro, -1, 1, /noplot)
			if s then begin
				dcount=1
				for i=0,12 do begin
					pc+=dcount
					s=test_counts_valid(shot, pc, tor_cnt, gyro, -1, 1, /noplot)
					if not s then return, [pol_lower_lim+2, pc-2]	; slightly conservative
				endfor
			endif
		endfor
	endif
endfor
print, '  !!! Get_poloidal_limits failed to find upper limit for gyro '+gyro
return, 0

end
