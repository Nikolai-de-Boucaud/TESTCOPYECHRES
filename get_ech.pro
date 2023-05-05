; GET_ECH		R. Prater	    	22 March 2010
;
; NAME:
;       GET_ECH
;
; PURPOSE:
;       Returns ECH data from MDSplus and ptdata calls
;
; CATEGORY:
;       DIII-D data handling
;
; CALLING SEQUENCE:
;       result=get_ech(shot, [[times,] inone_lines], $
;			table=table, nocache=nocache, nopower=nopower, $
;			listcache=listcache, ncache=ncache, dt=dt, pmin=pmin, ndata=ndata)
;
; ARGUMENTS:
;       SHOT    DIII-D shot number
;		TIMES	Optional array of times in msec at which data values are returned
;					If absent, data returned for time=0
;		INONE_LINES Optional argument used if TIMES argument is present
;			INONE_LINES returns an array of strings for use in the inone file
;
; KEYWORD PARAMETERS:
;       TABLE   If set, a table of injected powers for each gyrotron is printed,
;			one table per element of times. If no times, then no table.
;       NOCACHE If set, cache is not checked for data and all
;			calculations are redone. At completion, data is
;			written into the cache
;		NOPOWER	If set, all data except power is returned; for quicker response
;			when powers not needed.
;       LISTCACHE	If set, just list the shots in the cache and return.
;		NCACHE	Number of shots/slices to keep in cache; default is 8, max is 64
;		DT		Averaging time for ECH power traces (+/- msec); default is 1.0
;		PMIN	Power level (W) deemed noise for each gyrotron; default is 35,000
;		NDATA	Number of samples returned in power traces; default is 131072
;
; NOTES:
;       This procedure assumes all the power data are on the same clock.
;
;		Facility is present to address changes in launcher angles as a
;		function of time during a shot, when that capability becomes available.
;
;		Caching and table printing based on Chuck Greenfield's cmg_get_ech().
;
;	!!! This code differs from previous versions in that some output nodes have 
;		changed location or been deleted. These changes were made desirable by the
;		advent of mirrors which may be moved in the poloidal direction during a shot.
;		The elements systems.ray_azi, systems.ray_polar, systems.antenna.ray_azi, 
;		systems.antenna.ray_polar were deleted, and in their place there are now
;		systems.ray_azis, systems.ray_polars, systems.facet_angles, systems.tilt_angles,
;		systems.applied_inclinations, systems.applied_ellipticities. These latter
;		quantities are all functions of the second argument, the times array.
;
;		For backward compatibility, the nodes systems.power.injection, 
;		systems.antenna.ray_polar, systems.antenna.ray_azi, systems.antenna.launch_r,
;		systems.antenna.launch_z remain. If the optional argument 'times' is not
;		entered, all these values are zeroes.
;
;
;
; OTHER REQUIRED ROUTINES:
;		MDS routines in /link/idl/
;		$ECHRES_PATH/antenna_angles
;		$ECHRES_PATH/ECHSysData.pro file containing calibration data, by shot range 
;
; Returns a structure with ECH data. E.g., 
;IDL> t=get_ech(142650)             
;IDL> help, /st, t
;   SHOT            LONG            142650
;   TIMES           FLOAT           0.00000 ; times at which data are interpolated
;   NUM_ANTENNAS    INT              6
;   NUM_SYSTEMS     INT              6
;   DT              FLOAT           1.00000 ; ECH averaging time, msec
;   SYSTEMS         STRUCT    -> <Anonymous> Array[6]
;   TIME            FLOAT     Array[131072] ; time clock for fast power data (msec)
;   ECHPWR          FLOAT     Array[131072] ; total power(time) (W)
;   PINJS           FLOAT           0.00000 ; total power at times 'times'
;   SLOWTIME        FLOAT     Array[8001]	; time clock for mirror motions
;   STAT            BYTE         1
;IDL> help, /st, t.systems
;   GYROTRON        STRING    'Scarecrow'	; gyrotron name
;   FREQ            FLOAT       1.10000e+11 ; frequency (Hz)
;   TANK_NUMBER     INT              5
;   ANTENNA_NUMBER  INT              3
;   PORT            STRING    '256.4 R+1'
;   DRAWING_NUM     STRING    'P2006_M1'	; antenna ID
;   POLARIZER1      FLOAT          -10.0000 ; polarizer settings
;   POLARIZER2      FLOAT           65.0000
;   PINJ            FLOAT     Array[131072] ; injected power(time) for this gyrotron (W)
;   POLOIDAL_CNT    LONG      Array[8001]	; poloidal counts(slowtime) for mirror angle
;   TOROIDAL_CNT    LONG      Array[8001]	; toroidal counts(slowtime) 
;   DISPERSION      FLOAT           1.70000 ; FWHM of EC beam dispersion (deg)
;   LAUNCH_R        FLOAT           2.39990 ; Major radius of launch location (m)
;   LAUNCH_Z        FLOAT          0.679400 ; elevation from midplane of launch location (m)
;   ANTENNA         STRUCT    -> <Anonymous> Array[1]
;   POWER           STRUCT    -> <Anonymous> Array[1]
;   TRANSMISSION    STRUCT    -> <Anonymous> Array[1]
;   STAT            INT              1
;   ERRLIST         STRING    Array[30]
;   PINJS           FLOAT           0.00000 ; injected power(times) (W)
;   POLOIDAL_CNTS   LONG              9918	; poloidal counts(times)
;   TOROIDAL_CNTS   LONG             10954	; toroidal counts(times)
;   RAY_AZIS        FLOAT           184.827 ; ray azimuthal angle(times) (deg)
;   RAY_POLARS      FLOAT           52.2039 ; ray polar angle(times) (deg)
;   FACET_ANGLES    FLOAT           3.81296 ; mirror facet angle(times) (deg)
;   TILT_ANGLES     FLOAT           52.1052 ; mirror tilt angle(times) (deg)
;   APPLIED_INCLINATIONS
;                   FLOAT           4.98457 ; applied inclination of polarization(times)
;   APPLIED_ELLIPTICITIES
;                   FLOAT           31.3733 ; applied ellipticity of polarization(times)
;IDL> help, /st, t.systems.antenna			; static data about antenna
;   DISPERSION      FLOAT           1.70000 ; FWHM angular dispersion
;   LAUNCH_R        FLOAT           2.39990 ; R and Z of launch location
;   LAUNCH_Z        FLOAT          0.679400
;   Z_PIVOT         FLOAT          0.679400
;   R_PIVOT         FLOAT           2.39990
;   D_ANTENNA       FLOAT           0.00000 ; Antenna geometry
;   S_ANTENNA       FLOAT           0.00000
;   POL_ID          FLOAT           12.1000 ; Calibration angles, of ray incident on 
;   AZI_ID          FLOAT           179.400 ;   steering mirror
;   OFFSET_ANGLE    FLOAT          -1.40000 ; Toroidal offset of antenna in port
;   ANTENNA_INCLINATIOND
;                   FLOAT           22.3000
;	RAY_AZI 		FLOAT					; These quantities are redundant, but here for
;	RAY_POLAR		FLOAT					; backward compatibility. They contain the values
;	FACET_ANGLE 	FLOAT					; at times[0], which by default is t=0.0. So they
;	TILT_ANGLE		FLOAT					; have same values as first element of systems
;											; level quantities.
;   ERRLIST         STRING    Array[30]
;   STAT            INT              1
;IDL> help, /st, t.systems.transmission 	; static data about transmission line
;   GAMMA           FLOAT     Array[20]
;   MIRROR_TYPE     INT       Array[20]
;   ANISOTROPY      FLOAT     Array[20]
;   ELLIPTICITY     FLOAT           0.00000
;   INCLINATION     FLOAT           90.0000
;   GRVMIRROR_C1    FLOAT     Array[6]
;   GRVMIRROR_C2    FLOAT     Array[6]
;	APPLIED_INCLINATION FLOAT				; Also for backward compatibility
;	APPLIED_ELLIPTICITY FLOAT
;IDL> help, /st, t.systems.power
;   INJECTION       FLOAT           0.00000
;   CALIBCOPW       FLOAT           0.00000 ; calibration constant power/bit
;   CAL_SOURCE      STRING    'ECSCAFPWRC'	; power pointname
;
; If the second argument, times, is not present, times=[0.0] is used. If the 
; times argument is present and an array of size n, then the systems[i] nodes
; PINJS, POLOIDAL_CNTS, TOROIDAL_CNTS, RAY_AZIS, RAY_POLARS, FACET_ANGLES, 
; TILT_ANGLES, APPLIED_INCLINATIONS, APPLIED_ELLIPTICITIES
; are arrays of size n, corresponding to the times in times. If the times argument
; is absent, times=[0.0].
; 28 April 2010
;
;
; Some effort has been made to return more-or-less valid estimates of gyrotron
; power for shots before 109868, when the calorimetric calibrations shot-by-shot
; were begun. In cases where no reasonable calibration is available, an estimate of
; the power for each gyrotron is used.
; 22 March 2010 
;
;-----------------------------------------------------------------------------

function mdsget, node
data=mdsvalue(node, /quiet, stat=stat)
if not stat then begin
	msg=''
	msg=mdsgetmsg(stat,/quiet)
	;print, '  Error obtaining '+node
	return, {stat:stat, msg:msg} 
endif else return, {stat:stat, data:data, msg:''}
end

; nearest_index.pro
; like where(a eq b) but finds nearest value if a ne b anywhere
; If a eq b for more than one index, return the first case
; a is input array
; b is target values, can be array
function nearest_index, a, b, diff=diff, count=count
nb=n_elements(b)
indx=lonarr(nb)
diff=fltarr(nb)
i=0L
count=0L
for k=0, nb-1 do begin
	i=where(a eq b[k], count)
	if count gt 0 then begin
		indx[k]=i[0]
		diff[k]=0.
	endif else begin
		val=abs(a-b[k])
		i=where(min(val) eq val, count)
		indx[k]=i[0]
		diff[k]=a[indx[k]]-b[k]
	endelse
endfor
if nb eq 1 then begin
	diff=diff[0]
	return, indx[0] 
endif else return, indx
end

; arrsca: converts arrays of size 1 to scalars for
;	better info when 'help' is called
pro arrsca, strin, strout
sz=size(strin, /type)
if sz eq 8 then begin
	nms=tag_names(strin)
	nel=n_elements(nms)
	if nel ge 1 then begin
		a=strin.(0)
		if n_elements(a) eq 1 then b=a[0] else b=a
		strout=create_struct(nms[0], b)
		if nel ge 2 then begin
			for i=1, nel-1 do begin
				a=strin.(i)
				if n_elements(a) eq 1 then a=a[0]
				strout=create_struct(strout, nms[i], a)
			endfor
		endif
	endif
endif else if n_elements(strin) eq 1 then strout=strin[0] else strout=strin
end

function interpp, xx, yy, xtarget
x=float(xx)
y=float(yy)
nx=n_elements(x)
ny=n_elements(y)
nxt=n_elements(xtarget)
; check size of ordinate equals abscissa
if ((nx le 1) or (ny le 1)) then return, y
if ny ne nx then return, -1
; check for monotonicity of x
if x[nx-1] lt x[0] then return, -1
s=x-shift(x,1)
i=where(s[1:*] le 0., count)
if count gt 0 then return, -1	; non monotonic
; let x<x[0] be y[0] and x>x[nx-1] be y[ny-1]
ytarget=fltarr(nxt)
j=where(xtarget lt x[0], counti)
if counti gt 0 then ytarget[j]=y[0]
k=where(xtarget gt x[nx-1], countj)
if countj gt 0 then ytarget[k]=y[nx-1]
; interpolate the rest
n=indgen(nxt-counti-countj)+counti
ytarget[n]=interpol(y,x,xtarget[n])
return, ytarget
end
	
pro print_ech_table,ech
fmt='(a,t20,i6)'
tags=tag_names(ech)
i=where(tags eq 'TIMES', count)
if count gt 0 then begin
	ntimes=n_elements(ech.times)
	print, ''
	for j=0, ntimes-1 do begin
		print, 'Shot ' + strtrim(ech.shot,2) + ', ' + 'time ' + strtrim(ech.times[j],2) + ' msec'
		print,format=fmt,'Num systems:' + strtrim(ech.num_systems,2)
		fmt='(a' 
		for i=0,ech.num_systems-1 do fmt=fmt+',t'+strtrim(string(15*(i+1)),2)+',a'
		fmt=fmt+')'
		print,format=fmt,'Gyrotron: ',ech.systems.gyrotron
		print,format=fmt,'Inj power:',strtrim(string(ech.systems.pinjs[j]),2)
		print, ''
	endfor
endif else print, 'Table of powers is printed only if times argument is specified.'
return
end

;*********************************************************************************

function get_ech, shot, times, inone_lines, dt=dt, pmin=pmin, table=table, alldata=alldata, $
	nocache=nocache, listcache=listcache, ncache=ncache
	; times: optional array of particular times in msec at which parameters should be returned.
	;	Such parameters are identified by appended "s" to names in the returned structure.
	;   If the second argument is absent, data is returned for times=0
	; dt: time in +/-msec over which power data is averaged at times; default is +/- 1.0 msec.
	; pmin: ECH power level (W) deemed noise; default is 35 kW
	; table: set to print tables for times times
	; ncache: number of ech structures to keep in cache; default is 8
	; optional argument inone_lines can return a string array for entry into inone

catch, error_stat
if error_stat ne 0 then begin
	catch, /cancel
	return, {stat:0B, error_message:!ERROR_STATE.MSG}
endif

; handle listcache
common get_ech_cache, cache
if keyword_set(listcache) then begin
    n=n_elements(cache)
    if (n eq 0) then message,'cache is empty',/continue $
    else begin
        message,'the following shots are in the cache:',/information
        cache_shots=(*cache[0]).shot
        for i=1,n-1 do cache_shots=[cache_shots,(*cache[i]).shot]
        print,cache_shots[sort(cache_shots)]
    endelse
    return,{stat:1b}
endif

nargs=n_params()

; check for too many or too few arguments
if nargs lt 1 or nargs gt 3 then message, 'Number args must be between 1 and 3'

; first argument must be a shot number
s=size(shot,/type)
if s ne 2 and s ne 3 then message, 'First argument is not a shot number'
if s eq 2 then shot=long(shot)

; if no times argument then return data at t=0
if nargs eq 1 then times=[0.0]

; check second argument type
if nargs eq 2 then begin
	s=size(times, /type)
	; if 2nd arg is not a number then return an error
	if s ne 2 and s ne 3 and s ne 4 and s ne 5 then $
		message, 'Second argument is not a number (array of times)'
endif

times=float(times[uniq(times, sort(times))])
ntimes=n_elements(times)

; Enter keyword default values

; check averaging period of ECH power
if n_elements(dt) eq 0 then dt=0.0
if dt gt 100. then dt=100.
if dt le 0. then dt=0.01

; pmin is minimum power level (noise), below which power is zero
if n_elements(pmin) eq 0 then pmin=3.5e4

if not keyword_set(alldata) then alldata=0

; ECH tree
tree='\TOP.ECH'

table=keyword_set(table)

if n_elements(ncache) eq 0 then ncache=8
ncache = max([4, ncache]) ; keep cache size between 4 and 64
ncache=min([ncache, 64])

; use data from cache if slice already there
use_cache=0 
nocache=keyword_set(nocache)
if not nocache then begin     
    n=n_elements(cache)
	if n gt ncache then begin
		cache=cache[n-1-ncache:*]
		n=ncache
	endif
    if (n gt 0) then begin	; there are slices in the cache
        cache_shots=(*cache[0]).shot
        for i=1,n-1 do cache_shots=[cache_shots,(*cache[i]).shot]
        j=where(cache_shots eq shot, mcount)
        if (mcount gt 0) then begin	; found this shot at least once in the cache
			for k=0, mcount-1 do begin
				if max((*cache[j[k]]).echpwr) gt 1.e4 then begin
					use_cache=1
					icache=j[k]
				endif
			endfor
		endif
	endif
endif

; ***********************************************
; Connect to server and get some data
; ***********************************************

if getenv('mdsip') eq '' then mdsconnect,'atlas.gat.com'
if shot le 0 then shot=mdsget('current_shot("d3d")')+shot
mdsopen,'RF',shot, status=status
if not status then message, 'Could not open RF tree for shot '+strtrim(shot,2), /continue

num_ant=mdsget(tree+':NUM_ANTENNAS')
if not num_ant.stat then return,{stat:num_ant.stat,shot:shot, $
	error_message:num_ant.msg}
num_antennas=fix(num_ant.data)

num_sys=mdsget(tree+':NUM_SYSTEMS')
if num_sys.stat then num_systems=num_sys.data else num_systems=0

; get times
ectime=mdsget('\TOP.ECH:ECTIME')
if not ectime.stat then return, {shot:shot, stat:0B, error_message:'time data ECTIME not written to MDSplus node'}

; get calibrated power
echpwrc=mdsget('\ECHPWRC')
if not echpwrc.stat then return, {shot:shot, stat:0B, error_message:'No ECHPWRC found'}

if num_systems le 0 then return, { $
	shot:shot, $
	num_antennas:0, $
	num_systems:0, $
	time:ectime.data, $
	echpwrc:echpwrc.data, $
	error_message:'', $
	stat:1B}

ectimes=mdsget('\TOP.ECH:ECTIMES')
if not ectimes.stat then return, {shot:shot, stat:0B, error_message:'time data ECTIMES not written to MDSplus node'}

ectime=ectime.data
ectimes=ectimes.data
nectimes=n_elements(ectimes)
nectime=n_elements(ectime)
indtime=nearest_index(ectime, times)
dindtime=round(dt/(ectime[1]-ectime[0]))	; number of samples of powers to average
ndindtime=float(2*dindtime+1)
sindtime=nearest_index(ectimes, times)

systems={ $
	gyrotron:'', $					; gyrotron
	freq:0.0, $		 				; frequency in Hz
	tank_number:0, $				; Tank number in ECH vault
	antenna_number:0, $				; 1=270-offset_angle,...,6=240+offset_angle
	port:'', $						; Port description for antenna
	drawing_num:'', $				; type of antenna
	polarizer1:0.0, polarizer2:0.0, $	; polarizer settings
	dispersion:0.0, $				; angular gaussian dispersion of ray bundle HWHM, deg
	launch_r:0.0, launch_z:0.0, $	; (R,Z) of launch of ray bundle, 
	rfon:0.0, rftime:0.0, $ 		; time of first on and last off, in sec
	pinj:fltarr(nectime), $			; injected power vs time (fast time clock)
	poloidal_cnt:lonarr(nectimes), $	; poloidal and toroidal antenna counts,
	toroidal_cnt:lonarr(nectimes), $	; 	vs slow time clock
	ray_azi:fltarr(nectimes), $
	ray_polar:fltarr(nectimes), $
	xmfrac:fltarr(nectimes), $
	pinjs:fltarr(ntimes), $
	poloidal_cnts:lonarr(ntimes), $
	toroidal_cnts:lonarr(ntimes), $
	ray_azis:fltarr(ntimes), $
	ray_polars:fltarr(ntimes), $
	xmfracs:fltarr(ntimes), $
	stat:0,errlist:strarr(30)}

if alldata then systems=create_struct(systems, $
	'applied_inclination', fltarr(nectimes), $
	'applied_inclinations', fltarr(ntimes), $
	'applied_ellipticity', fltarr(nectimes),$
	'applied_ellipticities', fltarr(ntimes),$
	'facet_angle', fltarr(nectimes),$
	'facet_angles', fltarr(ntimes),$
	'tilt_angle', fltarr(nectimes),$
	'tilt_angles', fltarr(ntimes))

if ntimes eq 1 then arrsca, temporary(systems), systems

if alldata then begin
	; antenna substructure contains antenna data that are stationary in time,
	; like the dispersion angle. Evaluated for times[0] or t=0.0 if times not input.
	antenna={ $
		ray_azi:0.0, $					; ray bundle azimuthal and polar angles for times[0],
		ray_polar:0.0, $				;	for backward compatibility
		facet_angle:0.0, $				; mirror facet (toroidal) and tilt (poloidal) angles 
		tilt_angle:0.0, $				; 	for times[0], for backward compatibility 
		dispersion:0.0, $				; angular gaussian dispersion of ray bundle HWHM, deg
		launch_r:0.0, launch_z:0.0, $	; (R,Z) of launch of ray bundle, m
		r_pivot:0.0, z_pivot:0.0, $
		d_antenna:0.0, s_antenna:0.0, $
		pol_id:0.0, azi_id:0.0, $
		offset_angle:0.0, $
		antenna_inclinationd:22.4, $
		crank_tilt:22.4, $
		crank_azi:180., $
		errlist:strarr(30), stat:0}

	transmission={ $
		applied_inclination:0.0,$		; inclination of wave as applied to the plasma, at time[0]
		applied_ellipticity:0.0, $		; ellipticity
		gamma:fltarr(20), $
		mirror_type:intarr(20), $
		anisotropy:fltarr(20), $
		ellipticity:0.0, $
		inclination:0.0, $
		grvmirror_C1:fltarr(6), $
		grvmirror_C2:fltarr(6)}
		
	power={ $
		calibcopw:0.0, $
		cal_source:''}
	
	systems=create_struct(systems, 'antenna', antenna, 'transmission', transmission, 'power', power)

endif
systems=replicate(systems, num_systems)	

; data by system
if num_systems gt 0 then for i=0, num_systems-1 do begin

    sys=tree+'.SYSTEM_'+strtrim(string(i+1),2)
	sysa=sys+'.ANTENNA:'
	syst=sys+'.TRANSMISSION:'
	sysp=sys+'.POWER:'

    antenna_num=mdsget(sys+'.ANTENNA_NUM')
    if not antenna_num.stat then return,{stat:antenna_num.stat,shot:shot,tree:sys,error_message:antenna_num.msg}
	systems[i].antenna_number=antenna_num.data
	
    tank_num=mdsget(sys+'.TANK_NUM')
    if not tank_num.stat then return,{stat:tank_num.stat,shot:shot,tree:sys,error_message:antenna_num.msg}
	systems[i].tank_number=tank_num.data

	; enter the data into the systems nodes
	gname=mdsget(sys+'.GYROTRON:NAME')
	if not gname.stat then goto, next_sys
	gname=strupcase(gname.data)
	if strpos(gname,'"') ge 0 then gname=strmid(gname, 1, strlen(gname)-2)
	if strpos(gname, 'SCA') ge 0 then gname='SCARECROW'
	
	; get static system data from antenna node
	systems[i].freq = mdsvalue(sys+'.GYROTRON:FREQUENCY', /quiet)
	systems[i].dispersion=mdsvalue(sysa+'DISPERSION', /quiet)
	systems[i].launch_r=mdsvalue(sysa+'LAUNCH_R', /quiet)
	systems[i].launch_z=mdsvalue(sysa+'LAUNCH_Z', /quiet)
	systems[i].port=mdsvalue(sysa+'port', /quiet)
	systems[i].drawing_num=mdsvalue(sysa+'DRAWING_NUM', /quiet)
	systems[i].polarizer1=mdsvalue(sysa+'POLARIZER1', /quiet)
	systems[i].polarizer2=mdsvalue(sysa+'POLARIZER2', /quiet)

	gprefix='EC'+strmid(gname,0,3)	
;	IF systems[i].launch_r EQ 1.559 THEN gname='TLEIA'
	IF strcmp(systems[i].Drawing_NUM,'Top',3,/FOLD_CASE) THEN gname='T'+gname
	systems[i].gyrotron=gname
	
	if alldata then begin
		systems[i].antenna.dispersion=systems[i].dispersion
		systems[i].antenna.launch_r=systems[i].launch_r
		systems[i].antenna.launch_z=systems[i].launch_z
		systems[i].antenna.r_pivot=mdsvalue(sysa+'R_PIVOT', /quiet)
		systems[i].antenna.z_pivot=mdsvalue(sysa+'Z_PIVOT', /quiet)
		systems[i].antenna.d_antenna=mdsvalue(sysa+'D_ANTENNA', /quiet)
		systems[i].antenna.s_antenna=mdsvalue(sysa+'S_ANTENNA', /quiet)
		systems[i].antenna.pol_id=mdsvalue(sysa+'POL_ID', /quiet)
		systems[i].antenna.azi_id=mdsvalue(sysa+'AZI_ID', /quiet)
		systems[i].antenna.offset_angle=mdsvalue(sysa+'OFFSET_ANGLE', /quiet)
		systems[i].antenna.antenna_inclinationd=mdsvalue(sysa+'ANT_INCL', /quiet)
	
		systems[i].transmission.gamma=mdsvalue(syst+'GAMMA', /quiet)
		systems[i].transmission.mirror_type=mdsvalue(syst+'MIRROR_TYPE', /quiet)
		systems[i].transmission.anisotropy=mdsvalue(syst+'ANISOTROPY', /quiet)
		systems[i].transmission.ellipticity=mdsvalue(syst+'ELLIPTICITY', /quiet)
		systems[i].transmission.inclination=mdsvalue(syst+'INCLINATION', /quiet)
		systems[i].transmission.grvmirror_C1=mdsvalue(syst+'GRVMIRROR_C1', /quiet)
		systems[i].transmission.grvmirror_C2=mdsvalue(syst+'GRVMIRROR_C2', /quiet)
		
		systems[i].power.calibcopw=mdsvalue(sysp+'CALIBCOPW', /quiet)
		systems[i].power.cal_source=mdsvalue(sysp+'CAL_SOURCE', /quiet)	
	endif
	
	;read power waveforms for each system
	pinj=mdsvalue('\'+gprefix+'FPWRC',/quiet, stat=stat)
	if not stat or n_elements(pinj) lt 10 then pinj=fltarr(nectime) else $
		pinj=smooth(pinj, ndindtime, /edge_truncate)
	systems[i].pinj=pinj
	
	; get ECH timing
	k=where(smooth(systems[i].pinj,5) gt pmin, count) ; first turn on, last turn off
	if count gt 0 then begin
		systems[i].rfon=ectime[k[0]]
		systems[i].rftime=ectime[k[count-1]]-ectime[k[0]]
	endif else begin
		systems[i].rfon=1.e6
		systems[i].rftime=-1.0
	endelse
		
	; now data at the 'ectimes' and 'times' times
	pinjs=fltarr(ntimes)
	for j=0, ntimes-1 do begin
		if indtime[j]-dindtime lt 0 then $
			pinjs[j]=total(pinj[0:dindtime])/float(dindtime+1) $
		else if indtime[j]+dindtime gt nectime-1 then $
			pinjs[j]=total(pinj[indtime[j]-dindtime:indtime[j]])/float(dindtime+1) $
		else pinjs[j]=total(pinj[indtime[j]-dindtime:indtime[j]+dindtime])/ndindtime
		if pinjs[j] lt pmin then pinjs[j]=0.0
	endfor
	systems[i].pinjs=pinjs
	
	polcnt=mdsget('\'+gprefix+'POLCNT')
	if polcnt.stat then begin
		systems[i].poloidal_cnt=polcnt.data
		systems[i].poloidal_cnts=interpp(ectimes, polcnt.data, times)
	endif
		
	torcnt=mdsget('\'+gprefix+'TORCNT')
	if torcnt.stat then begin
		systems[i].toroidal_cnt=torcnt.data
		systems[i].toroidal_cnts=interpp(ectimes, torcnt.data, times)
	endif

	rayazi=mdsget('\'+gprefix+'AZIANG')
	if rayazi.stat then begin
		systems[i].ray_azi=rayazi.data
		systems[i].ray_azis=interpp(ectimes, rayazi.data, times)
		if alldata then systems[i].antenna.ray_azi=systems[i].ray_azis[0]
	endif
	
	raypol=mdsget('\'+gprefix+'POLANG')
	if raypol.stat then begin
		systems[i].ray_polar=raypol.data
		systems[i].ray_polars=interpp(ectimes, raypol.data, times)
		if alldata then systems[i].antenna.ray_polar=systems[i].ray_polars[0]
	endif
	
	xmf=mdsget('\'+gprefix+'XMFRAC')
	if xmf.stat then begin
		systems[i].xmfrac=xmf.data
		systems[i].xmfracs=interpp(ectimes, xmf.data, times)
	endif

	if alldata then begin
		facang=mdsget(sysa+'FACET_ANGLE')
		if facang.stat then begin
			systems[i].facet_angle=facang.data
			systems[i].facet_angles=interpp(ectimes, facang.data, times)
			systems[i].antenna.facet_angle=systems[i].facet_angles[0]
		endif
	
		tilang=mdsget(sysa+'TILT_ANGLE')
		if tilang.stat then begin
			systems[i].tilt_angle=tilang.data
			systems[i].tilt_angles=interpp(ectimes, tilang.data, times)
			systems[i].antenna.tilt_angle=systems[i].tilt_angle[0]
		endif
	
		ainc=mdsget(sysa+'RAY_INCL')
		if ainc.stat then begin
			systems[i].applied_inclination=ainc.data
			systems[i].applied_inclinations=interpp(ectimes, ainc.data, times)
			systems[i].transmission.applied_inclination=systems[i].applied_inclinations[0]
		endif
	
		aenc=mdsget(sysa+'RAY_ELLIPT')
		if aenc.stat then begin
			systems[i].applied_ellipticity=aenc.data
			systems[i].applied_ellipticities=interpp(ectimes, aenc.data, times)
			systems[i].transmission.applied_ellipticity=systems[i].applied_ellipticities[0]
		endif
	endif
	
	systems[i].stat=1b
	
	next_sys:
	
endfor

totpinjs=fltarr(ntimes)
for kk=0, ntimes-1 do for ii=0, num_systems-1 do totpinjs[kk] += systems[ii].pinjs[kk]

gyrotrons=systems.gyrotron

ech={ $
	shot:shot, $
	num_antennas:num_antennas, $
	num_systems:num_systems, $
	gyrotrons:gyrotrons, $
	dt:dt, $
	time:ectime, $
	echpwrc:echpwrc.data, $
	times:times, $
	pinjs:totpinjs, $
	stime:ectimes, $
	systems:systems, $
	error_message:'', $
	stat:1B}
	
if ((nargs eq 3) and (ntimes gt 0)) then begin	; a string array for inone is requested
	ap="'"	
	inone_lines=strarr(ntimes)
	for i=0, ntimes-1 do begin
		inone_lines[i]  = ' ! inone lines for ECH'+string(10B)
		inone_lines[i] += ' ! Shot='+strtrim(shot,2)+';  time='+strtrim(times[i],2)+' msec'+string(10B)
		inone_lines[i] += ' ! Gyrotrons: '+strjoin(ech.systems.gyrotron, ' ')+string(10B)
		inone_lines[i] += ' GAFSEP = 1.e-6'+string(10B)
		inone_lines[i] += ' RELRF = 0.25'+string(10B)
		inone_lines[i] += ' TORAY_VERSION = 1.95'+string(10B)
		inone_lines[i] += ' TORAY_PATH = '+ap+'/fusion/projects/codes/toray/toray/xtoray'+ap+string(10B)
		inone_lines[i] += ' RFON = '+strjoin(ech.systems.rfon/1000., ' ')+string(10B)
		inone_lines[i] += ' RFTIME = '+strjoin((ech.systems.rftime)/1000.,' ')+string(10B)
		inone_lines[i] += ' IRFCUR = 6*1'+string(10B)		; hardwired to positive Ip
		inone_lines[i] += ' FREQ = '+strjoin(ech.systems.freq[i], ' ')+string(10B)
;		inone_lines[i] += ' WRFO = 6*0.0'+string(10B)		; hardwired to X-mode
		inone_lines[i] += ' WRFO = '+strjoin(1-ech.systems.xmfracs[i], ' ')+string(10B)	
		inone_lines[i] += ' HLWEC = '+strjoin(ech.systems.dispersion, ' ')+string(10B)
		inone_lines[i] += ' RATWEC = 6*1.0'+string(10B)
		inone_lines[i] += ' RFMODE = 6*'+ap+'ech'+ap+' '+string(10B)
		inone_lines[i] += ' RFPOW = '+strjoin(ech.systems.pinjs[i], ' ')+string(10B)
		inone_lines[i] += ' PHAIEC = '+strjoin(ech.systems.ray_azis[i], ' ')+string(10B)
		inone_lines[i] += ' THETEC = '+strjoin(ech.systems.ray_polars[i],' ')+string(10B)
		inone_lines[i] += ' XEC = '+strjoin(ech.systems.launch_r*100.,' ')+string(10B)
		inone_lines[i] += ' ZEC = '+strjoin(ech.systems.launch_z*100.,' ')+string(10B)
		inone_lines[i] += ' NRAY = 6*30'+string(10B)
		inone_lines[i] += ' IDAMP = 6*8'+string(10B)
		inone_lines[i] += ' ! End of ECH lines'
	endfor
endif

return,ech

end
