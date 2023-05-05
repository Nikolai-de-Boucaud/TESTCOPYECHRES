; ECHtomds_setupfile.pro
; Writes data about ECH settings to MDS+
; Data are taken from a saved setup file from echres
; User is asked for first and last shot, for this setup
; Use data in ECHSysData and other routines to get other data

@$ECHRES_PATH/get_miters
@$ECHRES_PATH/antenna_angles
@$ECHRES_PATH/ang
@$ECHRES_PATH/get_range

if getenv('mdsip') eq '' then mdsconnect, 'ATLAS'
echtree='\RF::TOP.ECH'
set_database, 'd3drdb'
setup_file=''
newfile=''

; tplt is template made by ascii_template
tplt={version:1.0, datastart:long(0), delimiter:byte(9), $
	missingvalue:10000000.0, $
	commentsymbol:';', fieldcount:long(5), $
	fieldtypes:fix([7,4,4,3,3]), $
	fieldnames:['field1', 'field2', 'field3', 'field4', 'field5'], $
	fieldlocations:long([0,9,14,20,25]), $
	fieldgroups:[0,1,2,3,4]}
setupdir='$ECHRES_PATH'

start:
print, ''
print, '*********************************'
read, 'Get NEW setup file? (y/n), or q to quit: ', newfile
if newfile eq 'q' then goto, done

; get setup data file
if ((newfile eq 'y') or (newfile eq 'Y') or (newfile eq 'yes') or (newfile eq 'Yes') or (newfile eq 'YES')) $
	then setup_file=dialog_pickfile(title='Pick ECH setup file: ', $
	filter=setupdir + '/*.setup')
if setup_file eq '' then goto, start
setupdir=strmid(setup_file, 0, rstrpos(setup_file, '/'))

; reads data in setup_file and returns a structure with the data
d=read_ascii(setup_file, template=tplt)
shot=long(d.field1[0])
num_sys=fix(d.field1[1])
gyronames=string(d.field1[2:1+num_sys])
polarizer1=float(d.field2[2:1+num_sys])
polarizer2=float(d.field3[2:1+num_sys])
tilt_counts=long(d.field4[2:1+num_sys])
facet_counts=long(d.field5[2:1+num_sys])

; works for PPPL antennas; sometime, fix shim input for GA antennas 
shimstr=''
shim1=intarr(num_sys)
shim2=intarr(num_sys)

print, ''
for i=0, num_sys-1 do $
	print, gyronames[i], polarizer1[i], polarizer2[i], $
	tilt_counts[i], facet_counts[i]
print, ''

ok=''
FirstShot=long(0)
LastShot=long(0)
read, 'Enter first and last shot for this setup: ', FirstShot, LastShot
if (LastShot-FirstShot) gt 30 then begin
	print, '  !!! More than 30 shots.'
	read, '     Proceed anyway with mds write? (y/n) ', ok
	if ok ne 'y' then goto, start
endif

;***********
; Check right day
query='select time_of_shot from summaries where shot = ' + strtrim(string(FirstShot),2)
n=dsql(query, timeofshot)
DayOfShot=strmid(timeofshot, 0, 11)
day=systime()
today=strmid(day, 4, 6)
today=today + strmid(day, 19,5)
if DayOfShot ne today then begin
	print, '!!! Shot number ', FirstShot, ' not from today.'
	read, '     Proceed with mds write? (y/n) ', ok
	if ok ne 'y' then goto, start
endif
;************

ShotList=long(get_range(FirstShot, LastShot, LastShot-FirstShot+1))

NoTree=long(0)
ValidTree=long(0)
Xmissionlineprob=long(0)

print, ''
print, 'Shots to input: '
for i=0, n_elements(ShotList)-1 do print, i+1, ShotList[i]
yes=''

read, 'Write data to MDS? (y/n): ', yes
if ((yes ne 'y') and (yes ne 'Y') and (yes ne 'yes') and (yes ne 'Yes') and (yes ne 'YES')) $
	then goto, start

writeit:
for jj=0, n_elements(ShotList)-1 do begin	; for each shot do the following:
	shot=ShotList[jj]
	mdsopen, 'RF', shot, status=success
	if success then begin
		print, 'Archive opened for shot ', shot 
		ValidTree=[ValidTree, shot]
	endif else begin
		print, 'Shot archive unavailable for shot ', shot
		NoTree=[NoTree, shot]
		goto, nextshot
	endelse
	@$ECHRES_PATH/ECHSysData
	mdsput, echtree+':NUM_ANTENNAS', '$', NumberOfPorts
	mdsput, echtree+':NUM_SYSTEMS', '$', Num_Sys
	
	for kk=0, num_sys-1 do begin
		node=echtree+'.SYSTEM_'+string(kk+1, format='(I1)')
		nodea=node+'.ANTENNA:'
		nodet=node+'.TRANSMISSION:'
		GN=where(gyro.gyroname eq gyronames[kk], count)
		if count ne 1 then GN=where(gyro.gyroname eq strmid(gyronames[kk],1,strlen(gyronames[kk])-2), count)
		if count ne 1 then begin
			print, ' !!! Error in gyrotron number for shot ', shot
			print, ' Gyrotron '+gyronames[kk]+' not found; skipping shot.'
			print, ''
			goto, nextshot
		endif
		GN=GN[0]
		TN=where(Gyro_in_Tank eq GN)	; tank number
		TN=TN[0]
		if TN lt 0 then print, 'Error in tank number for shot ', shot
		gyroname=gyro[GN].gyroname
		AN=Ant_in_Port[Port_by_Tank[TN]]	; antenna number
		PN=Port_by_Tank[TN]			; port number
		ant=Antenna_data[AN]
		
		shimthk=shmtk[shim1[kk]] + shmtk[shim2[kk]]
		Antenna_angles, shot, AN, tiltang, facetang, tilt_counts[kk], $
			facet_counts[kk], shimthk, shimstr, /antenna_num
		
		antstyle=''
		if ((AN eq 6) or (AN eq 7)) then antstyle='P2001'
		if ((AN eq 8) or (AN eq 9)) then antstyle='P2002'
		if ((AN eq 10) or (AN eq 11)) then antstyle='P2006'
		
		get_reflected_angles, tiltang, facetang, Antenna_data[AN].pol_id*!dtor, $
			Antenna_data[AN].azi_id*!dtor, Antenna_data[AN].offset_angle*!dtor, $
			polar_angle, azimuthal_angle, $
			Antenna_data[AN].antenna_inclinationd, antenna_style=antstyle
		
		; calculate ray starting point as center of steering mirror
		dr_launch=-(ant.d_ant/cos(tiltang*!dtor) + $
		ant.s_ant/sin(ant.pol_id*!dtor))/(1./tan(ant.pol_id*!dtor)+tan(tiltang*!dtor))
		r_mir=ant.r_pivot + dr_launch
		dz_launch=tan(tiltang*!dtor)*dr_launch + ant.d_ant/cos(tiltang*!dtor)
		z_mir=ant.z_pivot + dz_launch

		mdsput, node+':ANTENNA_NUM', '$', PN+1	; this is really the port number
		mdsput, node+':TANK_NUM', '$', TN+1
		mdsput, node+'.GYROTRON:NAME', '$', gyroname
		mdsput, nodea+'AZI_ID', '$', ant.azi_id
		mdsput, nodea+'CRADLE_CNTS', '$', tilt_counts[kk]
		mdsput, nodea+'CRANK_AZI', '$', 180.
		mdsput, nodea+'CRANK_TILT', '$', ant.antenna_inclinationd
		mdsput, nodea+'DISPERSION', '$', 1.7
		mdsput, nodea+'DRAWING_NUM', '$', ant.DwgNo
		mdsput, nodea+'D_ANTENNA', '$', ant.d_ant
		mdsput, nodea+'FACET_ANGLE', '$', facetang
		mdsput, nodea+'FACET_CNTS', '$', facet_counts[kk]
		mdsput, nodea+'LAUNCH_R', '$', r_mir
		mdsput, nodea+'LAUNCH_Z', '$', z_mir
		mdsput, nodea+'OFFSET_ANGLE', '$', ant.offset_angle
		mdsput, nodea+'POLARIZER1', '$', polarizer1[kk]
		mdsput, nodea+'POLARIZER2', '$', polarizer2[kk]
		mdsput, nodea+'POL_ID', '$', ant.pol_id
		mdsput, nodea+'PORT', '$', ant.port
		mdsput, nodea+'RAY_AZI', '$', azimuthal_angle/!dtor
		mdsput, nodea+'RAY_POLAR', '$', polar_angle/!dtor
		mdsput, nodea+'R_PIVOT', '$', ant.r_pivot
		if shim1[kk] gt 0 then $
			mdsput, nodea+'SHIM1', '$', shim1[kk]
		if shim2[kk] gt 0 then $
			mdsput, nodea+'SHIM2', '$', shim1[kk]
		mdsput, nodea+'S_ANTENNA', '$', ant.s_ant
		mdsput, nodea+'TILT_ANGLE', '$', tiltang
		mdsput, nodea+'Z_PIVOT', '$', ant.z_pivot
		
		; here goes the data for the calibrated point names
		if GN eq 3 then gyrolong='SCARECROW' else gyrolong=strupcase(gyroname)
		mdsput, '\RF::TOP.ECH.' + gyrolong + '.SYSTEM', '$', fix(kk+1)
		calibptr='\RF::TOP.ECH.SYSTEM_'+strtrim(kk+1,2) + '.POWER:CALIBCOPW*1000.'
		calibnode='\RF::TOP.ECH.'+ gyrolong + ':CALIBCOPW'
		mdstcl,'put '+calibnode+' "'+calibptr+'" '

		get_miters, TN, PN, shot, mirtypen, gamman, delayn, success
		if not success then begin
			print,'XLine data not available for this combination tank and antenna'
			print,'Shot=',shot, '; TN=', TN, '; AN=', AN, '; PN=', PN
			Xmissionlineprob=[xmissionlineprob, shot]
			goto, nextsystem
		endif

		mdsput, nodet+'ANISOTROPY',	'$', delayn
		mdsput, nodet+'ELLIPTICITY',	'$', Gyro[GN].PolEll	
		mdsput, nodet+'GAMMA',			'$', gamman
		mdsput, nodet+'GRVMIRROR1SN',	'$', TankMir[kk].gm1SN
		mdsput, nodet+'GRVMIRROR2SN',	'$', TankMir[kk].gm2SN
		mdsput, nodet+'GRVMIRROR_C1',	'$', $
			TankMir[TN].gmc1 + TankMir[TN].gmc1corr
		mdsput, nodet+'GRVMIRROR_C2',	'$', $
			TankMir[TN].gmc2 + TankMir[TN].gmc2corr
		mdsput, nodet+'INCLINATION',	'$', Gyro[GN].PolInc
		mdsput, nodet+'MIRROR_TYPE',	'$', mirtypen
	nextsystem:
	endfor
	validtree=[validtree, shot]
nextshot:
endfor
goto, start
done:
end
