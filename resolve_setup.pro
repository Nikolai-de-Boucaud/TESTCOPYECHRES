function resolve_setup, setup_data, shotnumber=shotnumber
; setup_data is the structure returned from the setup file by read_ECH_setupfile
; returns a structure with the ECH data accurate for shot setup_data.shot
; Note that tank number TN, port number PN, gyrotron number GN 
; use counting starting at 1, as used in the field.

; isolate the argument
d=setup_data

; enter current data
shot=d.shot
if keyword_set(shotnumber) then shot=long(shotnumber)

nsys=d.num_sys	; number of systems
GN=intarr(nsys) & TN=GN & AN=GN & PN=GN 
PS=strarr(nsys)
for ii=0, nsys-1 do begin
	; find the gyrotron number GN, tank number TN, launcher number AN,
	; and port number PN
	gyroname=strupcase(d.gyroname[ii])
	if (strpos(gyroname, '"') gt -1) then gyroname=strmid(gyroname, 1, strlen(gyroname)-2)
	if (strpos(gyroname, 'ROW') gt -1) then gyroname='SCARECROW'	
	z=echcal(shot, gyroname, id_type='name')
	if z.status eq 0 then begin
		print, 'No such gyrotron as '+gyroname
		break
	endif
	GN[ii]=z.GyroNum
	TN[ii]=z.TankNum
	PN[ii]=z.PortNum
	AN[ii]=z.AntNum
	PS[ii]=z.PowerSupply

	if (GN[ii] lt 0) then begin
		dummyres=dialog_message(['No such gyrotron: '+ gyroname+' for this shot.', $
			'Try hitting "Present Gyrotron Setup" button first.'])
		return, {success:0}
	endif
	if (TN[ii] lt 0) then begin
		dummyres=dialog_message(['No such tank: sys '+ strtrim(ii+1,2) +' for this shot.', $
			'Try hitting "Present Gyrotron Setup" button first.'])
		return, {success:0}
	endif
	if PN[ii] lt 0 then begin
		dummyres=dialog_message(['Gyrotron ' + gyroname + $
			' not connected to a port.', $
			'Try setting historical/present waveguide configuration', $
			'or delete from setup file.'])
		return, {success:0}
	endif
	if (AN[ii] lt 0) then begin
		dummyres=dialog_message(['No such antenna or port: sys '+ ii+' for this shot.', $
		'Try hitting "Present Gyrotron Setup" button first.'])
		return, {success:0}
	endif	
	
endfor 

; Put tank number and port number into the vernacular (normal counting)
;PN=PN+1
;TN=TN+1
;GN=GN+1

; deal with case where the setup file is set up for an old case
ind=where(AN lt 6, count)
if count gt 0 then begin
	message, 'This calls for a GA antenna; use echres_old code.'
	return, {success:0}
endif

return, {success:1, shot:shot, NumberSystems:nsys, Gyrotron:d.GyroName, $
	GyroNumber:GN, TankNumber:TN, AntNumber:AN, PortNumber:PN, $
	polarizer1:setup_data.pol1, polarizer2:setup_data.pol2, $
	pol_counts:setup_data.tiltct, tor_counts:setup_data.facetct, $
	PowerSupply:PS}
	
end
