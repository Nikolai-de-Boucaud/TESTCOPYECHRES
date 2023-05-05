pro write_setup_file, filename, shotnum, setup_data, $
	success=success, user=user, write_timcon=write_timcon

success=0

on_ioerror, done

if not keyword_set(write_timcon) then write_timcon=0

if file_test(filename) then begin
	result=dialog_message('File exists; overwrite?', /question, /default_no)
	if (result ne 'Yes') OR (NOT file_test(filename,/user)) then return
endif

comment1=';------------- FORMAT: tab delimited -------------------------'
comment2=' First line: shot number'
comment3=' Second line: number of systems'
comment4=' Following lines: Gyrotron name, polarizer 1, polarizer 2,'
comment5='   poloidal steering counts, and toroidal steering counts'

tab=string(byte(9))
d=setup_data

j=where( ((d.gyroname ne '') and (d.PolarAng gt 10.)), num_sys)
if num_sys lt 1 then return

; open the file 
openw, fu, filename, /get_lun

; and write the data
printf, fu, strtrim(shotnum,2)
printf, fu, strtrim(num_sys,2)
for i=0, num_sys-1 do printf, fu, $
	string(d[j[i]].gyroname, format='(a12)'), tab, $
	string(d[j[i]].pol1, format='(f6.1)'), tab, $
	string(d[j[i]].pol2, format='(f6.1)'), tab, $
	d[j[i]].polcts, tab, d[j[i]].torcts, tab
printf, fu, comment1
printf, fu, comment2
printf, fu, comment3
printf, fu, comment4
printf, fu, comment5
if n_elements(user) gt 0 then printf, fu, ' User: '+user else user=''
printf, fu, ' '+systime()
close, fu
free_lun, fu

; check that the file was written
fh=file_search(filename, /full)
if fh ne '' then begin
	if file_test(filename,/user) then file_chmod, filename, /a_write, /a_read
	print, '  *** Wrote setup file ' + fh
	if not write_timcon then begin
		success=1
		return
	endif
endif else begin
	print, '  !!! Failed to write setup file '+filename
	res=dialog_message('Failed to write setup file '+filename)
	return
endelse

; optionally run Ben Penaflor's script to send setup file to timcon on d3pcs2
if write_timcon then begin
	itcfile=strpos(filename,'/',/reverse_search)
	if itcfile gt -1 and itcfile lt strlen(filename) then $
		fnm=strmid(filename, itcfile+1) else fnm=filename
	print, '  *** calling timcon_upload'
	print, '________________________________________'
	spawn, '$ECHRES_PATH/timcon_upload.csh '+filename+' '+user
	print, '________________________________________'
	print, ''
	IF strmatch(filename,'/fusion/projects/*',/FOLD_CASE) THEN BEGIN
		print, '  *** Sent setup file '+fnm+' to TIMCON under user '+user 
	ENDIF ELSE BEGIN
		print, 'sending to TIMCON might have failed due to permission issue'
		print, 'please save the setup file to publically accessible area,'
		print, 'such as /fusion/projects/results/echres/username/'
	ENDELSE 
endif
success=1
done: 
return
end

