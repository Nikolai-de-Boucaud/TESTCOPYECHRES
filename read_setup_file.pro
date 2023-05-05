function read_setup_file, infile=infile, setup_dir=setup_dir

; get the file for which gyrotron, which polarizer settings, etc
if not keyword_set(setup_dir) then setup_dir='./'

if not keyword_set(infile) then $
	infile=dialog_pickfile(filter='*.setup',title='Select setup file: ', $
		path=setup_dir, get_path=setup_dir, /must_exist)

if infile eq '' then $
	infile=dialog_pickfile(filter='*.setup',title='Select setup file: ', $
		path=setup_dir, get_path=setup_dir, /must_exist)

; tplt is template made by ascii_template
tplt={version:1.0, datastart:long(0), delimiter:byte(9), $
	missingvalue:10000000.0, $
	commentsymbol:';', fieldcount:long(5), $
	fieldtypes:fix([7,4,4,3,3]), $
	fieldnames:['field1', 'field2', 'field3', 'field4', 'field5'], $
	fieldlocations:long([0,9,14,20,25]), $
	fieldgroups:[0,1,2,3,4]}
d=read_ascii(infile, template=tplt)
shot=long(d.field1[0])
num_sys=fix(d.field1[1])
gyroname=strupcase(string(d.field1[2:1+num_sys]))
for i=0, num_sys-1 do begin
	if strpos(gyroname[i], '"') ge 0 then gyroname[i]=strmid(gyroname[i], 1, strlen(gyroname[i])-2)
	if strpos(strupcase(gyroname[i]), 'ROW') gt 0 then gyroname[i]='SCARECROW' 
endfor
pol1=float(d.field2[2:1+num_sys])
pol2=float(d.field3[2:1+num_sys])
tiltct=long(d.field4[2:1+num_sys])
;tiltct= (abs(tiltct) lt 15000) * tiltct 
facetct=long(d.field5[2:1+num_sys])
;facetct= (abs(facetct) lt 15000) * facetct 
sfiledata={success:1, shot:shot, num_sys:num_sys, gyroname:gyroname, $
	pol1:pol1, pol2:pol2, tiltct:tiltct, facetct:facetct, $
	setup_dir:setup_dir, infile:infile}
return, sfiledata
end
