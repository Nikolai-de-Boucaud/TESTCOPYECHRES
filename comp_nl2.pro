; file comp_nl2.pro
;
; comp_nl2 compares two namelist structures read by read_nl2 and prints the
; differences in the structures and/or the data
; Arguments: ascii namelist files 1 and 2
;	If filenames are absent, filename requesters are put up
;
;**************************************************
@$ECHRES_PATH/write_nl2
@$ECHRES_PATH/read_nl2

pro comp_nl2, file1, file2, delim=delim
pt=n_params()
if pt eq 1 or pt > 2 then begin
	print, 'Wrong number of arguments.'
	goto,done
endif
if pt eq 0 then begin
	file1=Dialog_Pickfile(title='Namelist file 1:',/must_exist)
	file2=Dialog_Pickfile(title='Namelist file 2:',/must_exist)
endif

if not keyword_set(delim) then delim='&'

cd, current=current
loc1=strpos(file1, '/')
loc2=strpos(file2, '/')
if ((loc1 eq -1) or (loc1 gt 0)) then file1=current+'/'+file1
if ((loc2 eq -1) or (loc2 gt 0)) then file2=current+'/'+file2

fh=findfile(file1, count=count1)
fh=findfile(file2, count=count2)
if (count1 ne 1) then print, 'File 1 not found: '+file1
if (count2 ne 1) then print, 'File 2 not found: '+file2
if ((count1 ne 1) or (count2 ne 1)) then goto, done

tol=0.00001	; allowable fractional error in floating point variables
nl1=read_nl2(file1);, delim=delim)
nl2=read_nl2(file2);, delim=delim)

list1=tag_names(nl1)
list2=tag_names(nl2)
	
print, '**************************************************************'
print, ''
print, '> File 1 = ', file1
print, '< File 2 = ', file2
print, ''

for l1=0, n_tags(nl1)-1 do begin	; loop through all namelists in the first file
	l2t=where(list2 EQ list1[l1])	; find namelist in second file matching first
	l2=l2t[0]
		
	if l2 LT 0 then begin
		print, '********'
		print, 'Namelist ', list1[l1], ' found in file1 but not file 2; going to next list'
		print, ''
		goto, nextlist
		endif else begin
			print, '**********'
			print, 'Namelist ', list1[l1]
			print, ''
			endelse
			
	if n_tags(nl1.(l1)) GT 0 then begin
		names1=tag_names(nl1.(l1)) 
		names2=tag_names(nl2.(l2))
		endif else begin
			print, nl1.(l1)
			print, nl2.(l2)
			print, ''
			goto, nextlist
			endelse
			
	tg2=intarr(n_elements(names2))				; check off tags of namelist2 as they are found
	
	for n1=0, n_tags(nl1.(l1))-1 do begin		; loop through elements of namelist
		n2t=where(names1[n1] EQ names2)		; find tag in second file matching tag[n1] in first
		n2=n2t[0]
		if n2 LT 0 then begin					; tag[n1] missing from namelist2
			print, '> ', names1[n1],'=Not Present'
			print, '< ', names1[n1],'=', deconvert_name(names1[n1]);deconvert_name(nl1.(l1).(n1))
			print, ''
			goto, nextname
			endif else tg2[n2]=1			; mark tag found
		
		nt1=n_elements(nl1.(l1).(n1))
		nt2=n_elements(nl2.(l2).(n2))

		if nt1 NE nt2 then begin
			print, 'Different number of elements for tag ', deconvert_name(names1[n1])
			print, '    > '+strtrim(string(nt1),2)
			print, '    < '+strtrim(string(nt2),2)
			print, ''
			goto, nextname
			endif
		
		result=size(nl1.(l1).(n1))
		if result[n_elements(result)-2] EQ 4 then begin	; then it's a float
			if nt1 GT 1 then begin
				for kk=0, nt1-1 do begin	; check each element of tag name
					if (abs(nl1.(l1).(n1)[kk] - nl2.(l2).(n2)[kk]) GT abs(tol*nl1.(l1).(n1)[kk])) then begin
						print, '> ', deconvert_name(names1[n1]), $
							'[',strcompress(string(kk)),'] = ', nl1.(l1).(n1)[kk]
						print, '< ', deconvert_name(names2[n2]), $
							'[',strcompress(string(kk)),'] = ', nl2.(l2).(n2)[kk]
						print, ''
						endif
					endfor
				endif $
				else begin
					if (abs((nl1.(l1).(n1) - nl2.(l2).(n2))) GT abs(tol*nl1.(l1).(n1))) then begin
						print, '> ', deconvert_name(names1[n1]),' = ', nl1.(l1).(n1)
						print, '< ', deconvert_name(names2[n2]),' = ', nl2.(l2).(n2)
						print, ''
						endif
					endelse
				goto, nextname
			endif

		; it's a string or integer
		if nt1 GT 1 then begin
			for kk=0, nt1-1 do begin	; check each element of tag name
				if nl1.(l1).(n1)[kk] NE nl2.(l2).(n2)[kk] then begin
					print, '> ', deconvert_name(names1[n1]), $
						'[',strcompress(string(kk)),'] = ', nl1.(l1).(n1)[kk]
					print, '< ', deconvert_name(names2[n2]), $
						'[',strcompress(string(kk)),'] = ', nl2.(l2).(n2)[kk]
					print, ''
					endif
				endfor
			endif $
		else begin
			if nl1.(l1).(n1) NE nl2.(l2).(n2) then begin
				print, '> ', deconvert_name(names1[n1]),' = ', nl1.(l1).(n1)
				print, '< ', deconvert_name(names2[n2]),' = ', nl2.(l2).(n2)
				print, ''
				endif
			endelse

		nextname:
	endfor
		
	missing=where(tg2 EQ 0)
	if missing[0] GT -1 then begin
		for kk=0, n_elements(missing)-1 do begin
			print, '> ', deconvert_name(names2[missing[kk]]),' =Not present'
			print, '< ', deconvert_name(names2[missing[kk]]),' = ', nl2.(l2).(missing[kk])
			endfor	
		endif
	nextlist:
	endfor
	
for m2=0, n_tags(nl2)-1 do begin	; loop through all namelists in the second file
	m1t=where(list1 EQ list2[m2])	; find namelist in first file matching second
	m1=m1t[0]
		
	if m1 LT 0 then begin
		print, '********'
		print, 'Namelist ', list2[m2], ' found in file 2 but not in file 1; going to next list'
		print, ''
		endif
	endfor
	
done:

end

