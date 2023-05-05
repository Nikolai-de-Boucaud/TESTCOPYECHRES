; file write_nl2.pro
;
; write_nl2 writes a listname file, inputting a structure written by read_nl2
; usage: write_nl2, namelist_structure, file_name
;

function deconvert_name, name
; inverts convert_name:  abc$1$2 -> abc(1,2)
k=strpos(name, '$')
if k eq -1 then return, name
newname=strmid(name, 0, k)
flag=0
for jj=k, strlen(name)-1 do begin
	a=strmid(name,jj,1)
	if a eq '$' then if flag eq 0 then begin
		a='(' 
		flag=1 
	endif else a=','
	newname=newname+a
endfor
return, newname+')'
end

;**************************************************
;write_nl2.pro
; Writes a namelist file from a namelist structure
; Improved version of write_nl to include multiple dimensions
;	and other namelist issues--see read_nl2.pro

pro write_nl2, nlist, fname, verbose=verbose, delim=delim, slashterm=slashterm
x = nlist
tname=tag_names_h(x)
nt=n_elements(tname)
if not keyword_set(delim) then delim='$'
if keyword_set(slashterm) then term='/' else term=delim+'END'

openw,iu,fname,/get_lun

i = where(tag_names_h(x) eq 'HEADER', header)
if (header eq 1) then begin
	for ii=0, n_elements(x.header)-1 do $
		printf, iu, x.header[ii]		;first tag name should always be a header
	jstart = 1
endif else jstart=0

for j=jstart, nt-1 do begin	; namelist loop
	lname=tname[j]
	; remove trailing $
	pos=strlen(lname)
	while strpos(lname, '$', pos, /reverse_search) gt 0 do pos=pos-1
	if pos gt 0 then lname=strmid(lname, 0, pos+1)	
	if lname eq 'TRAILER' then begin
		for ii=0, n_elements(x.trailer)-1 do printf, iu, x.trailer[ii]
		goto, done
	endif else printf, iu,' ' + delim + lname	; print list name
	params=tag_names_h(x.(j))
	n=n_elements(params)
	for i=0, n-1 do begin	; print parameter names and values
		w=where(x.(j).(i)(*) eq x.(j).(i)(0))
		n1=n_elements(x.(j).(i))
		param=deconvert_name(params[i])
		if param eq 'DUMMY__NAME' then goto, nextelement
		if (n_elements(w) eq n1) and (n1 gt 1) then begin 
			printf, iu, ' '+param+' = ', strtrim(string(n1),2)+'*'+ $
				strtrim(string(x.(j).(i)(0)),2)
		endif else printf, iu, ' '+param+' = ', strcompress(x.(j).(i))
	nextelement:
	endfor
	printf,iu,' ' + term
endfor

done:
if (keyword_set(verbose)) then print, 'Wrote: '+fname
close, iu
free_lun, iu
return
end
