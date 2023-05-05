; file read_nl2.pro
;
; read_nl2 reads Fortran style namelists and returns structure with namelist data
; 	usage: x=read_nl2(fname), where fname = namelist filename
; 	add keyword /dbl for double precision floating point
; Based on read_nl by Brad Rice, but modified by RP to 
; 	--include the entire header, not just first line
;	--handle correctly list element arrays like rya(1) which can't be written 
;		in cqlinput as rya since rya(0) is reserved for internal cql use;
;		rya(1) stored as rya$1 in the returned namelist
;	--deal adequately with list element names like abc(1,2,3), which it stores in
;		the returned namelist as abc$1$2$3
;	--deal with duplicate list names (e.g., cqlinput has two 'setup' lists),
;		which it returns as namelists by appending '$' to the list names
;	--handles a 'tailer' at the end, as required by cqlinput
;	--handles duplicate listname elements of different structure, keeping only the
;		last element data type (ie, read_nl chokes on tiin=8*0.0 in one place and 
;		tiin=11*0.0 in another place)
;	--handles namelists containing text like "abc=1,def=4", where the lack of spce
;		between the comma and the next element chokes read_nl
;	--preserves empty namelists through writing a dummy element called "dummy__name",
;		which should be ignored
;	--modified to use either &end or / to terminate a namelist (3/1/2007)
;
; write_nl2 reverses the process and write an listname file, inputting a structure
; 	written by read_nl2
; usage: write_nl2, namelist_structure, file_name
;
; comp_nl2 compares two namelist structures read by read_nl2 and prints the
; differences in the structures and/or the data
;
; None of these procedures will work with namelists that have '$' in either the
; list names or the list element names!
;

; arrsca
; Converts structure referenced in the first argument to one in the second
; argument by converting all elements of size 1 to scalars. This makes the help
; command more useful. Does the same thing for any argument not a structure.
@$ECHRES_PATH/arrsca

function  clean, st
; abc=7.,def=1. -> abc=7., def=1.
p=1
while p ge 1 do begin
	s=strpos(st,',',p)
	if s gt 0 then begin
		q2=strmid(st,s+1,1)
		q1=byte(q2)
		q=q1[0]
		if q ge 97 then strput, st, ' ', s
	endif
	p=s+1
endwhile
return, st
end		

function convert_name, name
; abc(1,2,3) -> abc$1$2$3
paren=strpos(name, '(')
if paren lt 0 then return, name	; no () in it
prefix=strmid(name, 0, paren)	; a contains the parens
suffix=strmid(name, paren,1000)
newsuf=''
for jj=0, strlen(suffix)-1 do begin
	a=strmid(suffix,jj,1)
	if ( (a eq '(') or (a eq ',') ) then a='$'
	if ( (a eq ' ') or (a eq ')') ) then a=''
	newsuf=newsuf+a
endfor
return, prefix+newsuf
end

;**********************************************************
function read_nl2, fname, dbl=dbl, delim=delim, no_arrsca=no_arrsca
; set no_arrsca to skip changing arrays of size 1 to scalars
; set dbl to use double floats
; delim = '$' or '&' or '/'

dbl=keyword_set(dbl)
x= {nodata:''}
k=file_search(fname, count=count)
if count eq 0 then return, x
tailim='/'+string(10B)	; new style namelist terminator

openr, lun, fname, /get_lun 
b=strarr(8000)				;assume < 8000 lines
c=''
i=0
while ~EOF(lun) do begin
	readf, lun, c				;read entire file into a string array
	b[i++]=c
endwhile
close, lun
free_lun, lun
b=strtrim(b,2)
b=b[where(b ne '')]
n_line=n_elements(b)

; capture entire header
header=strarr(100)	; assume < 100 lines in header
j=0
if delim ne !null then begin
	while strpos(b[j], delim) lt 0 do header[j]=b[j++]
endif else begin
	while ((strpos(b[j],'&') lt 0) and (strpos(b[j],'$') lt 0)) do header[j]=b[j++]
	if strpos(b[j],'&') ge 0 then delim='&' else delim='$'
endelse
inds=where(header ne '')
if inds[0] ge 0 then header=header[inds] else header=header[0]

; capture tailer if there is one, as in cqlinput
trailer=strarr(100)
j0=n_elements(b)-1
j=j0
while ((strpos(b[j], delim) lt 0) and (strpos(b[j], '/') lt 0) and (j lt 100)) $
	do trailer[j0-j]=b[j--]
if j ge 99 then begin	; no end to nl; assume an end!
	trailer=''
	b=[b,' /']
endif else begin
	inds=where(trailer ne '')
	if inds[0] ge 0 then trailer=reverse(trailer[inds]) else trailer = ''
endelse

;remove initial lines if not namelist
istart=0
while strpos(b(istart), delim) eq -1 do istart=istart+1
s=b(istart)

;put all lines into one string and remove leading and trailing blanks
pos1=0L & pos2=0L & pos3=0L & pos4=0L & pos5=0L & pos6=0L
for i=istart+1, n_line-1 do begin
	b[i]=strtrim(b[i],2)
	pos1=strpos(b[i], '!')	; delete comments
	if pos1 eq 0 then goto, nexti
	pos2=strpos(b[i], '#')
	if pos2 eq 0 then goto, nexti
	if ((pos1 gt 0) and (pos2 gt 0)) then b[i]=strmid(b[i], 0, min([pos1, pos2])-1) else $
	if pos1 gt 0 then b[i]=strmid(b[i], 0, pos1-1) else $
	if pos2 gt 0 then b[i]=strmid(b[i], 0, pos2-1)
	pos3=strpos(b[i], '/', /reverse_search)
	if pos3 eq strlen(b[i])-1 then $	; handle namelists ending with / rather than &end
		b[i]= strmid(b[i],0,pos3-1) + ' ' + delim + 'end' + ' '
	if b[i] ne '' then s += ' ' + b[i] ; strcompress(b(i))
	nexti:
endfor

; replace all commas by spaces, to get ITEM=1,NEXT=2 to read correctly
; but leave commas in EEGY(1,1,2,2)
max_to_parens=9
p4=0
nexts:
pos4=strpos(s, ',', p4)
if (pos4 gt 0) then begin	; found a comma
	; if it's between paranetheses then ignore it
	pos5=strpos(s, '(', pos4, /reverse_search)
	pos6=strpos(s, ')', pos4)
	if ((pos5 gt 0) and (pos6 gt 0)) then $
		if (pos4-pos5 gt max_to_parens) or (pos6-pos5 gt max_to_parens) then begin ; it's not in nearby parens
			strput, s, ' ', pos4 
			p4=pos4
		endif
	p4++
	goto, nexts
endif
s=strtrim(s,2)				

; separate into namelists
s=str_sep(s, delim)
nl=(n_elements(s)-1)/2 ; number of lists
if nl eq 0 then nl=1
w=indgen(nl)*2+1
s=s[w]					; remove $end from string array
s=strtrim(s, 2)
k=strpos(s, ' ')

; preserve empty lists
inds=where(k lt 0, count)
if count gt 0 then $
	for jj=0, n_elements(inds)-1 do $
		if (s[inds[jj]] ne '') then k[inds[jj]]=strlen(s[inds[jj]])

; extract list names
lst_nm=strarr(nl) 
for j=0, nl-1 do begin
	lst_nm[j]=strmid(s[j],0,k[j])
	s[j]=strmid(s[j],k[j],100000)		; eliminate list name from string
endfor

; eliminate duplicate list names by adding $ suffix
if nl gt 1 then begin
	for j=1, nl-1 do begin
		try_again:
		inds=where(lst_nm[0:j-1] eq lst_nm[j])
		if (inds[0] ne -1) then begin
			lst_nm[j]=lst_nm[j]+'$'
			goto, try_again
		endif
	endfor
endif

; start with header
x=create_struct('header', header)

for j=0, nl-1 do begin		;namelist loop, j refers to namelist number
	s1=s[j]
	if ((s1 eq '') and (j gt 0)) then begin
		x1=create_struct('dummy__name', 0); IDL can't have empty structure
		goto, nextlist
	endif
	s1=clean(s1)
	s_sep=strtrim(strsplit(s1, '=', /extract))
	ns=n_elements(s_sep)-1	; number of element names in jth list

	; first get a list of all the params
	param=strarr(ns+1)
	s_tmp=s_sep
	k=0
	for ii=1, ns do begin	
		param[ii]=convert_name(strtrim(strmid(s_tmp[ii-1], k, 1000), 2))
		if (ii eq ns) then s_tmp=s_tmp+' '
		k=rstrpos(s_tmp[ii], ' ')
		if k eq -1 then k=strlen(s_tmp[ii])	
	endfor
	
	; loop through namelists
	k=0
	for i=1, ns do begin
		parameter=convert_name(strtrim(strmid(s_sep[i-1], k, 1000), 2))
		if (i eq ns) then s_sep=s_sep+' '
		k=rstrpos(s_sep[i], ' ')
		if k eq -1 then k=strlen(s_sep[i])	;last value in string
		sd=strmid(s_sep[i], 0, k)			; string sd contains the data
		repeat begin						;replace commas by space in sd data
			kk=0
			kk=strpos(sd, ',', kk+1)
			if kk ne -1 then strput, sd, ' ', kk
    	endrep until kk eq -1

		; is it a string? look for ' or " or T or .true. or F or .false. 
		sdt=strupcase(string(sd))
		if ((sdt eq '.TRUE.') or (sdt eq '.FALSE.') or (sdt eq 'T') or (sdt eq 'F') or (sdt eq '.F.') or (sdt eq '.T.')) then begin
			sd=sdt
			goto, next_parameter
		endif 
		srch="'"
		q1=strpos(sd,srch)
		if q1 lt 0 then srch='"'
		q1=strpos(sd,srch)
		q2=strpos(sd,srch,q1+1)
		if ((q1 ge 0) and (q2 ge 0)) then begin	; yes it's a string
			sd=strcompress(sd)					;get rid of excess white space
			repeat begin						;replace ' ' with ','
				kk=-1
				kk=strpos(sd, "' '", kk+1)
				if kk ne -1 then strput, sd, "','", kk
			endrep until kk eq -1
			sd=str_sep(strtrim(sd, 2), ',')
		endif else begin	; it's not a string
			sd=str_sep(strtrim(strcompress(sd), 2), ' ')
			c1=strmid(sd[0],0,1)+strmid(sd[0],strlen(sd[0])-1,1) ;check 1st/last char
			if c1 ne ".." and c1 ne "ff" and c1 ne "tt" then begin
				if max(strpos(sd, '*')) ne -1 then begin		;split * into array
          			for kk=0, n_elements(sd)-1 do begin  
						y=str_sep(sd(kk), '*')                      
						if n_elements(y) eq 1 then begin
							if dbl then s1=double(y) else s1=float(y)
							if strpos(y[0],'.') eq -1 then s1=long(s1)
						endif else begin
							if dbl then y1=double(y[1]) else y1=float(y[1])
							if strpos(y[1],'.') eq -1 then y1=long(y1)
							s1=replicate(y1, y[0])
						endelse
						if kk eq 0 then s2=s1 else s2=[s2,s1]
					endfor
					sd=s2
				endif else if max(strpos(sd,'.')) eq -1 then sd=long(sd) $		; convert to long
						else if dbl then sd=double(sd) else sd=float(sd)	; convert to float
			endif
		endelse

		next_parameter:
		
		;***
		if (i eq 1) then x1=create_struct(parameter, sd) else begin $
			wx=where(parameter eq param)	; keep only the *last* instance of list element
			if i eq max(wx) then x1=create_struct(x1, parameter, sd)
		endelse
	endfor  ; end parameter i loop
nextlist:
	if not keyword_set(no_arrsca) then arrsca, x1, y1 else y1=x1
	x=create_struct(x, lst_nm[j], y1)
endfor	; end of namelist j loop

if ((n_elements(trailer) gt 1) or (trailer[0] ne '')) then x=create_struct(x, 'trailer', trailer)

close, lun
free_lun, lun

return, x

end
