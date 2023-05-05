;*************************************************************************
;
;  4dlib/EFITLIB/avaluesx.pro
;
;  created:
;    19910622
;
;  modified:
;   20120725 SMF - Remove order.  Base a.d index off of readanames.  
;   20090506 SMF - Added MW and MH to display grid size (from g keyword)
;   20090504 SMF - Cleaned up old code.  Added Version field. 
;   19980908 QP  - Added keyword time for sub-millisecond. The time
;		   in A is truncated to millisecond.
;   19980410 QP  - modified the PS part.
;   19980220 QP  - eliminate common blocks, turn Pro into Function
;    		   and return the list of variables
;   19970716 TT  - plot on right for printer.
;                  Added xadd and xfitplotcom
;   19950720 KG  - Reformat source file.
;
;***********************************************************************
;
; Write some of the A eqdsk file stuff to the plot device. 
; 

FUNCTION avaluesx,a,time=time,g=g

IF N_Elements(time) EQ 0 THEN time = a.time

aplotnames,s,factor,readaname
numplot=n_elements(readaname)
xadd = 0.

; Postscript output?  Is this for printing?
if ( strtrim(!D.(0)) eq 'PS'  ) then begin

  x=0.001+xadd
  y=0.97
  numlen = 60
  sout='shot!C'
  if numplot le numlen then sout =  sout + 'time!C'
  for i=0,min([numlen,numplot-1]) do begin
    sout = sout + s(i) + '!C'
  endfor
  xyouts,x,y,sout,/normal,color=color_index('Foreground')

  sout = string(a.shot,'(i10)')+'!C'
  if numplot le numlen then sout =  sout + strtrim(time,2)+'!C'
  for i=0,min([numlen,numplot-1]) do begin
    tag_num = where( tag_names(a.d) eq strupcase(readaname[i]) )
    if (tag_num ne -1) then begin
    	tag_num = where( tag_names(a.d) eq strupcase(readaname[i]) )   
    	sout = sout + strtrim(string(factor(i)*a.d.(tag_num),'(f13.3)'),2) +'!C'
    endif
  endfor
  xyouts,x+0.28,y,sout,align=1.0,/normal,color=color_index('Foreground')

  x=0.28+xadd
  sout='time!C'
  if(numplot le numlen) then return,' '
  for i=numlen,numplot-1 do begin
    sout = sout + s(i) + '!C'
  endfor
  xyouts,x,y,sout,/normal,color=color_index('Foreground')

  sout = strtrim(time,2)+'!C'
  for i=numlen,numplot-1 do begin
    tag_num = where( tag_names(a.d) eq strupcase(readaname[i]) )
    if (tag_num ne -1) then begin
    	tag_num = where( tag_names(a.d) eq strupcase(readaname[i]) )
    	sout = sout + strtrim(string(factor(i)*a.d.(tag_num),'(f13.3)'),2) +'!C'
    endif
  endfor
  xyouts,x+0.28,y,sout,align=1.0,/normal, color=color_index('Foreground')

  if ( strtrim(!D.(0)) eq 'PS' ) then $ 
      xyouts, .5+xadd, 1.1,'EFIT Results',/normal,align=.5,$
	color=color_index('Foreground')

  slist = ''

endif else begin

  ; Display the A Values 

  ; Shot
  slist = strarr(numplot+5) 
  dum =      '                     '
  slist(*) = '                     '
  strput , dum , 'Shot:', 0
  strput , dum , string(a.shot,'(i10)') , 8 
  slist(0) = dum

  ; Time
  dum =      '                           '
  strput , dum , 'Time:', 0
  strput , dum , strtrim(time,2) , 8
  slist(1) = dum

  ; Version  
  dum =      '                           '
  strput, dum , 'Version:', 0
  strput, dum , strtrim(a.MF1+a.MF2,2),8 
  slist(2) = dum

  ;; MW
  dum =      '                      '
  strput , dum , 'MW:', 0
  if keyword_set(g) then begin
     if struct_hastag(g,'MW') then strput, dum , strtrim(g.MW,2),8
  endif
  slist(3) = dum

  ;; MH
  dum =      '                      '
  strput , dum , 'MH:', 0
  if keyword_set(g) then begin
     if struct_hastag(g,'MH') then strput, dum , strtrim(g.MH,2),8
  endif
  slist(4) = dum

  ; Aplotnames Fields
  for i = 0,numplot-1 do begin
    dum =      '                      '
    strput,dum,strtrim(s(i),2), 0
    tag_num = where( tag_names(a.d) eq strupcase(readaname[i]) )
    if (tag_num ne -1) then begin
    	dum1 = strtrim(string(format = '(f13.3)',factor(i)*a.d.(tag_num) ),2)
    	n = strlen(dum1)
    	strput,dum,dum1 , 21 - n
    	slist(i+5) = dum
    endif
  endfor
endelse

return,slist
end
