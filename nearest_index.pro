; nearest_index.pro
; like where(a eq b) but finds nearest value if a ne b anywhere
; If a eq b for more than one index, return the first case
; a is input array
; b is target values, can be array
function nearest_index, a, b, diff=diff, max_diff=max_diff, count=count
nb=n_elements(b)
indx=-1L
diff=0.0
count=0
for k=0, nb-1 do begin
	i=where(a eq b[k], countb)
	if countb gt 0 then begin
		indx=[indx,i[0]]
		diff=[diff,0.]
	endif else begin
		val=abs(a-b[k])
		i=where(val eq min(val))
		if n_elements(max_diff) gt 0 then begin
			if val[i[0]] le max_diff then begin
				indx=[indx,i[0]]
				diff=[diff,val[i[0]]]
			endif else begin
				indx=[indx, -1]
				diff=[diff, 0.0]
			endelse
		endif else begin
			indx=[indx,i[0]]
			diff=[diff,val[i[0]]]		
		endelse
	endelse
endfor
count=n_elements(indx)
if count gt 1 then begin
	indx=indx[1:*]
	diff=diff[1:*]
	count--
endif
return, indx
end

