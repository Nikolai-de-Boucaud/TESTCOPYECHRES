; Returns values of y(x) interpolated at xtarget
; X must be monotonic
; R. Prater

function interp, x, y, xtarget
on_error, 0
nx=n_elements(x)
if n_elements(y) ne nx then message, 'y must have same number of elements as x'
if nx lt 2 then return, replicate(y[0], n_elements(xtarget))
i=where(((xtarget gt max(x)) or (xtarget lt min(x))), count)
if count gt 0 then message, 'xtarget must lie in range of x array'
if nx eq 2 then goto, interpolate

; see if x is monotonic
s=shift(x,1)-x
i=where(s[1:*] gt 0.0, count)
if count ne 0 then message, 'x array must be monotonic'
i=where(s[1:*] eq 0.0, count)
if count ne 0 then begin
	;see whether the problem points are near the target point
	for k=0, count-1 do begin
		
	endfor
endif	; message, 'x must be monotonic'
interpolate:
ind=interpol(findgen(n_elements(x)), x, xtarget)
return, interpolate(y, ind)
end
