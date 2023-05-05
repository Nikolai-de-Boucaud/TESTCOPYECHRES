; arrsca
; Converts structure referenced in the first argument to one in the second
; argument by converting all elements of size 1 to scalars. This makes the help
; command more useful. Does the same thing for any argument not a structure.

pro arrsca, strin, strout
sz=size(strin, /type)
if sz eq 8 then begin
	nms=tag_names(strin)
	nel=n_elements(nms)
	if nel ge 1 then begin
		a=strin.(0)
		if n_elements(a) eq 1 then b=a[0] else b=a
		strout=create_struct(nms[0], b)
		if nel ge 2 then begin
			for i=1, nel-1 do begin
				if n_elements(strin.(i)) eq 1 then a=strin.(i)[0] $
					else a=strin.(i)
				strout=create_struct(strout, nms[i], a)
			endfor
		endif
	endif
endif else if n_elements(strin) eq 1 then strout=strin[0] else strout=strin

end
