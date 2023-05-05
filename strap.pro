; strips apostrophes from start and end of argument

function strap, txt, no_arrsca=no_arrsca
; set keyword no_arrsca to skip running arrsca on output

nt=n_elements(txt)
res=strarr(nt)
for j=0, nt-1 do begin
	i=strpos(txt[j],"'")
	if i eq 0 then res[j]=strmid(txt[j],1,strlen(txt[j])-2) $
		else res[j]=txt[j]
endfor
if not keyword_set(no_arrsca) then begin
	arrsca, res, result
	return, result
endif else return, res
end
