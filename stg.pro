; stg.pro
; returns a string with data to left in field of width leng,
; given an input s placed into format form

function stg, s, form, leng

fmt='('+form+')'
st=strtrim(string(s, format=fmt), 2)
strg=string(st, format='(a' + strtrim(string(leng), 2) + ')')
return, strg
end
;pad=leng-strlen(st)
;spc=''
;for ii=0, pad/2 - 1 do spc=spc + ' '
;if pad/2 eq pad/2. then st=spc + st + spc $ 	;(even number)
;else st=spc + st + spc + ' '
;return, st
;end
