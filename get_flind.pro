;get_flind.pro
; Inputs: linear monotonic array A, target value v
; Outputs: floating index corresponding to v
; R. Prater

function get_flind, A, v

; check number of elements in A
n=n_elements(A)
if n lt 2 then return, -1.

; check that A is monotonic
if A[n-1] le A[0] then return, -1.
s=shift(A,1)-A
i=where(s[1:*] ge 0, count)
if count gt 0 then return, -1

; check v is within range
i=where(v lt A[0], count1)
i=where(v gt A[n-1], count2)
if count1 gt 0 or count2 gt 0 then return, -1

nv=n_elements(v)
out=fltarr(nv)
for j=0, nv-1 do out[j]=interpol(findgen(n), A, v[j])

return, out
end
