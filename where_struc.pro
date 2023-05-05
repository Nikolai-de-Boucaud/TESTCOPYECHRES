; only works on "flat" structures, i.e., the elements must not be arrays
; or other structures. Actually, the structure can contain arrays, but the
; comparison is only done on the first element.
;
;  WRITTEN 14-Aug-2006 by Bill Davis
;
function where_struc, instrc, lookfor

if size(instrc,/type) ne 8 then begin
   print, 'Usage:  IDL> index = where_struc( instrc, lookfor )
   return, -1
endif

ntags = n_tags( instrc )

for i=0,ntags-1 do if instrc.(i)[0] eq lookfor then break

if i ge ntags then return, -1

return, i

end
