function interpp, x, y, xtarget
nx=n_elements(x)
ny=n_elements(y)
nxt=n_elements(xtarget)

if ((nx le 1) or (ny le 1)) then return, y
if ny ne nx then return, -1
if x[nx-1] lt x[0] then return, -1
s=x-shift(x,1)
i=where(s[1:*] le 0., count)
if count gt 0 then return, -1	; non monotonic

j=where(((xtarget ge min(x)) and (xtarget le max(x))), countj)
if countj eq 0 then return, replicate(-1, nxt)

ind=interpol(findgen(nx), x, xtarget)
interpolates=interpolate(y,ind)

; return end value where xtarget is less than the minimum of x or maximum of x
k=where(xtarget lt min(x))
if k[0] ge 0 then interpolates[k]=y[0]
k=where(xtarget gt max(x))
if k[0] ge 0 then interpolates[k]=y[ny-1]

return, interpolates

end
