; rq.pro
; function to return the rho and minimum R at z=0. of some target q0

function rq, g, q0, f=f
; g is g-structure
; f is fluxfun structure
; q0 is the target q

st={rq, R_q:0.0, rho_q:fltarr(4)}

catch, error
if error ne 0 then begin
	print, ' !!! rq: ' + !error_state.msg
	catch, /cancel
	return, st
endif

if not keyword_set(f) then f=fluxfun_ech(g)

fin=where(finite(f.f) ne 1, count)
if count ne 0 then return, st 
if min(f.q) gt q0 then return, st

; find minimum value of major radius at the midplane where q=q0
indq0=interpol(findgen(n_elements(f.q)), f.q, q0)
psiq0=interpol(f.psi, indgen(n_elements(f.psi)), indq0)
izmid=fix(g.mh/2.)
mxindr=where(g.psirz[*,izmid]  eq min(g.psirz[*,izmid]))
mxindr=mxindr[0]
indr =interpol(findgen(n_elements(g.psirz[0:mxindr,izmid])), $
        g.psirz[0:mxindr,izmid], psiq0)
st.r_q=interpol(g.r, indgen(n_elements(g.r)), indr)

; get rho where q=q0 also
; is q montonic? Use the part with q>1.2
x=f.rho/f.rho[n_elements(f.rho)-1]
y=f.q-q0
in=where(y eq 0., count)
if count gt 0 then begin		; some radial points are exactly q0
	st.rho_q[0:count-1] = x[in]
	return, st
endif
yp=shift(y,1)
in=where(y*yp lt 0., count)		; find a zero crossing
if count le 1 then return, st	; no match, only false one at origin
in=in[1:*]						; get rid of sign change at origin
for i=0, count-2 do begin		; for each zero crossing, interpolate
	flin=get_flind(y[in[i]-1:in[i]], 0.)
	st.rho_q[i]=interpolate(x, in[i]-1+flin)
endfor
return, st
end
