; Input the ellipticity b/a for pure x-mode and for the applied wave,
; and input the tilt alpha in deg for x-mode and the applied wave,
; prints the fractional power in x-mode.

@find_pol.pro

dtr = !PI/180.

read, 'Enter inclination and ellipticity of x-mode (deg): ', alf_x, bet_x
start:
read, 'Enter applied inclination and ellipticity (deg): ', alf_a, bet_a

if alf_x GT 180. then alf_x=alf_x - 180.
if alf_x LT 0. then alf_x=alf_x + 180.
if abs(bet_x) GT 45. then begin
	print, 'beta__x illegal value'
	goto, start
	endif
if alf_a GT 180. then alf_a=alf_a - 180.
if alf_a LT 0. then alf_a=alf_a + 180.
if abs(bet_a) GT 45. then begin
	print, 'beta__a illegal value'
	goto, start
	endif

P_x = x_frac(alf_a*dtr, alf_x*dtr, bet_a*dtr, bet_x*dtr)
print, 'Per cent power in X-mode is ', P_x*100.
print, ''
goto, start
end
