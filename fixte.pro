function fixte, rho, te, temin=temin, delte=delte, nrho=nrho,echres=echres

; This function adds a small increment to the electron temperature, since
; ONETWO chokes when Te is less than about 3 eV due to lack of charge balance.
; Fixes error 'ERROR:ZEFF IMPLIES NEGATIVE ION DENSITY AT TIME *.***', or
; 'STOP in subroutine ZEN: negative ion density' which comes up in versions of onetwo
; greater than or equal to 4.26, which requires more like 10 eV minimum.
;
; See note by Holger St John, /p/linux/onetwo/pgf90/Zeff_problem.pdf
;
; Per Craig Petty's suggestion, warning messages will be given and 
; Te should be raised by 10eV for all situations (instead of 'break' for success=2,3
; since one using zipfit for toray calculation is likely not care about the details of
; the profile that much.

if not keyword_set(temin) then temin=0.010	; minimum Te (keV) to worry about
if not keyword_set(delte) then delte=0.010	; add to minimum Te (keV)
if not keyword_set(nrho) then nrho=3 ; number of rho's to feather in delte

n=n_elements(te)

; find whether Te too low anywhere
j=where(te lt temin, count)

if count eq 0 then return, {success:1, rho:rho, te:te}

if count eq 1 then count2=0

; check if low Te region is not continuous
if count gt 1 then begin
	js=j-j[1:*]
	bjs=where(js ne -1, count2)
endif

; check if low Te is not near the edge
if rho[j[0]] lt 0.92 then begin
	success=2
	IF keyword_set(echres) THEN BEGIN
		if count2 eq 0 then dummy=dialog_message('Te < 10 eV inside rho=0.92; 10 eV added') else $
		dummy=dialog_message('Te < 10 eV inside rho=0.92 in more than one noncontinous location; 10 eV added')
	ENDIF
endif


; check if edge Te is too low 
if rho[j[0]] ge 0.92 then begin
	success=3
	IF keyword_set(echres) THEN BEGIN
		if count2 eq 0 then dummy=dialog_message('Added 10 eV to Te (rho>0.92)') else $
		dummy=dialog_message('Te < 10 eV at rho>0.92 in more than one noncontinous location; 10 eV added')	
	ENDIF
endif

; fix it
dte=fltarr(n)
dte[j[0]:*]=1.0
kk=0
for k=j[0]-1, j[0]-nrho, -1 do dte[k]=0.5^kk++
nte=te+delte*dte

return, {success:success, rho:rho, te:nte}

end
