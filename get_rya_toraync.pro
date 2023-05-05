; get_rya_toraync.pro
; Inputs:
;	--num_rya	maximum number of rya 
; Outputs:
;	--returns rya values in an ordered array 
;	--num_rya set to number of rya elements

function get_rya_toraync, ncfile, num_rya, toray_window

old_window=!D.WINDOW

if num_rya gt 24 then begin
	print, '  !!! get_rya: num_rya too large: '+string(num_sys)
	print, '      => setting num_rya to 24'
	num_rya=24
endif
rya=fltarr(2*num_rya)

fh=file_search(ncfile, count=count)
if count ne 1 then message, 'File '+ncfile+' not found'
t=readnc(ncfile)
rho=float(t.xmrho)
nrho=n_elements(rho)
totpow=t.weecrh

;if max(totpow) le 0.005 then begin
;	res=dialog_message('No EC power; aborting cql3d', /info)
;	rya=fltarr(num_rya)
;	goto, done
;endif

ntotpow=totpow/max(totpow)	; normalized power profile from toray
ntotpowp=ntotpow-0.5

drho_min=0.005	; typical value for ds=0.2 and fdout=5

; find zero crossings of ntotpow-0.5
;plot, rho, ntotpow, xtitle='rho', ytitle='normalized power density from toray',$
;	title='Location of FPed surfaces (rya)'

if ((toray_window ge 0) AND windowavailable(toray_window)) then wset, toray_window 

; revised 11/14/2018
ind=where(ntotpow gt 0.02, nind)
if nind eq 0 then begin
	result=dialog_message('No points for cql3d', /info)
	goto, done
endif
ind1=ind[0]
ind2=ind[where(ind eq max(ind))]
if t.xmrho[ind1] gt 0.1 then llim=t.xmrho[ind1]-0.05 else llim=t.xmrho[ind1]
if t.xmrho[ind2[0]] lt 0.9 then ulim=t.xmrho[ind2[0]]+0.05 else ulim=t.xmrho[ind2[0]]
rya=get_range(llim, ulim, num_rya)
if ((toray_window ge 0) AND windowavailable(toray_window)) then $
   oplot, rya, fltarr(n_elements(rya)), psym=4, symsize=1.2
goto, done


rho_p5=rho[where(ntotpowp[*]*ntotpowp[1:*] lt 0.)]
npeaks=n_elements(rho_p5)/2
if npeaks eq 1 then begin
	ppk=6	; number of rya across peak
	hppk=5	; number of rya on each side of peak
	drho=abs(rho_p5[0]-rho_p5[1])/float(ppk)	; separation of rya's
	if drho lt drho_min then drho=drho_min
	rya=rho_p5[0]+( (indgen(ppk+2*hppk)-hppk+1)*drho)
	maxrya=max(rya)
	case 1 of 	; add some more outside of peak
		(maxrya le 0.2): rya=[rya, 0.22, 0.25, 0.30, 0.5, 0.7, 0.95]
		(maxrya le 0.3): rya=[rya, 0.33, 0.38, 0.45, 0.65, 0.95]
		(maxrya le 0.4): rya=[rya, 0.43, 0.48, 0.55, 0.75, 0.95]
		(maxrya le 0.5): rya=[rya, 0.53, 0.58, 0.65, 0.8, 0.95]
		(maxrya le 0.6): rya=[rya, 0.64, 0.7, 0.78, 0.95]
		(maxrya le 0.7): rya=[rya, 0.74, 0.8, 0.86, 0.95]
		(maxrya le 0.8): rya=[rya, 0.85, 0.95]
		else: rya=[rya, 0.95]
	endcase
endif

if npeaks gt 1 then begin	; add this code later
	s=where(ntotpow gt 0.001)
	minrho=min(rho[s])
	maxrho=max(rho[s])
	if maxrho gt 1./1.05 then maxrho=1./1.05-0.005
	rya=get_range(0.95*minrho, maxrho*1.05, num_rya)
endif

rya=rya[where(rya gt 0.)]
rya=rya[sort(rya)]
if n_elements(rya) gt num_rya then rya=rya[0:num_rya-1]

if ((toray_window ge 0) AND windowavailable(toray_window)) then $
   oplot, rya, fltarr(n_elements(rya)), psym=4

done:

num_rya=n_elements(rya)
wset, old_window
return, rya
end
