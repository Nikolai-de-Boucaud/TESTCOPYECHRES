function test_counts_valid, shot, tc, fc, gyrotron, counts_plot_window, sys, $
	noplot=noplot
; set noplot to avoid plotting

CATCH,Error_status
IF Error_status NE 0 THEN BEGIN
	print,!ERROR_STATE.MSG
	CATCH,/CANCEL
	return, -1
ENDIF

if n_elements(noplot) eq 0 then noplot=0

; get port limits
z=echcal(shot, gyrotron)

if z.status eq 0 then return, -1 ; no data on this gyro for this shot, assume in

if z.np_lim eq 0 then return, -1	; no data on port limits, assume in

tiltlim=z.tilt_limits
facetlim=z.facet_limits

minfacet=min(facetlim)
maxfacet=max(facetlim)
mintilt=min(tiltlim)
maxtilt=max(tiltlim)

if ((tc gt maxtilt) or (tc lt mintilt) or (fc gt maxfacet) or (fc lt minfacet)) $
	then if noplot then return, 0 else goto, plotit

sqz=10.
facetcounts=round((facetlim-min(facetlim))/sqz)
tiltcounts=round((tiltlim-min(tiltlim))/sqz)
nfacet=max(facetcounts)
ntilt=max(tiltcounts)
d=intarr(nfacet, ntilt)
inds=polyfillv(facetcounts, tiltcounts, nfacet, ntilt)
d[inds]=1
fcc=round((fc-minfacet)/sqz)
tcc=round((tc-mintilt)/sqz)

if tcc lt 0 then tcc=0 else if tcc ge ntilt then tcc=ntilt-1
if fcc lt 0 then fcc=0 else if fcc ge nfacet then fcc=nfacet-1
success=d[fcc,tcc]

if noplot then return, success

if success then begin
	if counts_plot_window gt 0 then begin
		wdelete, counts_plot_window
		counts_plot_window=-1
	endif
	return, 1
endif

; counts out of bounds; plot where
plotit:

; save for restoring
getpm=!p.multi
old_window=!d.window

if counts_plot_window le 0 then begin	; open new window
	window, xpos=500, ypos=300, xsize=600, ysize=400, retain=2, $
		title='Count location', /free
	counts_plot_window=!d.window
endif else begin
	wshow, counts_plot_window 
	wset, counts_plot_window
endelse

!p.multi=[0,1,1]	

plot, [minfacet, maxfacet], [mintilt, maxtilt], /nodata, $
	yrange=[min([mintilt, tc]), max([maxtilt, tc])], $
	xrange=[min([minfacet, fc]), max([maxfacet, fc])], $
	xtitle='toroidal counts', ytitle='poloidal counts', $
	title='System '+strtrim(sys+1,2)
plots, facetlim, tiltlim
plots, fc, tc, psym=2, symsize=2.0

wset, old_window
!p.multi = getpm

return, 0

end


; main, for testing
;shot=144000
;sys=1
;wind=-1
;a=6
;again:
;read, 'Enter tilt counts and facet counts: ', t, f
;success = test_counts_valid(shot, t, f, a, wind, sys)
;if success then print, 'This setting ok' else print, 'Too bad, you lose, try again'
;print, '  ', wind
;goto, again
;end
