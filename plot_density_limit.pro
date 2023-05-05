; plot_density_limit

pro plot_density_limit_event, ev

catch, an_error
if an_error ne 0 then begin
	print, '  !!! plot_density_limit_event: error: ', !err_string
	catch, /cancel
	return
endif

widget_control, ev.id, get_uvalue=uval
case uval of

	'CloseDenLim': begin
		widget_control, ev.top, /destroy
		return
	end

	else: 
	
endcase

end


pro plot_density_limit, fabs, ene, gyroname
Common ECHCOM
Common WIDGETIDS, WidIDs

;catch, an_error
;if an_error ne 0 then begin
;	print, '  !!! plot_density_limit: error: ', !err_string
;	catch, /cancel
;	return
;endif

; clean up inputs
i=where(ene gt 0.)
ene=ene[i]
fabs=fabs[i]
i=sort(ene)
ene=ene[i]
fabs=fabs[i]

;print, ene
;print, 1.-fabs

if xregistered('plot_density_limit') then begin

start_denplot:

density_margin=1.
min_absorption=0.98

old_window=!D.WINDOW
wset, PrgDat.density_limit_window

if PrgDat.Profiles.denv1 lt 1.0e8 then begin
	print, '  *** Getting CO2 densities'
	gadat, xx, yy, 'DENV1', PrgDat.shot
	if n_elements(xx) gt 100 then begin
		ixx=nearest_index(xx, PrgDat.time)
		yy=smooth(yy,10)
		if ixx gt 0 then PrgDat.Profiles.denv1=yy[ixx]
	endif else begin
		print, 'No denv1 data returned'
		PrgDat.Profiles.denv1=0.0
	endelse

	gadat, xx, yy, 'DENV2', PrgDat.shot
	if n_elements(xx) gt 100 then begin
		ixx=nearest_index(xx, PrgDat.time)
		yy=smooth(yy,10)
		if ixx gt 0 then PrgDat.Profiles.denv2=yy[ixx]
	endif else begin
		print, 'No denv2 data returned'
		PrgDat.Profiles.denv2=0.0
	endelse

	gadat, xx, yy, 'DENV3', PrgDat.shot
	if n_elements(xx) gt 100 then begin
		ixx=nearest_index(xx, PrgDat.time)
		yy=smooth(yy,10)
		if ixx gt 0 then PrgDat.Profiles.denv3=yy[ixx]
	endif else begin
		print, 'No denv3 data returned'
		PrgDat.Profiles.denv3=0.0
	endelse

	gadat, xx, yy, 'DENR0', PrgDat.shot
	if n_elements(xx) gt 100 then begin
		ixx=nearest_index(xx, PrgDat.time)
		yy=smooth(yy,10)
		if ixx gt 0 then PrgDat.Profiles.denr0=yy[ixx]
	endif else begin
		print, 'No denr0 data returned'
		PrgDat.Profiles.denr0=0.0
	endelse

	gadat, xx, yy, 'DENSITY', PrgDat.shot
	if n_elements(xx) gt 100 then begin
		ixx=nearest_index(xx, PrgDat.time)
		yy=smooth(yy,10)
		if ixx gt 0 then PrgDat.Profiles.density=yy[ixx]
	endif else begin
		print, 'No DENSITY data returned'
		PrgDat.Profiles.density=0.0
	endelse
	print, '  *** Done getting CO2 data'
endif

if PrgDat.Profiles.ShotCentralDensity gt 0. then begin
	DENSITY_ratio=PrgDat.Profiles.density/PrgDat.Profiles.ShotCentralDensity
	denv1_ratio=PrgDat.Profiles.denv1/PrgDat.Profiles.ShotCentralDensity
	denv2_ratio=PrgDat.Profiles.denv2/PrgDat.Profiles.ShotCentralDensity	
	denv3_ratio=PrgDat.Profiles.denv3/PrgDat.Profiles.ShotCentralDensity
	denr0_ratio=PrgDat.Profiles.denr0/PrgDat.Profiles.ShotCentralDensity
endif else begin
	DENSITY_ratio=0.0
	denv1_ratio=0.0
	denv2_ratio=0.0
	denv3_ratio=0.0
	denr0_ratio=0.0
endelse

; find minimum density where one gyrotron is less than 98% absorbed and closet to 98% absorption
eneintpol=get_range(ene[0], ene[n_elements(ene)-1],100)
tempfabs=interpol(fabs, ene, eneintpol)
idenmin=min(where(tempfabs lt min_absorption))
denmin=eneintpol[idenmin]*density_margin

print, ''
print, '  *** Maximum allowed central density='+strtrim(denmin,2)
print, '  *** Maximum allowed value DENSITY=' + $
	strtrim(denmin*PrgDat.Profiles.density/PrgDat.Profiles.ShotCentralDensity,2)
print, '  *** Maximum allowed value denv1=' + $
	strtrim(denmin*PrgDat.Profiles.denv1/PrgDat.Profiles.ShotCentralDensity,2)
print, '  *** Maximum allowed value denv2=' + $
	strtrim(denmin*PrgDat.Profiles.denv2/PrgDat.Profiles.ShotCentralDensity,2)
print, '  *** Maximum allowed value denv3=' + $
	strtrim(denmin*PrgDat.Profiles.denv3/PrgDat.Profiles.ShotCentralDensity,2)
print, '  *** Maximum allowed value denr0=' + $
	strtrim(denmin*PrgDat.Profiles.denr0/PrgDat.Profiles.ShotCentralDensity,2)
print, ''

!p.multi=[0,1,1]
y0=0.87
x0=0.21
dy0=0.03
chs=0.6

dest=0
black=1
file='plot_density_limit.ps'

plotit:
	
plot, findgen(10), /nodata, yr=[0,0.5], xr=[denmin/1.e13-2.0,denmin/1.e13+2.0], $
	xtit='Central density (10^19/m^3)', ytit='Frac power not absorbed', $
	tit=PrgDat.shottime, color=black
oplot, [0,20], [1.0-min_absorption,1.0-min_absorption], linestyle=1, color=black
oplot, [denmin, denmin]/1.e13, [0., 0.2], linestyle=2, color=black

src=strsplit(PrgDat.source, /extract)
if n_elements(src) gt 6 then if src[0] eq 'MDSplus,' then $
	src=strmid(src[6],0,6)
xyouts, x0, y0, src, charsize=chs, color=black, /normal
y0 -= dy0

case PrgDat.Profiles.source of
	'gaprofiles': xyouts, x0, y0, 'gaprofiles in '+PrgDat.prof_dir, charsize=chs, $
		color=black, /normal
	'zipfits': xyouts, x0, y0, 'zipfits', color=black, /normal, charsize=chs
	else: 
endcase
y0 -= 2*dy0
;xyouts, x0, y0, 'Maximum pointnames allowed (including ' + $
;	string((1.-density_margin)*100.,format='(f4.1)')+'% margin): ', /norm, $
;	color=black, charsize=chs
xyouts, x0, y0, 'Maximum pointnames allowed ', /norm, $
	color=black, charsize=chs
y0 -= dy0
xyouts, x0, y0, '  DENSITY: '+strtrim(density_ratio*denmin,2), /norm, color=black, charsize=chs
y0 -= dy0
xyouts, x0, y0, '  denv1: '+strtrim(denv1_ratio*denmin,2), /norm, color=black, charsize=chs
y0 -= dy0
xyouts, x0, y0, '  denv2: '+strtrim(denv2_ratio*denmin,2), /norm, color=black, charsize=chs
y0 -= dy0
xyouts, x0, y0, '  denv3: '+strtrim(denv3_ratio*denmin,2), /norm, color=black, charsize=chs
y0 -= dy0
xyouts, x0, y0, '  denr0: '+strtrim(denr0_ratio*denmin,2), /norm, color=black, charsize=chs
y0 -= dy0
xyouts, x0, y0, '  Maximum n_e(0): '+strtrim(denmin,2), /norm, color=black, charsize=chs
y0 -= 2*dy0

; plot the density behavior for each gyrotron
col=2
;for i=0, n_elements(PrgDat.Status.plotrays)-1 do begin
;	col++
;	if col eq 5 or col eq 7 then col++
;	if PrgDat.Status.plotrays[i] then begin
		oplot, ene/1.e13, 1.-fabs, color=col
		oplot, ene/1.e13, 1.-fabs, psym=1, color=col
		xyouts, x0, y0, gyroname, color=col, $
			charsize=chs, /norm
;		y0 -= dy0
;	endif
;endfor

if dest eq 1 then begin ; write to postscript file
	y0=-0.1
	dy0=0.05
	chs=1.0

	xyouts, x0, y0, PrgDat.Prefs.user+'    '+systime(), /norm, color=black, charsize=chs
	y0 -= dy0
	xyouts, x0, y0, PrgDat.shottime, /norm, color=black, charsize=chs
	y0 -= 2*dy0

	xyouts, x0, y0, 'DENSITY/n_e(0)='+strtrim(density_ratio,2), charsize=chs, $
		color=black, /norm
	y0 -= dy0
	xyouts, x0, y0, 'denv1/n_e(0)='+strtrim(denv1_ratio,2), charsize=chs, color=black, /norm
	y0 -= dy0
	xyouts, x0, y0, 'denv2/n_e(0)='+strtrim(denv2_ratio,2), charsize=chs, color=black, /norm
	y0 -= dy0
	xyouts, x0, y0, 'denv3/n_e(0)='+strtrim(denv3_ratio,2), charsize=chs, color=black, /norm
	y0 -= dy0
	xyouts, x0, y0, 'denr0/n_e(0)='+strtrim(denr0_ratio,2), charsize=chs, color=black, /norm
	y0 -= 2*dy0
	
endif

if dest eq 0 then begin ; switch to ps device
	dest=1
	set_plot, 'ps'
	device, /inches, yoffset=5.0, ysize=5.0, xsize=7.0, /color, /COURIER, $
		file=file, /portrait
	!p.thick=1.5
	!p.charthick=1.5
	!x.thick=2.0
	!y.thick=2.0
	!p.charsize=.7
	y0=0.90
	x0=0.12
	chs=1.0
	goto, plotit
endif else begin
	device, /close
	set_plot, 'x'
	dest=0
	!p.thick=1.250
	!p.charthick=1.250
	!p.charsize=1.0
	!x.thick=1.25
	!y.thick=1.25
	print, '  *** Wrote file '+PrgDat.gdir+file
endelse

; done with plotting, return to previous plotting location and return
wshow, PrgDat.density_limit_window
wset, old_window
return

endif else begin	; density_plot_window not registered yet

	ctr=PrgDat.ctr & ctg=PrgDat.ctg & ctb=PrgDat.ctb
	tvlct, ctr, ctg, ctb	; use color table from Plasma Equil window

	;if window not yet set up
	wBaseDenLim=widget_base(/column, title='Fraction absorbed(DENSITY)', $
		group_leader=WidIDs.Base, xoffset=200, yoffset=50, $
		event_pro='plot_density_limit_event')

	; graphic widget
	wBaseDrawDenLim=widget_draw(wBaseDenLim, uvalue='DrawDenLim', retain=2, $
		xsize=550, ysize=350)

	; button widgets
	wBaseDenLimButtons=Widget_Base(wBaseDenLim, /row)
	wClose=widget_button(wBaseDenLimButtons, value=' Close ', uvalue='CloseDenLim')
	
	; realize and register
	widget_control, wBaseDenLim, /realize
	
	; save plot window address
	widget_control, wBaseDrawDenLim, get_value=den_plot_window
	PrgDat.density_limit_window=den_plot_window

	xmanager, 'plot_density_limit', wBaseDenLim
	goto, start_denplot

endelse

end
