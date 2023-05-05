pro plot_cql_echres_event, ev
Common ECHCOM
Common cql_wids, cqlwids, badjustment, cqldraw

widget_control, ev.id, get_uvalue=uval

case uval of

'CQLSurface': begin
	i=where(PrgDat.cql_rya gt 0., nsurfs)
	isurf=0
	widget_control, ev.id, get_value=isurf
	if isurf lt 0 then isurf=0
	if isurf gt nsurfs-1 then isurf=nsurfs-1
	PrgDat.cql_surf=isurf
	surfrho=PrgDat.cql_rya[isurf]
	widget_control, cqlwids.wcqlsurf, set_value=isurf
	widget_control, cqlwids.wcqlrho, set_value=surfrho
end

'CQLrho': begin
	widget_control, ev.id, get_value=surftarget
	i=where(PrgDat.cql_rya gt 0., nsurfs)
	if surftarget gt 1. then surftarget=1.
	if surftarget lt 0. then surftarget=0.
	i=nearest_index(PrgDat.cql_rya[0:nsurfs-1], surftarget)
	PrgDat.cql_surf=i[0]
	widget_control, cqlwids.wcqlrho, set_value=PrgDat.cql_rya[i]
	widget_control, cqlwids.wcqlsurf, set_value=i[0]
end

'pmode': begin
	icqlmode=0	; 0=flux plot, 1=dist fun, 2=delta dist fn, 3=rf flux, 4=rf flux contours
				; 5=tot flux, 6=tot flux contours
	widget_control, ev.id
	PrgDat.cql_mode=ev.index
end

'badjust': begin	; small adjustment for toroidal field
	dummy=1.0
	widget_control, ev.id, get_value=dummy
	PrgDat.cql_b_fudge_factor=dummy
end

'plotres': begin
	dummy=1
	widget_control, ev.id, get_value=dummy
	PrgDat.cql_plotresonance=dummy
end

'printcql': begin
	PrgDat.cql_print=1
end

'quadrant': begin
	quadrant=0
	widget_control, ev.id, get_value=quadrant
	PrgDat.cql_quadrant=quadrant
end

'CQLsys':begin
	sys=1
	widget_control, ev.id, get_value=sys
	if sys lt 1 then sys=1
	if sys gt 8 then sys=8
	if ECHSys.Tank[sys-1].Power_MW lt 0.001 then begin
		res=dialog_message('System '+strtrim(sys,2)+' has no power', /info)
		return
	endif	
	PrgDat.cql_sys=sys-1
	gyroon=where(PrgDat.status.plotrays gt 0)
	widget_control, cqlwids.wGyroCQL, set_value=ECHSYS.tank[gyroon[PrgDat.cql_sys]].gyroname
end

'cqlstop': stop

endcase

plot_flux_echres, PrgDat.cql_mode, PrgDat.cql_window, PrgDat.cql_plotresonance, PrgDat.cql_sys

end


pro plot_cql_echres, cqdir, mne

; routine called in echres when cql3d has completed

cd, cqdir, current=oldir

Common ECHCOM
Common WIDGETIDS, WidIDs
Common cql_wids, cqlwids, badjustment, cqldraw

if xregistered('plot_cql_echres') then begin	; window exists
	start_cql_plot:
	plot_flux_echres, PrgDat.cql_mode, PrgDat.cql_window, PrgDat.cql_plotresonance
	cd, oldir
	return
endif

; if window not set up yet
wBasePCQL=widget_base(/column, title='CQL3D Plots', group_leader=WidIDs.Base, $
	xoffset=300, yoffset=0)
PrgDat.cql_windowID=wBasePCQL	; for killing cql window when toray runs
	
; graphic widget
wBaseDrawPCQL=widget_draw(wBasePCQL, uvalue='DrawPC', retain=2, $
	xsize=900, ysize=750)
	
; specify surface and rho
modeselect=['FLUX MAGNITUDE','DISTRIBUTION FN ', 'DELTA DIST FN', 'RF FLUX', $
	'RF FLUX CONTOURS', 'TOTAL FLUX ', 'TOTAL FLUX CONTOURS']
wBaseSurfPCQL=Widget_Base(wBasePCQL, /row)
wMode=widget_combobox(wBaseSurfPCQL, value=modeselect, uvalue='pmode')

PrgDat.cql_surf=10	; estimate starting surface
wSurface=cw_field(wBaseSurfPCQL, /integer, /return_events, /row, $
	title='  Surface:', value=PrgDat.cql_surf, uvalue='CQLSurface', xsize=3)
wCQLrho=cw_field(wBaseSurfPCQL, /floating, /row, /return_events, $
	title='  rho:', value=PrgDat.cql_rya[PrgDat.cql_surf], uvalue='CQLrho', xsize=8)
wCQLSys=cw_field(wBaseSurfPCQL, /integer, /row, /return_events, $
	title='  System:', value=PrgDat.cql_sys+1, uvalue='CQLsys', xsize=2) 
gyroon=where(PrgDat.status.plotrays gt 0)
wGyroCQL=cw_field(wBaseSurfPCQL, title='   Gyro:', /noedit, /string, xsize=8, $
	value=ECHSYS.tank[gyroon[PrgDat.cql_sys]].gyroname)

; adjust B
wBAd=widget_base(wBasePCQL, /row)
wResonance=cw_bgroup(wBAd, /exclusive, ['No', 'Yes  '], /row, $
	label_left='Overplot resonance:', button_uvalue=[0,1], $
	set_value=1, uvalue='plotres')
wRevWpar=cw_bgroup(wBAd, /exclusive, ['Both', 'Left', 'Right'], /row, $
	label_left='Quadrant:',	button_uvalue=[0,1,2], $
	set_value=0, uvalue='quadrant')
wBAdjust=cw_fslider(wBAd, maximum=1.05, minimum=0.95, /frame,/edit, $
	title='B adjustment factor:', uvalue='badjust', value=1.0, xsize=10)
wPrintCql=widget_button(wBAd, value='  PS file   ', uvalue='printcql')
usr=strlowcase(getenv('USER'))
if usr eq 'prater' or usr eq 'chenxi' then wStop=widget_button(wBAd, value='Stop', $
	uvalue='cqlstop')

widget_control, wBasePCQL, /realize
widget_control, wBaseDrawPCQL, get_value=cqldraw
PrgDat.cql_window=cqldraw
widget_control, wMode, set_combobox_select=0

cqlwids={wcqldraw:wBaseDrawPCQL, wcqlsurf:wSurface, wcqlrho:wCQLrho, wcqlmode:wMode, $
	wGyroCQL:wGyroCQL}

xmanager, 'plot_cql_echres', wBasePCQL

goto, start_cql_plot

end
