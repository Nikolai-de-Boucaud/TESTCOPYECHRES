pro plot_survey_event, ev
Common ECHCOM
Common AIMER
Common plot_survey_widids, ps_widids, e

widget_control, ev.id, get_uvalue=uval
	
case uval of
	'closePS': begin
		widget_control, ev.top, /destroy
		return
	end
	'printit': begin
		if strtrim(PrgDat.printer_name,2) eq '' then s=dialog_message('Set printer name in window first.')
		PrgDat.survey_print_file='survey_ech.ps'
		spawn, 'lp -d' + PrgDat.printer_name + ' ' + PrgDat.survey_print_file
		print, 'Printed file ' + PrgDat.survey_print_file + ' to printer ' + PrgDat.printer_name
	end
	'setptr': begin
		prntr=''
		widget_control, ev.id, get_value=prntr
		PrgDat.printer_name=prntr
		print, 'Changed default printer to ' + PrgDat.printer_name
	end
	'plottype': begin
		if ev.select then begin
			case ev.value of
				0: plot_type='ieccd'	; plot I_ECCD
				1: plot_type='jeccd'	; plot j_ECCD
				2: plot_type='qe'	; plot q_e
			endcase
			plot_survey, e, plot_type
		endif
	end
	'getcursorpoint': begin
		if ev.release then begin
			xpt=((ev.x-100.)/570.)*(!x.crange[1]-!x.crange[0])+!x.crange[0]
			ypt=((ev.y-60.)/410.)*(!y.crange[1]-!y.crange[0])+!y.crange[0]
			widget_control, ps_widids.AziAng, set_value=string(xpt, format='(f7.2)')
			widget_control, ps_widids.PolAng, set_value=string(ypt, format='(f7.2)')
		endif
	end
endcase
end

pro plot_survey, edat, plot_type
; e is echdat[npol, nazi, nbt] from survey_steering_angles
; obj is the objective: 0=no ECCD, 1=max ECCD, 2=max j_ECCD
; returns structure containing best fit angles
; plot_type = '' | 'jeccd' | 'ieccd' | 'qe'
Common ECHCOM
Common WIDGETIDS, WidIDs
Common AIMER
Common plot_survey_widids, ps_widids, e
e=edat
shottime=string(PrgDat.shot, format='(i6.6)')+'.'+string(PrgDat.time, format='(i5.5)')

if xregistered('plot_survey') then begin
start_plot:
old_window=!D.WINDOW
wset, PrgDat.survey_window
ctr=PrgDat.ctr & ctg=PrgDat.ctg & ctb=PrgDat.ctb
tvlct, ctr, ctg, ctb	; use color table from Plasma Equil window
blue=4 & red=2 & green=3 & black=1
;****************************************************

; plot ranges and parameters
xr=[e.azi[0], e.azi[e.nazi-1]]
yr=[e.pol[0], e.pol[e.npol-1]]
xtit='azimuthal (deg)'
ytit='polar (deg)'
titdrho='drho (Gaussian FW 1/e)'
contk=2.5	; thickness of contour lines
!p.multi=[0,1,1]

; update the printer name
widget_control, ps_widids.Printer, set_value=PrgDat.printer_name

; arrays to hold data
nuldat=fltarr(e.nazi,e.npol)
currho=nuldat
powrho=nuldat
Icd=nuldat
drho=nuldat
jeccd=nuldat
powden=nuldat
powfrac=nuldat
powabs=nuldat

for iazi=0, e.nazi-1 do begin 
	for ipol=0 ,e.npol-1 do begin
		if ((e.echdat[iazi,ipol].frac_abs GT 0.04) and $
		e.echdat[iazi,ipol].success) then begin
			currho[iazi,ipol]=e.echdat[iazi,ipol].rho_j0
			powrho[iazi,ipol]=e.echdat[iazi,ipol].rho_qe0
			drho[iazi,ipol]=e.echdat[iazi,ipol].drho_j0
			Icd[iazi,ipol]=e.echdat[iazi,ipol].ECCD/1000.	; kA/MW
			jeccd[iazi,ipol]=e.echdat[iazi,ipol].j0			; A/cm2/MW
			powden[iazi,ipol]=e.echdat[iazi,ipol].qe0		; W/cm3/MW
			powabs[iazi,ipol]=e.echdat[iazi,ipol].frac_abs
				
				; get power remaining at nharm location
				;if abs(r[ipol,iazi].driven_current) gt 0.5e3 then $
				;	powfrac[ipol,iazi]=r[ipol,iazi].frac_pow_j else powfrac[ipol,iazi]=1.0
				; get median of current and power profiles for cases
				; where gaussfit doesn't work well
				;goto, skip_nongaussian
				;intpow=echdat[iazi,ipol,ibt].integrated_power	; W
				;intpow=1.e6
				;dpow=(intpow[199]-intpow[0])	; W
				;kk0=findex(intpow, intpow[199]-0.5*dpow)
				;kk1=findex(intpow, intpow[199]-0.1353*dpow)
				;kk2=findex(intpow, intpow[199]-0.8647*dpow)
				;powrho[iazi,ipol]=interpolate(echdat[iazi,ipol,ibt].rho,kk0)
				;currho[iazi,ipol]=powrho[iazi,ipol] ; approximately
				;drho[iazi,ipol]=(interpolate(echdat[iazi,ipol,ibt].rho,kk1) - $
				;	interpolate(echdat[iazi,ipol,ibt].rho,kk2))*1.3
				;frhoind=findex(f.rhonorm, currho[iazi,ipol])
				;dvol=interpolate(f.dvol,frhoind)*drho[iazi,ipol]
				;dA=dvol/(2.*!pi*g.rmaxis) ; m2
				;jeccd[iazi,ipol]=echdat[iazi,ipol,ibt].driven_current/(dA*1.e4)*2.0	; A/cm2
				;powden[iazi,ipol]=2.*dpow/1.e6/dvol 			; W/cm3=MW/m3
				;titdrho='drho (Full width 1/e^2 power)'
				;skip_nongaussian:
		endif
	endfor
endfor

; get rid of any NaNs
;ij=where(~finite(jeccd), count)
;if count gt 0 then jeccd[ij]=0.0
;ij=where(~finite(powrho), count)
;if count gt 0 then powrho[ij]=0.0
;ij=where(~finite(Icd), count)
;if count gt 0 then Icd[ij]=0.0
;ij=where(~finite(powden), count)
;if count gt 0 then powden[ij]=0.0

subtit=string(e.freq, format='(f6.2)') + ' GHz; ' + $
	string(e.bt, format='(f6.3)') + ' T;' + $
	string(PrgDat.profiles.centraldensity*1.e6, format='(e11.4)') + ' m-3;' + $
	string(PrgDat.profiles.centralte, format='(f6.3)') + ' keV'
	
if plot_type ne '' then begin
	case plot_type of
		'jeccd': plot=2
		'ieccd': plot=1
		'qe': plot=0
		else: plot=0
	endcase 
endif else begin
	case e.obj of 
		0: plot=1 ; plot I_ECCD
		1: plot=1 ; plot I_ECCD
		2: plot=2 ; plot j
		else: plot=1
	endcase
endelse
if plot eq 0 then widget_control, ps_widids.PlotType, set_value=2 else $
	widget_control, ps_widids.PlotType, set_value=plot-1

; set plot parameters
case plot of
	0:begin
		tit='q_e (W/cm3/MW) '+shottime
		levs0=get_range(0., max(powden), 20)
		levs1=round(levs0)
		il=uniq(levs1)
		levs=float(levs1[il])
		if n_elements(levs) lt 5 then begin
			levs0 *= 10.
			levs1=round(levs0)
			il=uniq(levs1)
			levs=float(levs1[il])/10.
		endif
	end
	1: begin
		tit='I_ECCD (kA/MW) '+shottime
		ij=where(Icd ne 0., count)
		if count gt 2 then begin
			levs0=get_range(min(Icd[ij]),max(Icd[ij]), 30) 
			levs1=round(levs0)
			il=uniq(levs1)
			levs=float(levs1[il])
			if n_elements(levs) lt 5 then begin
				levs0 *= 10.
				levs1=round(levs0)
				il=uniq(levs1)
				levs=float(levs1[il])/10.
			endif
		endif else levs=[0.,1.]

	end
	2: begin
		tit='Peak j_ECCD (A/cm2/MW) '+shottime
		ij=where(jeccd ne 0., count)
		if count gt 2 then begin
			levs0=get_range(min(jeccd[ij]),max(jeccd[ij]), 30)
			levs1=round(levs0)
			il=uniq(levs1)
			levs=float(levs1[il])
			if n_elements(levs) lt 5 then begin
				levs0 *= 1000.
				levs1=round(levs0)
				il=uniq(levs1)
				levs=float(levs1[il])/1000.
			endif
		endif else levs=[0.,1.]
	end
	else:
endcase

ibad=where(powrho eq 0.0, count)
if count gt 0 then begin
	currho[ibad]=-1.e30
	powrho[ibad]=-1.e30
	Icd[ibad]=-1.e30
	jeccd[ibad]=-1.e30
	powden[ibad]=-1.e30
endif

dest=0
filename=PrgDat.gdir+'survey_ech.ps'
plotit:

; contour rho of power and rho_target
contour, nuldat, e.azi, e.pol, levels=1.0, xtitle=xtit, ytitle=ytit, $
	title=tit, /follow, min_value=0.0, /data, subtitle=subtit, $
	thick=contk, xr=xr, yr=yr, xsty=1, ysty=1, color=black
contour, powrho, e.azi, e.pol, levels=get_range(0.0,0.95,20), /follow, $	
	/overplot, color=green, c_charsize=1., min_val=-1.e28
contour, powrho, e.azi, e.pol, levels=[e.rho_target], /follow, $
	/overplot, color=blue, c_charsize=1., min_val=-1.e28
	
case plot of
	0: contour, powden, e.azi, e.pol, levels=levs, /follow, color=red, $
		/overplot, min_value=-1.e28
	1: begin
		contour, Icd, e.azi, e.pol, levels=levs, /follow, color=red, $
			/overplot, c_charsize=1.0, min_value=-1.e28
		contour, Icd, e.azi, e.pol, levels=[0], /follow, color=green, $
			/overplot, c_charsize=1.0, min_value=-1.e28
	end
	2: contour, jeccd, e.azi, e.pol, levels=levs, /follow, color=red, $
		/overplot, c_charsize=1.0, min_val=-1.e28
endcase

if dest eq 0 then begin
	dest=1
	set_plot, 'ps'
	device, /landscape, /color, filename=filename
	!p.thick=3.0
	!p.charthick=2.25
	!x.thick=2.0
	!y.thick=2.0
	goto, plotit
endif else begin
	dest=0
	device, /close
	print, '  *** Wrote ps file '+filename
	set_plot, 'x'
	!p.thick=1.250
	!p.charthick=1.250
	!x.thick=1.25
	!y.thick=1.25
endelse

set_plot, 'x'
wset, old_window

return

endif	; end plotting part from start_plot

; starts here if not xregistered
wBasePS=widget_base(/column, title='TORAY Survey Plot Window', group_leader=WidIDs.Base)

; graphic widget
wBaseDrawPS=widget_draw(wBasePS, xsize=700, ysize=500, retain=2, $
	/button_events, uvalue='getcursorpoint')

; button widgets
wBaseButtonsPS=Widget_Base(wBasePS, /column)

wBaseCursorValues=Widget_Base(wBaseButtonsPS, /row)
wXpoint=cw_field(wBaseCursorValues, title='Azimuthal angle:', xsize=8, value='0.0', $
	uvalue='xpoint', /string)
wYpoint=cw_field(wBaseCursorValues, title='Polar angle:', xsize=8, value='0.0', $
	uvalue='ypoint', /string)

wBaseActions=Widget_Base(wBasePS, /row)
wPlotType=cw_bgroup(wBaseActions, /exclusive, ['I_ECCD', 'j_ECCD', 'q_e'], $
	/row, label_left='Plot:', button_uvalue=[0,1,2], $
	uvalue='plottype', set_value=0)

wPrinter=cw_field(wBaseActions, title='  Printer:', xsize=12, value=PrgDat.printer_name, $
	uvalue='setptr', /return_events, /string)
	
wPrint=widget_button(wBaseActions, value=' Print ', uvalue='printit')


wClosePS=widget_button(wBaseActions, value=' Close ', uvalue='closePS')

widget_control, wBasePS, /realize
widget_control, wBaseDrawPS, get_value=survey_window
PrgDat.survey_window=survey_window
ps_widids={AziAng:wXpoint, PolAng:wYpoint, PlotType:wPlotType, Printer:wPrinter}
xmanager, 'plot_survey', wBasePS
goto, start_plot
end
