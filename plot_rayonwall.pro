;plot the rays on the wall
;call subroutine plot_d3d_wall.pro to plot ports

PRO plot_rayonwall_event, ev
Common plot_rayonwall_share,wSysWall,redat,gyroind

catch, an_error
if an_error ne 0 then begin
	print, '  !!! plot_rayonwall_event: error: ', !err_string
	catch, /cancel
	return
endif

widget_control, ev.id, get_uvalue=uval
case uval of

	'CloseRayonWall': begin
		widget_control, ev.top, /destroy
		return
	end

	'plotgyro': begin
		widget_control, wSysWall, get_value=sw
		plot_rayonwall,redat,gyroind,selectsys=sw
	end

else: 
	
endcase
END

;;;;;;;;;;
PRO plot_rayonwall,rayend_data,plgyroi,selectsys=selectsys
Common ECHCOM
Common WIDGETIDS, WidIDs
Common plot_rayonwall_share,wSysWall,redat,gyroind

catch, an_error
if an_error ne 0 then begin
	print, '  !!! plot_rayonwall: ', !err_string
	return
endif

nsys=n_elements(plgyroi)
plSysWall=intarr(nsys)+1

if NOT keyword_set(selectsys) then selectsys=plSysWall

if xregistered('plot_rayonwall') then begin

start_rayonwallplot:

old_window=!D.WINDOW
wset, PrgDat.rayonwall_window
sind = where (selectsys NE 0, ngyro)
re=rayend_data[sind]
igyro=plgyroi[sind]

;define parameters for EC power calculation / plotting
phrange=[185., 325.] ;D3D LHCS in degree
thrange=[-90., 120.]  ;D3D LHCS in degree

plotit: 	

	loadct,39
	nlevel=33
	!p.color=0
	!p.background=16777215	

	IF ngyro GT 0 THEN BEGIN	
	IF ngyro GT 1 THEN hf_all=total(re.rf_bin,3) ELSE $
		hf_all=re.rf_bin	
	
	zrange = [0, max(hf_all)*1.001]		; contour range 
	levels = findgen(nlevel)*(zrange[1]-zrange[0])/float(nlevel)+zrange[0]
	colors = (findgen(nlevel)+1)*(254)/(float(nlevel))
			
	contour,hf_all,re[0].phhis,re[0].thhis,POSITION=[0.1,0.15,0.75,0.9], $
	    LEVELS=levels,C_COLORS=colors, /FILL, $
        XRANGE=phrange,XSTYLE=1,XTICKLEN=-0.02,XTITLE='Toroidal Angle (Deg)',    $                       
        YRANGE=thrange,YSTYLE=1,YTICKLEN=-0.02,YTITLE='Pololoidal Angle (Deg)',  $
        ZRANGE=zrange, ZSTYLE=1,charsize=0.8,title=PrgDat.Source

	for i=0, ngyro-1 do begin
	    nonzeroind=where(re[i].rf_bin GT 0.,nonzeron)
		avev=total(re[i].rf_bin[nonzeroind])/nonzeron/1E3 ;change from W to kW
		maxv=max(re[i].rf_bin, tmpi)/1E3   ;change from W to kW
		maxi=array_indices(re[i].rf_bin,tmpi)
		gyronm=strmid(strupcase(re[i].gyroname),0,3)		
		xyouts,re[i].phhis(maxi[0])+3.*re[i].portLR, $
			re[i].thhis(maxi[1])+3.*re[i].portLR, $
			gyronm,col=255;,charsize=0.7
	endfor  ;;;loop of gyro systems (i loop)   
	ENDIF

	IF ngyro EQ 0 THEN BEGIN
	plot,findgen(10),findgen(10),/nodata,POSITION=[0.1,0.15,0.75,0.9], $
        XRANGE=phrange,XSTYLE=1,XTICKLEN=-0.02,XTITLE='Toroidal Angle (Deg)',    $                       
        YRANGE=thrange,YSTYLE=1,YTICKLEN=-0.02,YTITLE='Pololoidal Angle (Deg)'
	ENDIF
	plot_d3d_wall
	;;;different colors for different EC sensitive windows
	col=255    ;or set to -1 for white
	col0=145  ;green - low risk
	col1=195  ;yellow - medium risk, or 200 orange color
	col2=254  ;red - high risk
	xyouts, 0.77, 0.85, 'ECH launcher port', col=col2,/normal
	xyouts, 0.77, 0.75, 'Note 90deg R+2 top', /normal
	xyouts, 0.77, 0.72, 'launch port not shown',/normal


; done with plotting, return to previous plotting location and return
	;;;in order to be consistent with plot_toray.pro
	!p.color=1
	!p.background=0

wshow, PrgDat.rayonwall_window
wset, old_window
	;;;in order to not change the Plasma Equil window background color
	ctr=PrgDat.ctr & ctg=PrgDat.ctg & ctb=PrgDat.ctb
	tvlct, ctr, ctg, ctb	; use color table from Plasma Equil window

return

endif else begin	; rayonwall_window not registered yet

	ctr=PrgDat.ctr & ctg=PrgDat.ctg & ctb=PrgDat.ctb
	tvlct, ctr, ctg, ctb	; use color table from Plasma Equil window

	;if window not yet set up
	wBaseRayonWall=widget_base(/column, title='Unabsorbed EC Power on the Wall', $
		group_leader=WidIDs.Base, xoffset=140, yoffset=400, $
		event_pro='plot_rayonwall_event')

	; graphic widget
	wBaseDrawRayonWall=widget_draw(wBaseRayonWall, uvalue='DrawRayonWall', retain=2, $
		xsize=850, ysize=500)

	; button widgets
	wBaseRayonWallButtons=Widget_Base(wBaseRayonWall, /row)
	wClose=widget_button(wBaseRayonWallButtons, value=' Close ', uvalue='CloseRayonWall')

	redat=rayend_data
	gyroind=plgyroi

	SysWallLab=strmid(strupcase(redat.gyroname),0,3)
	wSysWall=cw_bgroup(wBaseRayonWallButtons, SysWallLab, /nonexclusive, /row, $
	label_left='from systems: ', button_uvalue=igyro, $
	uvalue='plotgyro', set_value=plSysWall)
	
	; realize and register
	widget_control, wBaseRayonWall, /realize
	
	; save plot window address
	widget_control, wBaseDrawRayonWall, get_value=rayonwall_plot_window
	PrgDat.rayonwall_window=rayonwall_plot_window

	xmanager, 'plot_rayonwall', wBaseRayonWall
	goto, start_rayonwallplot

endelse
    
END    
