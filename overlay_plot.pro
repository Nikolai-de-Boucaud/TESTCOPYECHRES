;==============================================================================
FUNCTION overlay_ech_cancel_event,ev
;==============================================================================
; print,'overlay_ech_cancel_event'
; 04-28-98
; Invoked by the Done button in ECH window

; Get info structure from the tlb of ech window.
  Widget_Control,ev.top,Get_UValue=info,/No_Copy
  wPlotBase = (*info.ptr_para).top
  Widget_Control,ev.top,Set_UValue=info,/No_Copy

; Get uvalue structure from the tlb of the equilibrium window.
  Widget_Control,wPlotBase,Get_UValue=uvalue,/No_Copy
  viewer = uvalue.viewer
  Widget_Control,wPlotBase,Set_UValue=uvalue,/No_Copy

; Get state structure from the tlb of the main efit_viewer window.
  Widget_Control,viewer,Get_UValue=state,/No_Copy
  IF struct_hastag(*state.overlay,'echres') THEN (*state.overlay).echres = 0

; Update the ech button on the overlay control.
  wDiagButtons = Widget_Info(state.wOverlay,/Child)
  Widget_Control,wDiagButtons,Get_Value=value,Get_UValue=button_uvalue
  index = where(button_uvalue eq 'echres',count)
  if count ne 0 then value[index]=0
  Widget_Control,wDiagButtons,Set_Value=value
  Widget_Control,viewer,Set_UValue=state,/No_Copy

; Replot the equilibrium.
  wrap_all_plots,{DIAG,top:wPlotBase}

END ; overlay_ech_cancel_event

;==============================================================================
FUNCTION nbiplot_cancel_event,ev
;==============================================================================
; print,'nbiplot_cancel_event'
; 20110107 SMF 
; Invoked by the Done button in NBIPLOT window

; Get info structure from the tlb of ech window.
  Widget_Control,ev.top,Get_UValue=info,/No_Copy
  wPlotBase = (*info.ptr_para).top
  Widget_Control,ev.top,Set_UValue=info,/No_Copy

; Get uvalue structure from the tlb of the equilibrium window.
  Widget_Control,wPlotBase,Get_UValue=uvalue,/No_Copy
  viewer = uvalue.viewer
  Widget_Control,wPlotBase,Set_UValue=uvalue,/No_Copy

; Get state structure from the tlb of the main efit_viewer window.
  Widget_Control,viewer,Get_UValue=state,/No_Copy
  IF struct_hastag(*state.overlay,'nbiplot') THEN (*state.overlay).nbiplot = 0
; Update the nbi button on the overlay control.
  wDiagButtons = Widget_Info(state.wOverlay,/Child)
  Widget_Control,wDiagButtons,Get_Value=value,Get_UValue=button_uvalue
  index = where(button_uvalue eq 'nbiplot',count)
  if count ne 0 then value[index]=0
  Widget_Control,wDiagButtons,Set_Value=value
  Widget_Control,viewer,Set_UValue=state,/No_Copy

; Replot the equilibrium.
  wrap_all_plots,{DIAG,top:wPlotBase}

END ; nbiplos_cancel_event

;==============================================================================
FUNCTION ichres_cancel_event,ev
;==============================================================================
; print,'ichres_cancel_event'
; 20110107 SMF
; Invoked by the Done button in ICHRES window

; Get info structure from the tlb of ech window.
  Widget_Control,ev.top,Get_UValue=info,/No_Copy
  wPlotBase = (*info.ptr_para).top
  Widget_Control,ev.top,Set_UValue=info,/No_Copy

; Get uvalue structure from the tlb of the equilibrium window.
  Widget_Control,wPlotBase,Get_UValue=uvalue,/No_Copy
  viewer = uvalue.viewer
  Widget_Control,wPlotBase,Set_UValue=uvalue,/No_Copy

; Get state structure from the tlb of the main efit_viewer window.
  Widget_Control,viewer,Get_UValue=state,/No_Copy
  IF struct_hastag(*state.overlay,'ichres') THEN (*state.overlay).ichres = 0
; Update the ich button on the overlay control.
  wDiagButtons = Widget_Info(state.wOverlay,/Child)
  Widget_Control,wDiagButtons,Get_Value=value,Get_UValue=button_uvalue
  index = where(button_uvalue eq 'ichres',count)
  if count ne 0 then value[index]=0
  Widget_Control,wDiagButtons,Set_Value=value
  Widget_Control,viewer,Set_UValue=state,/No_Copy

; Replot the equilibrium.
  wrap_all_plots,{DIAG,top:wPlotBase}

END ; ichres_cancel_event

;==============================================================================
PRO overlay_plot,state,WidgetId=widgetid,Group=group
;==============================================================================
; print,'overlay_plot'

; 03-12-98
; information used in state
;	shot
;	time
;	*ptr_a, *ptr_g
;	overlay - also changed
;	plots.(state.nwindow) - window Id of the contour draw widget
;	state.plots.id0 - uvalue and position
; 11-3-98 Q. Peng - Except for special cases such as ech,ich,cer and 
; 	mds_tang that have their own widgets, overlays are called
;	by executing the 'name' with a standard input interface. The
;	names are thoses in the diagnoses_table.pro.
; 01-27-2000 Q.P. - distinguish between error(result.error>0) that prevents 
;	overlay and waring(<0) that indicates something is wrong. But this
;	can be annoying.
; 04-06-2000 Q.P. - adds more overlay options and allows users' defined overlay
; 	routine to be called on the fly.

  shot = state.efit->GetShot()
  time = state.efit->GetTime()
  machine = state.machine
  NameId = *state.overlay
  Widget_Control,state.plots.w0,Get_Value=WinId
  Widget_Control,state.plots.id0,GET_UVALUE=uvalue,/NO_COPY
  systemvar = {!p:uvalue.!p,!x:uvalue.!x,!y:uvalue.!y}
  Widget_Control,state.plots.id0,SET_UVALUE=uvalue,/NO_COPY
  on_error, 2 ; Return to caller

; Check the overlay options
  IF Widget_Info(state.wOverlayOption,/Valid) THEN BEGIN
     Widget_Control,state.wOverlayOption,Get_UValue=optionuval,/No_Copy
     Widget_Control,optionuval.wOptionChoice,Get_Value=choice
     CASE choice OF
	0:BEGIN 					; alternative shot
	  Widget_Control,optionuval.wShot,Get_Value=ishot
	  IF ishot NE 0 THEN shot = ishot
	  END
	2:BEGIN 					; present
	  END
	3:BEGIN 					; future
	  END
     	ELSE: 						; as-run
     End ; case
     Widget_Control,optionuval.wOtherChoice,Get_Value=otheroverlay
     Widget_Control,optionuval.wOtherPath,Get_Value=otherpath
     Widget_Control,optionuval.wOther,Get_Value=othersource
     otheroverlay = otheroverlay[0] 
     otherpath = otherpath[0] & othersource = othersource[0]
     Widget_Control,state.wOverlayOption,Set_UValue=optionuval,/No_Copy
  ENDIF ELSE BEGIN
     otheroverlay = 0
     othersource = ''
  END

; check keywords
  if not Keyword_Set(group) then group = 0L

; check if need to send special events to sub-widgets (mds_tang)
  IF Widget_Info(WidgetId.wMDS,/Valid) THEN $
	Widget_Control,WidgetId.wMDS,Event_Func='mds_switch_event',$
	Send_Event={id:WidgetId.wMDS,top:WidgetId.wMDS,handler:0L,shot:shot},/No_Copy

; Do not use graphics func that employs XOR. It generates unpredicatable color.
;
; set graphics function to equivelent mode, in overlay routines, use OPLOT
; restore system graphic variables from the contour plot window
;  if !D.Name eq 'X' then $
;  DEVICE,GET_GRAPHICS_FUNC=oldg,SET_GRAPHICS_FUNC=6
  oldP = !p
  oldX = !x
  oldY = !y
  if Keyword_Set(systemvar) then begin
  !p = systemvar.!p
  !x = systemvar.!x
  !y = systemvar.!y  
  endif

; Find the type of NameId
  s = SIZE(nameid)
  type = s(N_Elements(s)-2)
  if type eq 7 then names = nameid $			; string (array)
  else if type eq 8 then begin				; struct
     tagnames = Tag_Names(nameid)
     ntags = N_Elements(tagnames)
     value = intarr(ntags)
     for i=0,ntags-1 do value(i)=nameid.(i)
     itags = where(value ne 0, count)
     if count eq 0 then goto,done
     names = strlowcase(tagnames(itags))
  endif

Forward_Function coils,cerpaths,cerpaths_main,echres,ichres,mds_tang,nbiplot,ichres,eceoverlap,scale_iter

  diagnoses_table,machine,name=namedefined,color=colordefined
  for i=0, N_Elements(names)-1 do begin
     name = names(i)
     index = Where(namedefined EQ name)
     IF index[0] NE -1 THEN color=colordefined[index[0]] $
		       ELSE color='Foreground'
     CASE name OF
    	'scale_iter':begin
		if Widget_Info(group,/Valid) then $
                   WidgetId.wScaleIter = scale_iter(shot,astruct=state.efit->GetA(/New),wScaleIter=WidgetId.wScaleIter,$
                                           mstruct=state.efit->GetM(/New),color=color_index('Red'),$
                                           callback='wrap_all_plots',parameter={DIAG,top:group},$
                                           Group_Leader=state.wOverlay,/Floating,Tlb_Frame_Attr=2)
                end
	'coils':begin
		if Widget_Info(group,/Valid) then $
                   WidgetId.wCoils = coils(shot,astruct=state.efit->GetA(/New),wCoils=WidgetId.wCoils,$
				           mstruct=state.efit->GetM(/New),color=color_index('Red'),$
                         	           callback='wrap_all_plots',parameter={DIAG,top:group},$
                         	           Group_Leader=state.wOverlay,/Floating,Tlb_Frame_Attr=2)
                end
	'eceoverlap':begin
                if Widget_Info(group,/Valid) then $
                   WidgetId.wEceOverlap = eceoverlap(shot,time=time,wEceOverlap=WidgetId.wEceOverlap,$
                                                     astruct=state.efit->GetA(/New),gstruct=state.efit->GetG(/New),$
						     color=color_index('Red'),callback='wrap_all_plots',$
						     parameter={DIAG,top:group},Group_Leader=state.wOverlay,$
						     /Floating,Tlb_Frame_Attr=2)
                end
	'cerpaths':begin					; CER chords
		if Widget_Info(group,/Valid) then $
		   WidgetId.wCer = cerpaths(shot,color=color_index('Red'),$
			callback='wrap_all_plots',parameter={DIAG,top:group},$
			Group_Leader=state.wOverlay,/Floating,Tlb_Frame_Attr=2)
		end
        'cerpaths_main':begin                                   ; CERMain chords
                if Widget_Info(group,/Valid) then $
                   WidgetId.wCerM = cerpaths_main(shot,color=color_index('Red'),$
                        callback='wrap_all_plots',parameter={DIAG,top:group},$
                        Group_Leader=state.wOverlay,/Floating,Tlb_Frame_Attr=2)
                end
	'echres':begin					; ECH-resonance-255
		if Widget_Info(group,/Valid) then $
		   WidgetId.wEch = echres($
			state.efit->GetA(/New),state.efit->GetG(/New),$
			bid=widgetId.wEch,color=[color_index('Cyan'),$
			color_index('Magenta'),color_index('Red')],$
			callback='wrap_all_plots',/verbose,$
			parameter={DIAG,top:group},$
			cancel_event='overlay_ech_cancel_event',$
			Group_Leader=state.wOverlay,/Float,Tlb_Frame_Attr=2)
		end
        'ichres':begin                                  ; ICH-resonance
                if Widget_Info(group,/Valid) then $
                   WidgetId.wIch = ichres($
                        state.efit->GetA(/New),state.efit->GetG(/New),$
                        bid=widgetId.wEch,color=[color_index('Cyan'),$
                        color_index('Magenta'),color_index('Red')],$
                        callback='wrap_all_plots',/verbose,$
                        parameter={DIAG,top:group},$
                        cancel_event='overlay_ech_cancel_event',$
                        Group_Leader=state.wOverlay,/Float,Tlb_Frame_Attr=2)
                end
         'nbiplot':begin
     		if Widget_Info(group,/Valid) then $
		   WidgetId.wNbi = nbiplot($
                        state.efit->GetA(/New),state.efit->GetG(/New),$
                        bid=widgetId.wNbi,color=[color_index('Cyan'),$
 		        color_index('Magenta'),color_index('Red')],$
                        callback='wrap_all_plots',/verbose,$
                        parameter={DIAG,top:group},$
                        cancel_event='nbiplot_cancel_event',$
                        Group_Leader=state.wOverlay,/Float,Tlb_Frame_Attr=2)
   		end
	 ELSE:  BEGIN
	     IF StrUpCase(StrMid(name,0,4)) EQ 'MDS_' THEN BEGIN
		; MDS tangential
		index = where(names eq 'mds_tang',count3)
		if count3 gt 0 and strlen(name) eq 6 then begin	; selected
		   if (where(names eq 'mds_old'))[0] ge 0 then old=1 else old=0
		   result = mds_tang(shot,long(strmid(name,4,2)),$
				color=color_index('Red'),old=old)
		endif
	     ENDIF ELSE BEGIN		; everthing else
		result=call_function(name,shot,time=time,$
 			color=color_index(color),$
 			gstruct=state.efit->GetG(/New),$
 			astruct=state.efit->GetA(/New),$
 			mstruct=state.efit->GetM(/New),$
 			ParentId=state.plots.id0,$
 			Annotate=state.overlayannotate)
		if result.error gt 0 then begin
		 ; put on error message
		   ok=Dialog_Message(result.msg,Dialog_Parent=state.plots.id0)
		 ; pop up the button
		   wButtons = Widget_Info(state.wOverlay,/Child)
		   Widget_Control,wButtons,Get_UValue=button_uvalue,$
			Get_Value=selected,/No_Copy
		   index = where(button_uvalue eq name)
		   selected[index] = 0
		   Widget_Control,wButtons,Set_UValue=button_uvalue,$
			Set_Value=selected,/No_Copy
		 ; reset switch in state.overlay
		   index = where(strlowcase(Tag_Names(*state.overlay)) eq name)
		   (*state.overlay).(index(0)) = 0
		endif else if result.error lt 0 then begin
		 ; put on a warning message without popup the button
		   ok=Dialog_Message(result.msg,Dialog_Parent=state.plots.id0)
		end
	      END ; else
	    END
     END ; case

  endfor ; 

done:
; If otheroverlay is on, put on user's own overlay with the given routine
; turn on error handling, report and skip any error here.

  IF getenv_('DEBUG') eq '' THEN Catch, error_status else error_status=0
  IF error_status NE 0 THEN BEGIN
     Catch,/Cancel
     if WIDGET_INFO(state.wOverlayOption,/VALID) then $
	Widget_Control,state.wOverlayOption,Map=1
     ok=Dialog_Message("User's overlay - "+!Err_String,$
	Dialog_Parent=state.plots.id0)
     goto, skip
  endif

  IF otheroverlay EQ 1 AND othersource NE '' THEN BEGIN
     IF (StrPos(!path,otherpath+':') EQ -1) THEN !path=otherpath+":"+!path
     ; parse the routine name
     routinename = (Str_Sep(othersource,','))[0]
     routinename = Str_Sep(routinename,'=')
     IF N_Elements(routinename) GT 1 THEN BEGIN 	; function
     	routinename = routinename[1]
     	routinename = (Str_Sep(routinename,"("))[0]
     	Resolve_Routine,routinename,/Is_Function
     ENDIF ELSE Resolve_Routine,routinename
     okay = Execute(othersource)
  ENDIF

  Catch,/Cancel

skip:
; Hide or map sub-windows including CER, ECH, ICH, NBI and MDS tangential
  if count ne 0 then begin
     index = where(names eq 'echres',count1)
     index = where(names eq 'ichres',count2)
     index = where(names eq 'mds_tang',count3)
     index = where(names eq 'cerpaths',count4)
     index = where(names eq 'nbiplot',count5)
     index = where(names eq 'coils',count7)
     index = where(names eq 'eceoverlap',count8)
     index = where(names eq 'cerpaths_main',count9)
     index = where(names eq 'scale_iter',count10)
  endif else begin
     count1 = 0 
     count2 = 0 
     count3 = 0
     count4 = 0
     count5 = 0
     count6 = 0
     count7 = 0
     count8 = 0
     count9 = 0 
     count10 = 0
  endelse
; Map only when overlay control window is on.
  selected = state.OverlayControl
  if Widget_Info(widgetId.wEch,/VALID) then $
     Widget_Control,widgetId.wEch,MAP=(count1 and selected)
  if Widget_Info(widgetId.wIch,/VALID) then $
     Widget_Control,widgetId.wIch,MAP=(count2 and selected)
  if Widget_Info(widgetId.wMDS,/VALID) then $
     Widget_Control,widgetId.wMDS,MAP=(count3 and selected)
  if Widget_Info(widgetId.wCER,/VALID) then $
     Widget_Control,widgetId.wCER,MAP=(count4 and selected)
  if Widget_Info(widgetId.wNbi,/VALID) then $
     Widget_Control,widgetId.wNbi,MAP=(count5 and selected)
  if Widget_Info(widgetId.wCoils,/VALID) then $
     Widget_Control,widgetId.wCoils,MAP=(count7 and selected)
  if Widget_Info(widgetId.wEceOverlap,/VALID) then $
     Widget_Control,widgetId.wEceOverlap,MAP=(count8 and selected)
  if Widget_Info(widgetId.wCERM,/VALID) then $
     Widget_Control,widgetId.wCERM,MAP=(count9 and selected)
  if Widget_Info(widgetId.wScaleIter,/VALID) then $
     Widget_Control,widgetId.wScaleIter,MAP=(count10 and selected)

; restore graphics func and graphic variables
;  if !D.Name eq 'X' then $
;  DEVICE,SET_GRAPHICS_FUNC=oldg
  !p = oldP
  !x = oldX
  !y = oldY

END ; overlay_plot
