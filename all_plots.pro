;************************************************************************
;+
; NAME:
;	ALL_PLOTS
;
; PURPOSE:
;	Put on the specified or all plots.  Called by EFIT_VIEWER
;	once some selection is made at the main menu that should
;	take effect in all plot pages.
;
; CATEGORY:
;
; CALLING SEQUENCE:
;	ALL_PLOTS,top,state
;
; INPUTS:
;	top	A widget Id that might be used as the group leader
;		in overlay_plot, or the top in the psudo event
;		sent to the efit_viewer_plot_event.  Should be 
;		set to the ev.top when the called is made.
;	state	The information structure carried by the uvalue
;		of the efit viewer wId.
;
; KEYWORD PARAMETERS:
;	plot	The plot number starting 0.  Default is all plots
;		that have been opened.
;		0 - equilibrium
;		1 - fitting quality
;		2 - profiles 1
;		3 - profiles 2
;		4 - pointnames
;
; OUTPUTS:
;	None.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;	Use the specified plot number of all of the five pages. 
;	Loop over all pages.  First check if the page is toggeled on
;	(mapped).  If it is, update the plot with the new state.
;	Otherwise, skip this page.
;
; EXAMPLE:
;	all_plots,ev.top,state,plot=0
;
; HISTORY:
;	03-15-98 Q. Peng firest created
;	05-19-98 Q. Peng replace getenv with CMG's getenv_ for MacOS.
;	11-24-99 Q. Peng make it work with Mac or Window in addition to X
;       07-30-12 SMF     Let iefit handle overlays and avaluesx, otherwise
;                        these get plotted twice and causes problems for ps
;-
;************************************************************************

;==============================================================================
PRO wrap_all_plots, para
;==============================================================================
; 04-10-98
; A wrapper to all_plots used by the GA_PRINTER object to print
; the equibrium, and echres as a callback.

  type = Tag_Names(para,/Structure_Name)
  case type of
    'PRINTOBJ':begin
	all_plots,para.top,*para.ptr_state,plot=para.plot
	end
    'DIAG':begin
	Widget_Control,para.top,Get_UValue=uvalue,/No_Copy
	viewer=uvalue.viewer
	Widget_Control,para.top,Set_UValue=uvalue,/No_Copy
	Widget_Control,viewer,Get_UValue=state,/No_Copy
	all_plots,para.top,state,plot=0
	Widget_Control,viewer,Set_UValue=state,/No_Copy
	end
     else:Message,type,/Info
  endcase

END ; wrap_all_plots

;==============================================================================
PRO all_plots,top,state,plot=plot
;==============================================================================
; check and put on all plots
; top - widget id for group_leader 
; state - information structure held by efit_viewer tlb
; plot - which plot to put on, index number or name of plot

; Catch errors
  if getenv_('DEBUG') eq '' then catch, error_status else error_status=0
  if error_status ne 0 then begin
     ok = Dialog_Message(!Err_String,Dialog_Parent=top)
     return
  endif

  Widget_Control,/Hourglass

; Check keywords
  plotnames = ['eq','chisq','prof1','prof2','pointname']
  if N_Elements(plot) eq 0 then plot=plotnames else begin
     sp = Size(plot)
     type = sp(N_Elements(sp)-2)
     if type eq 2 or type eq 3 then plot=plotnames[plot]   ; int or long
  endelse
  plot = strupcase(plot)

; Get the status of the plot buttons
;  WIDGET_CONTROL,state.wPlot,GET_VALUE=selected,/NO_COPY
  selected = [1,0,0,0,1]	

; Loop over plots
for iplot=0, N_Elements(plot)-1 do begin
case plot[iplot] of 

'EQ':begin
;------------------------------------------------------------------------------
; Contour plot - page 0
;------------------------------------------------------------------------------
  if WIDGET_INFO(state.plots.id0,/VALID) and selected[0] then begin
   ; put on contour plots
     if (!D.Name eq 'X' OR !D.Name eq 'MAC' or !D.Name eq 'WIN') then begin
        Widget_Control,state.plots.(state.nwindow),Get_Value=wid
        WSET,wid
     endif ; !d.name
     state = state.efit->equilibrium(Info=state,$
	wid=state.plots.id0,prefwid=state.wEqPref,ptr_pref=state.ptr_eqpref)
   ; put on overlays if g.error eq 0
;     g = state.efit->GetG(/New)
;     if WIDGET_INFO(state.wOverlay,/VALID) and g.error eq 0 then begin
;	;(*(*state.ptr_g)[0]).error eq 0 then begin
;	WIDGET_CONTROL,state.wOverlay,GET_UVALUE=wIds,/NO_COPY
;	overlay_plot,state,WidgetId=wIds,Group=top
;      	WIDGET_CONTROL,state.wOverlay,SET_UVALUE=wIds,/NO_COPY
;     endif 
     if (!D.Name eq 'X' or !D.Name eq 'MAC' or !D.Name eq 'WIN') then begin
   ; copy the display into the pixmap
        Widget_Control,state.plots.id0,Get_UValue=uvalue,/No_Copy
        WSet, uvalue.pixID
        Device, Copy=[0, 0, uvalue.xsize, uvalue.ysize, 0, 0, uvalue.wid]
        Widget_Control,state.plots.id0,Set_UValue=uvalue,/No_Copy
     endif ; !d.name
   ; update parameters
   Forward_Function avaluesx
     a = state.efit->GetA(/New)
     g = state.efit->GetG(/New)
     ;if (*(*state.ptr_a)[0]).error ne 0 then $
     if a.error ne 0 then $
	listvar = [' ',	'EFIT A file or MDSplus',$
			'equivalent is NOT',$
		       	'available for the',$
			'parameter list.'] else $
     	;listvar = avaluesx(*(*state.ptr_a)[0])
     	;listvar = avaluesx(a,g=g)
     if (!D.Name eq 'X' or !D.Name eq 'MAC' or !D.Name eq 'WIN') then $
        WIDGET_CONTROL,state.wPara,SET_VALUE=listvar

  endif else begin
   ; Window is not valid. Check if it is selected.  If selected, send a
   ; psudo event for selection button to the plot_event to create the window.
   ; Note that the event structure is anonymous (cw_group). Not needed anymore.
  endelse
end ; 'EQ'

'CHISQ': begin
;------------------------------------------------------------------------------
; Fitting Quality - page 1, replaced by iefit class
;------------------------------------------------------------------------------
;  if WIDGET_INFO(state.plots.id1,/VALID) and selected[1] then begin
;   ; Fitting quality page
;     wid = state.plots.(state.nwindow+1)
;     WIDGET_CONTROL,wid,GET_VALUE=curves,/NO_COPY
;     curves = chisq_plot(*state.ptr_m,curve=curves,oplot=state.oplot,$
;	color=sync_color(N_Elements(*state.ptr_g)))
;     WIDGET_CONTROL,wid,SET_VALUE=curves,/NO_COPY
;  endif
  end ; 'CHISQ'

;------------------------------------------------------------------------------
; Profiles - page 2 & 3, replaced by iefit class
;------------------------------------------------------------------------------
'PROF1':begin
;  if WIDGET_INFO(state.plots.id2,/VALID) and selected[2] then begin
;   ; Profiles page 1
;     wids = state.plots.(state.nwindow+2)
;     xcoord = ['r','rho','psi']
;     for i=0,2 do begin
;     	WIDGET_CONTROL,wids(i),GET_VALUE=curves
;	curves = prof_plot(*(*state.ptr_a)[0],*(*state.ptr_g)[0],$
;		mse=*state.ptr_m,xcoord(i),state.plot2,gfile=gfile,$
;		Grid=[2,3],curve=curves,oplot=state.oplot,$
;		color=sync_color(N_Elements(*state.ptr_g)))
;	WIDGET_CONTROL,wids(i),SET_VALUE=curves
;     endfor
;  endif
  end ; 'PROF1'

'PROF2':begin
;  if WIDGET_INFO(state.plots.id3,/VALID) and selected[3] then begin
;   ; Profiles page 2
;     wids = state.plots.(state.nwindow+3)
;     xcoord = ['r','rho','psi']
;     for i=0,2 do begin
;     	WIDGET_CONTROL,wids(i),GET_VALUE=curves
;	curves = prof_plot(*(*state.ptr_a)[0],*(*state.ptr_g)[0],$
; 		mse=*state.ptr_m,xcoord(i),state.plot3,gfile=gfile,$
;		Grid=[2,3],curve=curves,oplot=state.oplot,$
;		color=sync_color(N_Elements(*state.ptr_g)))
;	WIDGET_CONTROL,wids(i),SET_VALUE=curves
;     endfor
;  endif
  end ; 'PROF2'

'POINTNAME':begin
;------------------------------------------------------------------------------
; EFIT Pointnames 
;------------------------------------------------------------------------------
;  if WIDGET_INFO(state.plots.id4,/VALID) and selected[4] then begin
  wid = state.wInteractive[N_Elements(state.wInteractive)-1]
  if WIDGET_INFO(wid,/VALID) and selected[4] then begin
     if state.mode ne 'FILE' then path=0 else $
	path=(*state.ptr_picked).path
     WIDGET_CONTROL,wid,SET_VALUE=$
	{shot:state.shot,oplot:state.oplot,mode:state.mode,$
	 run:state.run,path:path}
  endif
  end ; 'POINTNAME'
  else :
endcase ; plot[iplot]
skip:
endfor ; iplot

;  WIDGET_CONTROL,state.wPlot,SET_VALUE=selected,/NO_COPY

END ; all_plots
