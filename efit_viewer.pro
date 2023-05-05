;************************************************************************
;+
; NAME:
;	EFIT_VIEWER
;
; PURPOSE:
;	A tool for viewing EFIT results including equilibrium, profiles,
;	fitting quality and time trace of 1-D data.  
;
; CATEGORY:
;	Widget
;
; CALLING SEQUENCE:
;	wId = EFIT_VIEWER(parent)
;
; INPUTS:
;	parent	The parent widget ID (optional).  If omitted, a top level
;		widget is created.
;
; KEYWORD PARAMETERS:
;	nopicker Do not include efitpicker. To update, use
;		Widget_Control,wId,Set_Value={shot:0L,time:0L}
;	_extra  Keywords that are acceptable to the top level base widget,
;		e.g., the Group_Leader.
;
; OUTPUTS:
;	None.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	X resource consuming if all plot pages are created.  Note that 
;	once a page of plot is opened, canceling it only hides it from
;	the display, but not from the memory.  
;
; RESTRICTIONS:
;	Restriced by the size of RAM if multiple plots are to be created.
;	Opening all five pages of plots with 8MB RAM will cause allocation
;	error almost certainly.
;
; PROCEDURE:
;	The main window of the EFITVIEWER includes the EFIT PICKER,
;	which selects a shot and a time either from a file or MDSplus,  
;	and the plot selection part.  It also features a multi-slice
;	overlay switch and a button for making a simple EFIT run 
;	using runefit on the fly.  Plot pages 2 to 5 utilize the 
; 	plot object GA_PLOT.
;
;	There are five pages of plots.  They are
;
;	1. Equilibrium - lists the scalar parameters from either
;		A file or MDSplus equvivalent, plots the equilibrium
;		reconstruction.  Features diagnostics and other 
;		overlays, and the preference setup.
;	2. Fitting Quality - chi squares etc.
;	3. Profiles 1 - Current
;			Pressure
;			Safety factor 
;			Pitch angle of MSE
;			Bz without Er
;			Br
;	4. Profiles 2 - Er
;			Gamma_Er
;			FFPrime
;			PPrinm 
;			Magnetic Shear
;			Bz with Er
;	5. Pointnames - time trace of 1-D data from A file or 
;		MDSplus equivalent.  This page is created with
;		CW_Pointname compound widget.
;
;	Toggle on a plot item either creates or maps the plot
;	window.  Toggle off or 'Done' with the plot unmaps the window.
;
;	Selecting a new slice from the efit picker updates all the
;	plot windows that are displayed (toggle on).  The off pages
;	are updated when they are brought up.
;
;	Turning on the multi-slice overlay puts the plots of next slice 
;	on top of the existing ones.  The overlay applies to plot pages
;	that have been created (whether toggeled on or off) when the
;	switch is turned on.  This feature does not apply to the page
;	5 pointname plot yet.  
;
;	The event handling routines include
;
;	PRO 	 efit_viewer_event
;	FUNCTION efit_viewer_print_event
;	FUNCTION efit_viewer_quit_event
;	PRO 	 killed_widget,id
;	PRO 	 efit_viewer_help_event
;	FUNCTION efit_viewer_run_event
;	FUNCTION efit_viewer_ref_event
;	FUNCTION efit_viewer_plot_event
;	PRO	 efit_viewer_setv
;	FUNCTION prof_coord_select_event
;	PRO 	 eq_draw_event
;	FUNCTION eq_animate_create_event
;	FUNCTION eq_pref_create_event
;	PRO 	 eq_pref_setv
;	FUNCTION eq_pref_getv
;	FUNCTION eq_overlay_create_event
;	FUNCTION eq_overlay_event
;
; EXAMPLE:
;	wId = efit_viewer()
;
; HISTORY:
;	02-20-98 Q. Peng first created
;	03-25-98 Q. Peng released for beta testing
;	04-10-98 Q. Peng released to all users
;	04-30-98 Q. Peng 
;		.replaced ECH (Prater)
;		.syncronized colors for all plot pages
;		.adapted event name change returned by efitpicker (Schachter)
;		.restructured the resizing of contour plot
;		.made pointname window resize, respond to shot selection 
;		.rearranged profiles (Rice)
;		.fixed some units confusion
;		.made printer object work on all pages (Schachter)
;		.reconstructed preference window for contour plot
;	05-01-98 Q. Peng 
;		.run summard automatically after time-dependent efit
;		.rerun xmanager at the main level error handler so that any 
;		 error that bubbles up here does not drop out the application 
;	05-11-98 Q. Peng fixed a pointer problem that caused the error in
;		 printing. It needs a revisit since it used to work.
;	05-15-98 Q. Peng added set value routine and nopicker option for 
;		 setting shot and time from a caller routine.
;	05-18-98 C.M.Greenfield Added window sizing workaround for MacOS.
;		 Substitute getenv with getenv_ for MacOS. Fixed negative
;		 sizing bug.
;	05-19-98 Q. Peng default runefit to NOT checking shot availablity
;		 per Schissel's request.
;	05-27-98 Q. Peng shot-time selection will NOT replot pointnames.
;		 Selecting on same shot enforces re-reading for pointnames.
;	06-11-98 C. Greenfield: Fixed Mac path specification bug.
;	06-18-98 Q. Peng fix the confusion between new and old MDS
;		 tangential (Brooks).
;	07-24-98 C. Greenfield: Forward_function setenv_ removed - 
;		 setenv_ is a procedure, not a function.
;	07-24-98 Q. Peng set character size for vector-drawn fonts instead
;		 of using the defautls to have a consistent look.
;	07-31-98 Q. Peng the set_character_size takes effect only after at
;		 least one window has been created.
;	10-02-98 Q. Peng added MDS tangential upper chords overaly (Brooks).
;	10-06-98 Q. Peng changed the overlay 'Photo Diode Chords' to
;		 'Filterscopes' per Colchin.  Cleaned up and corrected
;		 phdpath based on shot and date of the calibrations (Brooks). 
;     October,98 Q. Peng MAJOR CHANGE - replaced ga_plot with ga_plot_new. 
;		 Use iefit classes for all plots except equilibrium and time
;		 history.
;	12-16-98 Q. Peng for T. Carlstrom - added a new diagnostic overlay,
;		 Thomson view chords.
;	01-26-99 Q. Peng fixed a bug that caused problem in any file case.
;     	02-08-99 Q. Peng - Convert to MKS unit in reada only when mdsplus
;		 is installed.
;	03-02-99 Q. Peng for P. Gohil - added a new diagnostic overlay,
;		 CER chords.
;	11-03-99 Q. Peng construct diagnostic overlay widget based on 
;		 the diagnoses_table that is created by diagnoses_config.
;		 This allows customizable overlays.
;	04-06-2000 Q.Peng added more options for overlay for flexibility and
;                users' own overlay routine.
;	04-14-2000 Q.Peng added animation to the equilibrium window
;	04-26-2000 Q.Peng passed the zoomed range to animation.
;	01-12-2001 Q.P. Use MDSPLUS environment variable in conjunction with
;		 mdsplus_installed to decide whether to use MDSplus or not.
;		 Upgrade JET version efitviewer. MDSplus has been installed 
; 		 but there is no server nor efit trees.
;	07-05-2002 Q.Peng change state.overlay from {overlay} struct to pointer
;                to a struct returned by overlay_construct(). overlay__define
;                is replaced by function overlay_construct to build an overlay
;                struct based on MACHINE whenever a new site is selected.
;	09-24-2002 Use bold font for widget text. Default font is too small
;		 on some systems after OS and IDL(5.5.1) upgrade.
;	06-25-2003 Add 'Print All' under the main window to print all active
;		 plot windows.
;	07-25-2003 fixed a path parsing problem that caused wrong info attached
;		 to the shot_time label (ext in efit obj)
;	10-16-2003 added a switch to turn on/off the labels/annotations on 
;		 diagnostic overlay - for overlay routines that support this.
;       07-30-2004 accomodate design efit EFITDE trees
;       01-20-2006 handle file type 't' for geqdsk created by TRANSP
;                Note: if both g and t files exist and on, use g file.
;	03-06-2005 destroy wInteractive windows before destroying the main
;                window to fix "Xlib: unexpected async reply" error on Linux
;                (multi-threading related)
;       08-02-2006 destroy wInteractive window only if it is valid to 
;                prevent hanging at exiting. 
;       11-02-2006 added "mod Bp" option in Preference window
;                for plotting poloidal field (per Jackson's request)
;	12-05-2007 added a "Set as Ref/Clear Ref" button to allow setting
;                the current time-slice as the reference.
;       20090310 SMF Destroy plot windows when the experiment changes in
;                the efitpicker to avoi ptr errors.
;	20130201 SMF Changed coils overlay to handle both measured and 
;		     calculated f-coil values.
;	20151013 SMF ECE Overlap replaces ECE Info
;-
;************************************************************************

;==============================================================================
PRO efit_viewer_event,ev
;==============================================================================
;Message,'',/Informational
; Get the uvalue of the widget
; ev.handler is same as ev.top except for the efit_picker

  Forward_Function getenv_   ;, setenv_
  if getenv_('DEBUG') eq '' then catch, error_status else error_status=0
  if error_status ne 0 then begin
     if n_elements(uvalue) then $
	Widget_Control,ev.id,Set_UValue=uvalue,/No_Copy
     if n_elements(state) then $
	Widget_Control,ev.handler,Set_UValue=state,/No_Copy
     ok = Dialog_Message(!Err_String,Dialog_Parent=ev.id)
     return
  endif

  Widget_Control,/HOURGLASS
  Widget_Control,ev.handler,Get_UValue=state,/No_Copy
  Widget_Control,ev.id,Get_UValue=uvalue,/No_Copy

  IF Tag_Names(ev,/Struct) EQ 'CW_EFITPICK_SERVER' THEN BEGIN
    ;; Destroy the plot windows if the experiment changes in 
    ;; efitpicker.  This is to avoid ptr errors, notably w/ overlay. 
    for i=0,n_elements(state.wInteractive)-1 do begin
       if widget_info(state.wInteractive[i],/valid) then $
          widget_control,state.wInteractive[i],/destroy
    end

    ; destroy the pointname window if active
    index = where(state.efit->GetPlotProperty(pages=-1) EQ 'POINTNAMES_VS_TIME',count)
    IF count GT 0 THEN $
       IF Widget_Info(state.wInteractive[index[0]],/Valid) THEN BEGIN
          message,'need to update anames in pointname window',/info
          ;Widget_Control,state.wInteractive[index[0]],Send_Event=ev
       ENDIF
  ENDIF

  s = SIZE(uvalue)
  if s(n_elements(s)-2) ne 7 then goto, done		; not a string

  case uvalue(0) of 
   'OPLOT':begin
	;----------------------------------------------------------------------
	; from Multi Slice Overlay radio buttons. 0-on, 1-off
	;----------------------------------------------------------------------
	Widget_Control,ev.id,Get_Value=on	
	state.efit->SetProperty,oplot=1-on
	end
   'PICKER':begin
	;----------------------------------------------------------------------
      	; from cw_efitpick
	;----------------------------------------------------------------------
	*state.ptr_picked = ev
	state.shot = ev.shot[0]
	event_type = TAG_NAMES(ev,/STRUCTURE_NAME)
        if ( ev.machine ne state.machine ) then begin
           ; server change from picker
           ; contruct a new diagnostic overlay structure
           state.machine=ev.machine
           Ptr_Free,state.overlay
           Forward_Function overlay_construct
           state.overlay = Ptr_New(overlay_construct(machine=state.machine))
        endif
      	if event_type eq 'CW_EFITPICK_MDSTIME' then begin
	 ; MDSplus mode
	   state.time = ev.time
	   state.mode = 'MDSPLUS'
	   state.run = ev.run[0]
	 ; read data
	   a = reada(ev.shot[0],ev.time,Mode='MDSPLUS',RunId=ev.run[0],/Exact,$
		     MKS=state.mdsplus_installed)
	   g = readg(ev.shot[0],ev.time,Mode='MDSPLUS',RunId=ev.run[0],/Exact)
	   mse_calib_dir = getenv('MSE_CALIB_DIR')+'/'
; KLUDGE   - do not use readm_m() on non-D3D mdsplus servers
; 20170718 - this should be fixed later to better recognize the server/machine/efit being viewed
	   if (file_test(mse_calib_dir) and (state.machine eq 'DIII-D'))  then begin
	      m = readm_m(ev.shot[0],ev.time,Mode='MDSPLUS',RunId=ev.run[0],/Exact)
   	   endif else begin
	      m = readm(ev.shot[0],ev.time,Mode='MDSPLUS',RunId=ev.run[0],/Exact)
	      m = Create_Struct(m,'vport',0)
 	   endelse
	endif else if (event_type eq 'CW_EFITPICK_NMDSTIME') or $
		      (event_type eq 'CW_EFITPICK_ANYFILE') then begin
	 ; File mode
	   state.time = ev.time
	   state.mode = 'FILE'
	   if TAG_NAMES(ev,/STRUCTURE_NAME) ne 'CW_EFITPICK_ANYFILE' then BEGIN
	      case !VERSION.OS_FAMILY of		; CW_EFITPICK_NMDSTIME
	      	'vms': filenames = ev.path+ev.files
	      'MacOS': filenames = ev.files
	     	 else: filenames = ev.path+'/'+ev.files	
	      endcase
	   ENDIF else filenames = ev.files		; CW_EFITPICK_ANYFILE
	 ; a or g file must present; read in a, g, or m structure
	   geqdsk = 0  ; tag to remember whether G was read ('g' or 't')
	   for i=0, n_elements(ev.types)-1 do begin
	      filename = filenames(i)
	      case STRLOWCASE( ev.types(i) ) of 
	      'a':if ev.files(i) ne '' then begin
		     a = reada(filename,MKS=state.mdsplus_installed)
		     if state.shot eq 0 then state.shot = a.shot
		     if state.time eq 0 then state.time = a.time
		  endif else begin
		   ; file not found, use a template structure and set error 
		     a = *(*state.ptr_a)[0]
		     a.error=1 & a.shot=state.shot & a.time=state.time
		  endelse
	      'g':if ev.files(i) ne '' then begin
		     g = readg(filename) 
		     if state.shot eq 0 then state.shot = g.shot
		     if state.time eq 0 then state.time = g.time
		     geqdsk = 1
		  endif else begin
		     g = *(*state.ptr_g)[0]
		     g.error=1 & g.shot=state.shot & g.time=state.time
		  endelse
	      't':if geqdsk eq 0 then begin    ; G file is not present
                  ; t files are geqdsk files creatd by TRANSP
		     if ev.files(i) ne '' then begin
		        g = readg(filename) 
		        if state.shot eq 0 then state.shot = g.shot
		        if state.time eq 0 then state.time = g.time
		        geqdsk = 1
		     endif else begin
		        g = *(*state.ptr_g)[0]
		        g.error=1 & g.shot=state.shot & g.time=state.time
		     endelse
		  endif
	      'm':if ev.files(i) ne '' then begin
		     mse_calib_dir = getenv('MSE_CALIB_DIR')+'/'
		     if file_test(mse_calib_dir) then begin
		     	m = readm_m(filename,state.time,/Exact) 
		     endif else begin
			m = readm(filename,state.time,/Exact)
	                m = Create_Struct(m,'vport',0)
		     endelse
                   endif else m = {error:1,shot:state.shot,time:state.time}
	      else:
	      endcase
	   endfor
	endif else if event_type eq 'CW_EFITPICK_NMDSSHOT' then begin
	 ; File mode with shot returned only
	   state.mode = 'FILE'
   	   all_plots,ev.top,state,plot=4
	   goto, done
 	endif else if event_type eq 'CW_EFITPICK_MDSRUN' then begin
	 ; MDSplus mode with shot and run only
	   state.mode = 'MDSPLUS'
	   state.run = ev.run
   	   all_plots,ev.top,state,plot=4
	   goto, done
	endif else goto, done ; event_type

	; Store the new data in the state.
	; Use state.shot,time instead of ev.shot,time, because in
	; ANYFILE case, ev.shot,time are empty while state.shot,time
	; are set by a(g).shot,time
	; Find file extension - will be used for label
	subs = str_sep(g.source,'/')
	subs = str_sep(subs[N_Elements(subs)-1],'_')
	IF N_Elements(subs) GT 1 THEN ext='_'+subs[N_Elements(subs)-1] ELSE ext=''
	state.efit->SetProperty,shot=state.shot,time=state.time,$
		A=Ptr_New(a),G=Ptr_New(g),M=Ptr_New(m),Ext=ext

	; Special case for equilibrium plot. Check if the button is checked
	; but no plot is up.  In that case, send an event to create the 
	; window.  It only happens on the first selection of a slice.

	IF NOT Widget_Info(state.plots.id0,/Valid) AND $
	   Widget_Info(state.wPlot,/Valid) THEN BEGIN
	   Widget_Control,state.wPlot,Get_Value=value
	   IF value.value[0] EQ 1 THEN Widget_Control,value.wid,Send_Event=$
		{id:value.wid,top:ev.top,handler:0L,select:1,value:0},$
		Event_Func='efit_viewer_plot_event'
	ENDIF
      end ; PICKER
    else:
  endcase ; uvalue(0)

done:
  ; Update the Animation widget if there is one
  IF Widget_Info(state.wAnimate,/Valid) THEN BEGIN
     val = {shot:state.shot,runid:state.run}
     Widget_Control,state.plots.id0,Get_UValue=plotuval,/No_Copy
     IF (plotuval.zoom NE 0) THEN val = {val,$
	zoom:plotuval.zoom,xrange:plotuval.!x.range,yrange:plotuval.!y.range}
     Widget_Control,state.plots.id0,Set_UValue=plotuval,/No_Copy

     Widget_Control,state.wAnimate,Set_Value=val
  ENDIF

  if WIDGET_INFO(ev.handler,/VALID) then begin
  	if s(n_elements(s)-2) ne 0 then $	; valid uvalue
	Widget_Control,ev.id,Set_UValue=uvalue,/No_Copy
	Widget_Control,ev.handler,Set_UValue=state,/No_Copy
  endif
  return
END ; efit_viewer_event

;==============================================================================
PRO efit_viewer_print, state, event, printObj
;==============================================================================
;Message,'',/info
if getenv_('DEBUG') eq '' then catch, error_status else error_status=0
if error_status ne 0 then return

index = where(state.wInteractive NE 0, count)
IF count EQ 0 THEN Return
FOR i=0,count-1 DO BEGIN
  IF Widget_Info(state.wInteractive[index[i]],/Valid) THEN BEGIN
    CASE state.efit->GetPlotProperty(pages=index[i]) OF
    'PLASMA_EQUILIBRIUM':BEGIN
      ; force 'full portrait' for PS and restore afterwards
	IF !d.name EQ 'PS' THEN device,/portrait,yoffset=1.0,ysize=9.0,/inches
	all_plots,event.top,state,plot=0
    	IF !d.name EQ 'PS' THEN printObj->_ConfigPrinter
     END
    'POINTNAMES_VS_TIME':cw_pointname_print,state.wInteractive[index[i]]
     ELSE:cw_interactive_print,state.wInteractive[index[i]],0
    ENDCASE
  ENDIF
END
END ; efit_viewer_print

;==============================================================================
FUNCTION efit_viewer_print_event,ev
;==============================================================================
;Message,'',/info
if getenv_('DEBUG') eq '' then catch, error_status else error_status=0
if error_status ne 0 then begin
   IF n_elements(state) THEN Widget_Control,ev.top,Set_UValue=state,/No_Copy
   ok = Dialog_Message(!Err_String,Dialog_Parent=ev.id)
   return, 0
endif

Widget_Control,ev.id,Get_UValue=printObj
Widget_Control,ev.top,Get_UValue=state,/No_Copy
printObj->DialogPrint,'efit_viewer_print',state,ev,printObj
Widget_Control,ev.top,Set_UValue=state,/No_Copy

Return, 0
END

;==============================================================================
FUNCTION efit_viewer_quit_event,ev
;==============================================================================
;Message,'',/Informational

  if getenv_('DEBUG') eq '' then catch, error_status else error_status=0
  if error_status ne 0 then begin
     return, 0
  endif

   Widget_Control,ev.id,Get_UValue=uvalue 
   case uvalue(0) of
   'CANCEL': begin			; dismiss(unmap) the current window
	; Invoked by Cancel button on plot window
	; The uvalue of ev.top has structure of 
	; {viewer:efit_viewer widget ID,index:index number of the plots,
	;  caller:wid of the selection button and data coordinates variables 
	;  for contour window})

	Widget_Control,ev.top,MAP=0

	; Toggle off the corresponding selection button on the main menu
	; Note that the equilibrium is always the first button.

	Widget_Control,ev.top,Get_UValue=uvalue,/No_Copy
	IF Widget_Info(uvalue.caller,/Valid) THEN BEGIN
	   Widget_Control,uvalue.caller,Get_Value=value
	   value[0] = 0
	   Widget_Control,uvalue.caller,Set_Value=value
        ENDIF
	Widget_Control,ev.top,Set_UValue=uvalue,/No_Copy

	end
   'CW_POINTNAME_CANCEL':begin
	; Invoked by Cancel button on EFIT Pointname plot window
	; The uvalue of ev.top contains wCaller, which is the viewer
	Widget_Control,ev.top,Get_UValue=uvalue,/No_Copy
	viewer = uvalue.wCaller
	Widget_Control,viewer,Get_UValue=state,/No_Copy
	Widget_Control,state.wPlot,Get_Value=selected,/No_Copy
	selected(4) = 0
	Widget_Control,state.wPlot,Set_Value=selected,/No_Copy
	Widget_Control,viewer,Set_UValue=state,/No_Copy
	Widget_Control,ev.top,Set_UValue=uvalue,/No_Copy
	; unmap the window
	Widget_Control,ev.top,MAP=0
	end
   'EXIT': begin			; exit from the main window
	; clean up heap before quit
	Widget_Control,ev.top,Get_UValue=state,/No_Copy
	ng = N_Elements(*state.ptr_g)
	for i=0, ng-1 do begin
	    if Ptr_Valid((*state.ptr_a)[i]) then Ptr_Free,(*state.ptr_a)[i]
	    if Ptr_Valid((*state.ptr_g)[i]) then Ptr_Free,(*state.ptr_g)[i]
	endfor
	if PTR_VALID(state.ptr_a) then PTR_FREE,state.ptr_a
	if PTR_VALID(state.ptr_g) then PTR_FREE,state.ptr_g
	if PTR_VALID(state.ptr_m) then PTR_FREE,state.ptr_m
	if PTR_VALID(state.ptr_time) then PTR_FREE,state.ptr_time
	if PTR_VALID(state.ptr_picked) then PTR_FREE,state.ptr_picked
	common managed,ids,names,outermodel
	FOR i=0,N_Elements(ids)-1 DO BEGIN
	   IF ids[i] NE ev.top AND Widget_Info(ids[i],/Valid) THEN $
	      Widget_Control,ids[i],/Destroy
	END
	;FOR i=0,N_Elements(state.wInteractive)-1 DO BEGIN
	;   IF Widget_Info(state.wInteractive[i],/Valid) THEN $
	;   Widget_Control,state.wInteractive[i],/DESTROY
	;END
	Widget_Control,ev.top,/DESTROY
	end
    else:
   endcase

   return, 0
END ; efit_viewer_quit_event

;==============================================================================
PRO killed_widget,id
;==============================================================================
;Message,'',/Informational

; Invoked by close of pointname window

; clean up, unset the pointnames button
  if not WIDGET_INFO(id,/VALID) then return
  Widget_Control,id,Get_UValue=uvalue,/No_Copy
  if WIDGET_INFO(uvalue.wCaller,/VALID) then $
  Widget_Control,uvalue.wCaller,SET_BUTTON=0
  Ptr_Free,uvalue.ptr_a
  Ptr_Free,uvalue.ptr_time

END ; killed_widget

;==============================================================================
PRO efit_viewer_help_event,ev
;==============================================================================
;Message,'',/Informational

  if getenv_('DEBUG') eq '' then catch, error_status else error_status=0
  if error_status ne 0 then begin
     ok = Dialog_Message(!Err_String,Dialog_Parent=ev.id)
     return
  endif

; Invoked by items in help menu
  Widget_Control,ev.id,Get_Value=value
  case value of
    'Plot Object Window':begin
	; help for ga_plot objects
	filename = '$IDLSOURCE/ga_plot_new/ga_plot.help'
	xdisplayfile,GA_FileSpec(filename),title='Help on Plot Window',$
		group=ev.id
	end
    'Printing':begin
	; help on printer objects
	Widget_Control,ev.id,Get_UValue=printObj
	printObj->Help
	end
    'About EFIT Viewer':begin
        restore,getenv('EFITVIEW')+'/version.compile'
        tvlct,about_r,about_g,about_b 
        window,1,title=about_version,xsize=about_info.dimensions[0],ysize=about_info.dimensions[1]
        tv,about_image 
        color_setup,/reverse
        end
  else:
  endcase

END ; efit_viewer_help_event

;==============================================================================
FUNCTION efit_viewer_ref_event,ev
;==============================================================================
;Message,'',/Informational

; Get the info structure
  sid = Widget_Info(ev.id,/Parent)
  Widget_Control,sid,Get_UValue=viewer
  Widget_Control,viewer,Get_UValue=state,/No_Copy
  
  Widget_Control,ev.id,Get_UValue=uval
  CASE uval[0] OF
   'REF_ON': BEGIN
	state.efit->SetReference	
	Widget_Control,ev.id,Set_Value='Clear Ref'
	Widget_Control,ev.id,Set_UValue='REF_OFF'
	END
   'REF_OFF': BEGIN
	state.efit->ClearReference
	Widget_Control,ev.id,Set_Value='Set as Ref'
	Widget_Control,ev.id,Set_UValue='REF_ON'
	END
    ELSE: help,uval
  ENDCASE ; uval

  Widget_Control,viewer,Set_UValue=state,/No_Copy
  return, 0

END

;==============================================================================
FUNCTION efit_viewer_run_event,ev
;==============================================================================
;Message,'',/Informational
; 04-09-98
; Invoked by the run efit button or the buttons on the run efit window.
; Spawn an efit run.

  if getenv_('DEBUG') eq '' then catch, error_status else error_status=0
  if error_status ne 0 then begin
     if n_elements(state) ne 0 then $
	Widget_Control,viewer,Set_UValue=state,/No_Copy
     ok = Dialog_Message(!Err_String,Dialog_Parent=ev.id)
     return, 0
  endif

; Get the information structure.
  sid = Widget_Info(ev.id,/Parent)
  Widget_Control,sid,Get_UValue=top
  Widget_Control,top,Get_UValue=uvalue,/No_Copy
  type = Tag_Names(uvalue,/Struct)
  if type eq 'RUNEFIT' then begin
   ; Invoked by buttons in the run efit window.
   ; Viewer Id is stored in uvalue
     viewer = uvalue.viewer
     wInput = uvalue.wInput
  endif else begin
   ; Invoked by run efit button on the main window
   ; The uvalue is the state. Put it back and get again for consistancy.
     viewer = top
  endelse
  Widget_Control,top,Set_UValue=uvalue,/No_Copy
  Widget_Control,viewer,Get_UValue=state,/No_Copy

  Widget_Control,ev.id,Get_UValue=uval
  case uval[0] of
   'RUNEFIT':begin
      ; Create or Map the run efit window.
	IF Widget_Info(state.wRunEfit,/Valid) THEN BEGIN
	   Widget_Control,state.wRunEfit,Set_Value={$
		shot:state.efit->GetShot(),time:state.efit->GetTime()}
	   Widget_Control,state.wRunEfit,Map=1
	ENDIF ELSE BEGIN
	   state.wRunEfit = cw_runefit(Shot=state.efit->GetShot(),$
		Time=state.efit->GetTime(),Group_leader=top,/Floating,$
		Tlb_Frame_Attr=2)
	ENDELSE ; valid
	end
   'OK':begin
      ; Spawn an efit. UnMap the widget.
	Widget_Control,wInput,Get_Value=input
      ; change the diretory
	z = file_search(input.dir,count=c)
	if c eq 0 then begin
	 ; invalid directory, pop up the pickdir widget.
	   input.dir = pickdir(Title='Invalid directory - Pick a directory:',$
			Group=top,/Floating)
	   if input.dir eq '' then input.dir = getenv_('PWD')
	   Widget_Control,wInput,Set_Value=input
	endif	
        cd, input.dir
	;;; JMS 98 - added fix to input.start which otherwise 
	;;; gives error in script runefit.
	if input.nstep eq 1 then dosummard = '' else dosummard = '-a'
	cmd = '/d/hp/codes/runefit -s -v 6565d '+dosummard+' background '+ $
		strcompress(input.shot) + strcompress(fix(input.start)) + $  
		strcompress(input.step) + strcompress(input.nstep) + $
		' ' + input.snap
	spawn,cmd,/sh
	Widget_Control,top,map=0
	end
   'CANCEL':begin
      ; UnMap the widget without spawning the efit.
	Widget_Control,top,map=0
	end
    else: help,uval
  endcase ; uval

  Widget_Control,viewer,Set_UValue=state,/No_Copy
  return, 0
END ; efit_viewer_run_event

;==============================================================================
FUNCTION efit_viewer_plot_event,ev
;==============================================================================
;Message,'',/Informational

; handle the event from the plot-selection check buttons 
; or resizing of each graphic window
; 02-19-98

  if getenv_('DEBUG') eq '' then catch, error_status else error_status=0
  if error_status ne 0 then begin
     if n_elements(uvalue) then $
	Widget_Control,ev.id,Set_UValue=uvalue	; event type 'WIDGET_BASE' 
     if n_elements(state) then begin
	if n_elements(wIds) then $
	Widget_Control,state.wOverlay,Set_UValue=wIds,/No_Copy
	Widget_Control,viewer,Set_UValue=state,/No_Copy
     endif
     ok = Dialog_Message(!Err_String,Dialog_Parent=ev.id)
     return, 0
  endif

  Widget_Control,/HOURGLASS
  event_type = TAG_NAMES(ev,/STRUCT)

  if event_type eq 'WIDGET_BASE' then begin	
;---------------------------------------------------------------------------
; Invoked by widget resize
;---------------------------------------------------------------------------
     resize = 1
     Widget_Control,ev.id,TLB_GET_SIZE=wsize,TLB_GET_OFFSET=offset,$
		    Get_UValue=uvalue,/No_Copy
     viewer = uvalue.viewer				; tlb of efit_viewer
     index = uvalue.index				; index of plot window
     caller = uvalue.caller				; wid of select button.
     Widget_Control,ev.id,Set_UValue=uvalue,/No_Copy
     Widget_Control,viewer,Get_UValue=state,/No_Copy
     if index eq 1 then $			; fitting quality window
	Widget_Control,state.plots.(index+state.nwindow),Get_Value=curves0 $
     else if index gt 1 then begin			; profiles windows
        Widget_Control,(state.plots.(index+state.nwindow))(0),Get_Value=curves0
	Widget_Control,(state.plots.(index+state.nwindow))(1),Get_Value=curves1
	Widget_Control,(state.plots.(index+state.nwindow))(2),Get_Value=curves2
     endif
     if Widget_Info(state.wEqPref,/Valid) then begin	; pref of the eq window
	Widget_Control,state.wEqPref,Get_Value=epref
	*state.ptr_eqpref = epref
     endif
     if Widget_Info(state.wOverlay,/Valid) then overlay_valid = 1
     if index eq 0 then begin
      ; equilibrium window, destroy the plot and the list only
	Widget_Control,state.wPara,/Destroy
	Widget_Control,state.plots.w0,/Destroy
     endif else begin
      ; other plot window. destroy the entire window
	Widget_Control,ev.id,/Destroy
     endelse
     xsize = wsize(0)
     ysize = wsize(1)
     xoffsets = offset(0)-10
     yoffsets = offset(1)-30
     wPlotBase = ev.id
     on = 1
     count = 1

  endif else if event_type eq 'WIDGET_BUTTON' then begin
;---------------------------------------------------------------------------
; Invoked by buttons on each plot window.  
; 'Print' from contour window only for now.
;---------------------------------------------------------------------------
     Widget_Control,ev.top,Get_UValue=uvalue,/No_Copy
     viewer = uvalue.viewer
     Widget_Control,ev.top,Set_UValue=uvalue,/No_Copy
     Widget_Control,viewer,Get_UValue=state,/No_Copy
     Widget_Control,ev.id,Get_Value=value
     if value eq 'Print' then begin
	Widget_Control,ev.id,Get_UValue=printObj
	printObj->SetProperty,'format','Full Portrait'
	ptr_state=Ptr_New(state)
	printObj->DialogPrint,'wrap_all_plots',$
		uvalue={PRINTOBJ,top:ev.top,ptr_state:ptr_state,plot:0}
	;Ptr_Free,ptr_state	; need to figure out where to delete the ptr
     endif
     goto,done

  endif else begin
;---------------------------------------------------------------------------
; Invoked by plot selection toggle/check button on/off
; Event is sent by 'cw_page_select'.
;---------------------------------------------------------------------------
     resize = 0

   ; Get the uvalue of the viewer's top base
   ; The draw widget IDs and their window IDs are stored in state.plots

     Widget_Control,ev.id,Get_UValue=viewer
     Widget_Control,viewer,Get_UValue=state,/No_Copy

     index = ev.value 
     caller = ev.id
     on = ev.select
     wPlotBase = state.plots.(index)

   ;---------------------------------------------------------------------------
   ; If the draw widget is valid, put it on or withdraw, then return
   ; If the draw widget is NOT valid, and is NOT selected, return
   ;---------------------------------------------------------------------------
     
     if WIDGET_INFO(wPlotBase,/VALID) then begin
        if on then begin
	   Widget_Control,wPlotBase,MAP=1
	   all_plots,viewer,state,plot=index
	endif else Widget_Control,wPlotBase,MAP=0
        goto,done
     endif

     if (not WIDGET_INFO(wPlotBase,/VALID)) and (not on) then goto,done

   ; find the screen size for scaling and positioning
     DEVICE,GET_SCREEN_SIZE=screen
     p=4./9.					; scale of draw windows
     xsize = screen(0)*p
     ysize = screen(1)*(1-p)
   ; if screen(1) lt 1000 then yunit=screen(1)*0.2 else yunit=screen(1)*0.17
     Widget_Control,ev.id,Get_Value=value
     z=where(value[0:N_Elements(value)-1] ne 0,count) ;number of selected plots
     xoffsets = [screen(0)*(1-p),screen(0)*(1-p),$    ; need to fix for count>4
	         screen(0)*(0.95-2*p),screen(0)*(0.95-2*p)]
     yoffsets = [0,screen(1)*(1-p),screen(1)*(1-p),0]  

  endelse

;------------------------------------------------------------------------------
; The draw widget is NOT valid but selected or 
; destroyed by resizing, create one
;------------------------------------------------------------------------------

; create widgets
  titles=['Plasma Equilibrium','Fitting Qualities','Profiles 1','Profiles 2']
  if (index eq 0 and not resize) or (index ge 1 and index le 3) then $
  wPlotBase = Widget_Base( /COLUMN,$				; first 4 plots
		XOFFSET=xoffsets(count-1),YOFFSET=yoffsets(count-1),$
		TLB_FRAME_ATTR=8,/TLB_SIZE_EVENTS,GROUP=viewer,$
		TITLE=titles(index),EVENT_FUNC='efit_viewer_plot_event',$
		UVALUE={viewer:viewer,index:index,caller:caller})

  ; For cw_page_select to (un)map later.

  state.wInteractive[index] = wPlotBase

  if index ge 1 and index le 3 then begin
	;----------------------------------------------------------------------
	; Fitting Qualities, Profiles 1 and 2 have common parts
	;----------------------------------------------------------------------
	wToolBase = Widget_Base(wPlotBase,/ROW,/FRAME)
  	wCancelButton = Widget_Button(wToolBase,VALUE=' Done ',$
		UVALUE='CANCEL',EVENT_FUNC='efit_viewer_quit_event')
	if index ne 1 then begin			; profiles
	xi = where(['r','rho','psi'] eq state.x)
	if xi(0) eq -1 then xi = 0
  	wXcoordButtons = CW_BGroup(wToolBase,['R','rho','normalized psi'],$
		BUTTON_UVALUE=['r','rho','psi'],Set_Value=xi(0),/NO_RELEASE,$
		/EXCLUSIVE,/ROW,EVENT_FUNC='prof_coord_select_event')
	endif
  	;wHelpButton = Widget_Button(wToolBase,VALUE='Help',/HELP)

  	wDrawBase = Widget_Base(wPLotBase)
  endif

; put plots on
  cs = size(curves0)
  case index of
    0 : begin					; contour plot
	;----------------------------------------------------------------------
    	; Plasma equilibrium and global parameters
  	;----------------------------------------------------------------------
	if not resize then begin
	 ; Create the part of widgets that are not destroyed by resizing.
	   wToolBase = Widget_Base(wPlotBase,/ROW,/FRAME)
  	   wCancelButton = Widget_Button(wToolBase,VALUE=' Done ',$
		UVALUE='CANCEL',EVENT_FUNC='efit_viewer_quit_event')
	   wPrefButton = Widget_Button(wToolBase,Value='Preference',$
		UValue='PREF',Event_Func='eq_pref_create_event')
  	   wOverlayButton = Widget_Button(wToolBase,$
		Value='Diagnostic Overlays',UValue='OVERLAY',$
		EVENT_FUNC='eq_overlay_create_event')
	   diagnoses_table,name=name	; disable the button if no diagnostics
	   IF N_Elements(name) EQ 1 AND name[0] EQ '' THEN $
	      Widget_Control,wOverlayButton,Sensitive=0
	   wAnimateButton = Widget_Button(wToolBase,$
		Value='Animation',UValue='ANIMATE',$
		EVENT_FUNC='eq_animate_create_event')
	   wStatusLabel = Widget_Label(wToolBase,Value='(R,Z)')
	   wStatusText = Widget_Text(wToolBase,Value='',XSize=13)
	   printObj = Obj_New('GA_PRINTER',wPlotBase,Title='Print Equilibrium')
  	   wPrintButton = Widget_Button(wToolBase,VALUE='Print',$
		UValue=printObj)

	   wPlotEqBase = Widget_Base(wPlotBase,/ROW)

	   xsize = long(xsize/2)
	   ysize = long(ysize-40)
	endif else begin
	 ; Resize
	   Widget_Control,wPlotBase,Get_UValue=uvalue,/No_Copy
	   wPlotEqBase = uvalue.wPlotEq
	   wStatusText = uvalue.wStatus
	   xsize = long(uvalue.xsize + wsize[0] - uvalue.wsize[0])
	   ysize = long(uvalue.ysize + wsize[1] - uvalue.wsize[1])
	endelse

      ; Put on variables
	wText = Widget_Text(wPlotEqBase,XSIZE=30,YSIZE=30,/SCROLL,$
		VALUE='')
  	wDraw = WIDGET_DRAW(wPlotEqBase,XSIZE=xsize,YSIZE=ysize,$
		UVALUE=wPlotBase,/Motion_Event,/Button_Event,Retain=2,$
		Event_Pro='eq_draw_event')

      ; Store the wids into state.
        state.plots.(index) = wPlotBase			; plot widget ID
	state.wPara = wText
        state.plots.(index+state.nwindow) = wDraw	; draw widget/base ID

      ; Realize the window
      ; include MacOS window sizing bug workaround if necessary
        if not resize then begin 
            if (!VERSION.OS eq 'MacOS') then begin
                widthTool=(widget_info(wToolBase,/geometry)).xsize
  	        widthPlot=(widget_info(wPlotEqBase,/geometry)).xsize
  	        Widget_Control,wPlotBase,xsize=max([widthTool,widthPlot])+21,$
			      /REALIZE
  	    endif else Widget_Control,wPlotBase,/REALIZE
	    Widget_Control,wPlotBase,TLB_GET_SIZE=wsize
	endif
  	Widget_Control,wDraw,Get_Value=wid

      ; Include fix for negative xsize
        if (xsize le 0) then begin
     	    widthTool=$
	      (widget_info(widget_info(wPlotBase,/child),/geometry)).scr_xsize
     	    widthText=(widget_info(wtext,/geometry)).scr_xsize
     	    xsize=long(widthTool-widthText)
     	endif

      ; Create a pixmap for erasing, etc.
	Window,XSize=xsize,YSize=ysize,/Pixmap,/Free
	pixID = !D.Window

      ; Set uvalue for the plot base for keeping data coordiates, 
      ; cursor tracking and zooming.
	uvalue = {viewer:viewer,index:index,caller:caller,!p:!p,!x:!x,!y:!y,$
		x1:0,y1:0,xsize:xsize,ysize:ysize,wsize:wsize,$
		wStatus:wStatusText,wPlotEq:wPlotEqBase,$
		wid:wid,pixID:pixID,cursorcolor:color_index('Red'),zoom:0}
	Widget_Control,wPlotBase,Set_UValue=uvalue

      ; Put the plot on
  	WSet,wid
	state = state.efit->Equilibrium(Info=state,machine=state.machine)

	state.plots.(index) = wPlotBase
	state.plots.(index+state.nwindow) = wDraw
        if Widget_Info(state.wOverlay,/Valid) then begin
	   WIDGET_CONTROL,state.wOverlay,GET_UVALUE=wIds,/NO_COPY
	   overlay_plot,state,WidgetId=wIds,Group=viewer
      	   WIDGET_CONTROL,state.wOverlay,SET_UVALUE=wIds,/NO_COPY
	endif
     
      ; copy the display into the pixmap
	WSet, uvalue.pixID
	Device, Copy=[0,0,uvalue.xsize,uvalue.ysize,0,0,uvalue.wid]

      ; if resize, re-create the animation if was on
        IF resize AND Widget_Info(state.wAnimate,/Valid) THEN BEGIN
     	   Widget_Control,state.wAnimate,/Destroy
     	   Widget_Control,state.plots.id0,Get_UValue=plotuval,/No_Copy
     	   zoom = plotuval.zoom
           xrange = plotuval.!x.range
     	   yrange = plotuval.!y.range
     	   Widget_Control,state.plots.id0,Set_UValue=plotuval,/No_Copy
     	   state.wAnimate = Eq_Animate(state.shot,runid=state.run,$
			epref=epref,draw=wDraw,group=wPlotBase,/Floating,$
			zoom=zoom,xrange=xrange,yrange=yrange)
        ENDIF

	end
    4 : begin
	;----------------------------------------------------------------------
	; Pointnames
	;----------------------------------------------------------------------
	wPlotBase=cw_pointname(state.shot,state.run,wid=state.plots.id4,$
		group=ev.top,caller=viewer,$
		cancel_event='efit_viewer_quit_event',$
		Kill_Notify='killed_widget')
	if state.mode ne 'FILE' then path=0 else $
		path=(*state.ptr_picked).path
	Widget_Control,wPlotBase,Set_Value=$
		{shot:state.shot,oplot:state.oplot,mode:state.mode,$
		 run:state.run,path:path}
	end
  else:
  endcase
  if index le 3 then $
     Widget_Control,wPlotBase,/REALIZE

done:
  Widget_Control,viewer,Set_UValue=state,/No_Copy
  return, 0
END ; efit_viewer_plot_event

;==============================================================================
FUNCTION prof_coord_select_event,ev
;==============================================================================
;Message,'',/Informational

  if getenv_('DEBUG') eq '' then catch, error_status else error_status=0
  if error_status ne 0 then begin
     if n_elements(state) then $
	Widget_Control,viewer,Set_UValue=state,/No_Copy
     ok = Dialog_Message(!Err_String,Dialog_Parent=ev.id)
     return, 0
  endif

; Get the uvalue of top base
  Widget_Control,ev.top,Get_UValue=uvalue,/No_Copy
  viewer = uvalue.viewer				; tlb of efit_viewer
  index = uvalue.index
  Widget_Control,ev.top,Set_UValue=uvalue,/No_Copy
  Widget_Control,viewer,Get_UValue=state,/No_Copy

; Set the new x coord name
  state.x = ev.value

  if WIDGET_INFO(state.plots.(index),/VALID) then begin	; plotbase
     if (index eq 2) or (index eq 3) then begin 
	; map the desired one
	imap = where(['r','rho','psi'] eq state.x)
	for i=0,2 do Widget_Control,(state.plots.(index+state.nwindow))(i),$
				     MAP=(imap(0) eq i)
     endif
  endif 
  
  Widget_Control,viewer,Set_UValue=state,/No_Copy
  return, 0
END ; prof_coord_select_event

;==============================================================================
PRO eq_draw_event,ev
;==============================================================================
;Message,'',/Informational
; Only handle UP, DOWN, and MOTION events.
; In motion event, use uvalue.zoom to distinguish between zooming and 
; cursor tracking.
; zoom	0 - unzoomed.
;	1 - button pressed but not released yet for zooming.
;	2 - button released and plot is zoomed and has not been reset.

  if ev.type gt 3 then return

  if getenv_('DEBUG') eq '' then catch, error_status else error_status=0
  if error_status ne 0 then begin
     if n_elements(uvalue) then $
	Widget_Control,wPlotBase,Set_UValue=uvalue,/No_Copy
     if n_elements(state) then $
	Widget_Control,viewer,Set_UValue=state,/No_Copy
     ok = Dialog_Message(!Err_String,Dialog_Parent=ev.id)
     return
  endif

; Get the location of information and retrive the info.
  Widget_Control, ev.id, Get_UValue=wPlotBase
  Widget_Control,wPlotBase,Get_UValue=uvalue,/No_Copy

; What kind of draw event is this:
  eventTypes=['BUTTON DOWN','BUTTON UP', 'MOTION']
  thisEvent = eventTypes[ev.type]

; Restore data coordinates.
  oldP = !p
  oldX = !x
  oldY = !y
  !p = uvalue.!p
  !x = uvalue.!x
  !y = uvalue.!y

  case thisEvent of
    'BUTTON DOWN': begin
      ; store one corner of the box
	uvalue.x1 = ev.x
	uvalue.y1 = ev.y
      ; set the zoom status
	uvalue.zoom = 1
	end
    'BUTTON UP': begin
	Widget_Control,/Hourglass
      ; check if a box is created
	if uvalue.x1 ne ev.x and uvalue.y1 ne ev.y then begin
	 ; Set the new range.  Prepare for zoomed plot.
	   x1 = min([uvalue.x1, ev.x],max=x2)
	   y1 = min([uvalue.y1, ev.y],max=y2)
         ; Convert coordinate from device to data.
	   newCoords = Convert_Coord([x1,x2],[y1,y2],/Device,/To_Data)
         ; Make sure the coordinates are inside the plot boundaries.
	   xx1 = !X.CRange[0] > newCoords[0,0] < !X.CRange[1]
	   xx2 = !X.CRange[0] > newCoords[0,1] < !X.CRange[1]
	   yy1 = !Y.CRange[0] > newCoords[1,0] < !Y.CRange[1]
	   yy2 = !Y.CRange[0] > newCoords[1,1] < !Y.CRange[1]
         ; Store the new range.
	   uvalue.!x.range = [xx1, xx2]
	   uvalue.!y.range = [yy1, yy2]
	 ; Store zoom status.  
	 ; If a line or a box outside of boundary is drawn, unzoomed the plot.
	   if xx1 eq xx2 or yy1 eq yy2 then uvalue.zoom = 0 else $
	   uvalue.zoom = 2
	endif else begin
	 ; Unset zoom status.  Prepare for unzoomed plot
	   uvalue.zoom = 0
	endelse
      ; Set the uvalue back.  Draw the new plot.
	viewer = uvalue.viewer
	Widget_Control,wPlotBase,Set_UValue=uvalue,/No_Copy
	Widget_Control,viewer,Get_UValue=state,/No_Copy
	state = state.efit->Equilibrium(Info=state,machine=state.machine)
	wAnimate = state.wAnimate
	Widget_Control,viewer,Set_UValue=state,/No_Copy
      ; Get the uvalue again, since it has been modified by eq_plot.
	Widget_Control,wPlotBase,Get_UValue=uvalue,/No_Copy
      ; copy the new display into the pixmap.	; not needed, done by all_plots
;	WSet,uvalue.pixID
;	Device, Copy=[0,0,uvalue.xsize,uvalue.ysize,0,0,uvalue.wid]
      ; Pass the zoomed range to animation window if exists
  	IF Widget_Info(wAnimate,/Valid) THEN $
	   Widget_Control,wAnimate,Set_Value=$
	     {zoom:uvalue.zoom,xrange:uvalue.!x.range,yrange:uvalue.!y.range}
	end
    'MOTION': begin
      ; Erase the current plot.
	WSet, uvalue.wid
	Device, Copy=[0,0,uvalue.xsize,uvalue.ysize,0,0,uvalue.pixID]
      ; Convert coordinate from device to data.
	newCoords = Convert_Coord(ev.x,ev.y,/Device,/To_Data)
	newx = newCoords[0]
	newy = newCoords[1]
      ; Skip if out of boundary
	if newx lt !x.crange[0] or newx gt !x.crange[1] or $
	   newy lt !y.crange[0] or newy gt !y.crange[1] then goto,done
      ; Display the cursor tracking.
  	Widget_Control,uvalue.wStatus,Set_Value=$
		string(string(newx),format='(f6.3)')+','+$
		string(string(newy),format='(f6.3)')
	if uvalue.zoom ne 1 then begin
         ; Cursor mode.  
	 ; Draw cursor.
	   PlotS, [newx,newx],!y.crange,color=uvalue.cursorcolor
	   PlotS, !x.crange,[newy,newy],color=uvalue.cursorcolor
	endif else begin
         ; Zoom mode
         ; Erase the last box and draw the new box
	   Device, Copy=[0,0,uvalue.xsize,uvalue.ysize,0,0,uvalue.pixID]
	   x1 = min([uvalue.x1, ev.x],max=x2)
	   y1 = min([uvalue.y1, ev.y],max=y2)
	   PlotS,[x1,x1,x2,x2,x1],[y1,y2,y2,y1,y1],/Device,$
		Color=uvalue.cursorColor
	endelse
	end
  endcase

done:
; Set data coordinates back.
  !p = oldP
  !x = oldX
  !y = oldY
  Widget_Control,wPlotBase,Set_UValue=uvalue,/No_Copy
END ; eq_draw_event

;==============================================================================
FUNCTION eq_animate_create_event,ev
;==============================================================================
; Invoked by the Animation button on equilibrium window
; or by the Done button on the pref window

Forward_Function eq_animate

  Widget_Control,/Hourglass

  if getenv_('DEBUG') eq '' then catch, error_status else error_status=0
  if error_status ne 0 then begin
     if n_elements(state) then $
	Widget_Control,viewer,Set_UValue=state,/No_Copy
     ok = Dialog_Message(!Err_String,Dialog_Parent=ev.id)
     return, 0
  endif

; Get the information structure.
  Widget_Control,ev.top,Get_UValue=uvalue,/No_Copy
  viewer = uvalue.viewer
  Widget_Control,ev.top,Set_UValue=uvalue,/No_Copy

  Widget_Control,viewer,Get_UValue=state,/No_Copy

  IF Widget_Info(state.wEqPref,/Valid) THEN $
     Widget_Control,state.wEqPref,Get_Value=epref ELSE epref=default_eq_pref()

  IF Widget_Info(state.wAnimate,/Valid) THEN BEGIN
     val={shot:state.shot,runid:state.run,epref:epref,now:1,$
	  draw:state.plots.(state.nwindow)}
     Widget_Control,state.plots.id0,Get_UValue=plotuval,/No_Copy
     IF (plotuval.zoom NE 0) THEN val = {val,$
	zoom:plotuval.zoom,xrange:plotuval.!x.range,yrange:plotuval.!y.range}
     Widget_Control,state.plots.id0,Set_UValue=plotuval,/No_Copy

     Widget_Control,state.wAnimate,Set_Value=val
  ENDIF ELSE BEGIN
     Widget_Control,state.plots.id0,Get_UValue=plotuval,/No_Copy
     zoom = plotuval.zoom
     xrange = plotuval.!x.range
     yrange = plotuval.!y.range
     Widget_Control,state.plots.id0,Set_UValue=plotuval,/No_Copy
     state.wAnimate = Eq_Animate(state.shot,runid=state.run,$
			epref=epref,group=state.plots.(0),$
			draw=state.plots.(state.nwindow),/Floating,$
			zoom=zoom,xrange=xrange,yrange=yrange)
  END

  Widget_Control,viewer,Set_UValue=state,/No_Copy

  Return,0
END ; eq_animate_create_event


;==============================================================================
PRO eq_pref_editcallback,ev,top
;==============================================================================

; Called by cw_struct for any typing or return (all_event). 
; Sensitize the 'Apply' button if typing, de-sensitize if return.
; The ev is the event that invoked by field all_event. 
; Top is the caller of the cw_struct, or the viewer.  
; It should carry the info structure of the caller application.

IF NOT Widget_Info(top,/Valid) THEN Return

Widget_Control,top,Get_UValue=state,/No_Copy
Widget_Control,state.wEqPref,Get_UValue=uvalue,/No_Copy
IF ev.update THEN Widget_Control,uvalue.wApply,Sensitive=0 $	; return
	     ELSE Widget_Control,uvalue.wApply,Sensitive=1	; typing
Widget_Control,state.wEqPref,Set_UValue=uvalue,/No_Copy
Widget_Control,top,Set_UValue=state,/No_Copy

END ; eq_pref_editcallback

;==============================================================================
FUNCTION eq_pref_create_event,ev
;==============================================================================
;Message,'',/Informational
; Invoked by the Preference button on equilibrium window
; or by the Done button on the pref window

  Widget_Control,/Hourglass

  if getenv_('DEBUG') eq '' then catch, error_status else error_status=0
  if error_status ne 0 then begin
     if n_elements(state) then $
	Widget_Control,viewer,Set_UValue=state,/No_Copy
     ok = Dialog_Message(!Err_String,Dialog_Parent=ev.id)
     return, 0
  endif

; Get the information structure.
  Widget_Control,ev.top,Get_UValue=uvalue,/No_Copy
  viewer = uvalue.viewer
  Widget_Control,ev.top,Set_UValue=uvalue,/No_Copy

  Widget_Control,viewer,Get_UValue=state,/No_Copy

  Widget_Control,ev.id,Get_UValue=uval
  event_type = Tag_Names(ev,/Struct)

  case uval[0] of
    'PREF':begin
      ; Map the preference window if valid or create the new window if not.
	state.PrefControl = ev.select
  	if Widget_Info(state.wEqPref,/Valid) then $
     	   Widget_Control,state.wEqPref,Map=1 $
	else begin
	   if event_type eq 'PSUDO_BUTTON_EVENT' then begin
	    ; Invoked by resizing of eq window, map only when it was on.
	    ; (Obselete)
	      epref = *state.ptr_eqpref
	   endif else begin
	    ; Invoked by preference button on the eq window
	          Forward_Function default_eq_pref
     	      epref = default_eq_pref()
	   endelse
	   wPrefBase = Widget_Base(/Column,Title='Equilibrium Preference',$
		Group_Leader=ev.top,/Floating,Tlb_Frame_Attr=2,$
		/Base_Align_Center)
	   wPref0Base = Widget_Base(wPrefBase,/Column,/Frame,$
			/Base_Align_Center)
	   wContourBase = Widget_Base(wPref0Base,/Column)
	   wcontourButton = CW_BGroup(wContourBase,$
		['psi','rho','mod Bp'],Label_Left='Contour of:',$
		Set_Value=epref.contour,/Exclusive,/No_Release,$
		/Row,UValue='CONTOUR',Event_Func='eq_pref_create_event')
           wPrefStruct = cw_struct(epref,wPref0Base,$
		Itags=[0,1,2],$				; see default_eq_pref
		Labels=['Number of contours',$
			'SOL contours - number',$
			'SOL contours - distance(m)'],$
		Field_Len=10,/Left,$
		func='wrap_eq_plot',editcallback='eq_pref_editcallback',$
		caller=viewer)
	   wSeparatrixBase = Widget_Base(wPref0Base,/Column)
	   wSeparatrixButton = CW_BGroup(wSeparatrixBase,$
		['Straight lines','Contoured lines'],Label_Left='Separatrix:',$
		Set_Value=epref.separatrix,/Exclusive,/No_Release,$
		/Row,UValue='SEPARATRIX',Event_Func='eq_pref_create_event')

	   wPrefButtonBase = Widget_Base(wPrefBase,/Row)
	   wPrefApplyButton = Widget_Button(wPrefButtonBase,Value=' Apply ',$
		UValue='APPLY',Event_Func='eq_pref_create_event')
	   Widget_Control,wPrefApplyButton,Sensitive=0
	   wPrefCancelButton = Widget_Button(wPrefButtonBase,Value=' OK ',$
		UValue='CANCEL',Event_Func='eq_pref_create_event')
	   Widget_Control,wPrefBase,/Realize,Pro_Set_Value='eq_pref_setv',$
		Func_Get_Value='eq_pref_getv',Map=ev.select,$
		Set_UValue={viewer:viewer,wPrefStruct:wPrefStruct,$
			    wContour:wContourButton,wApply:wPrefApplyButton,$
			    wSeparatrix:wSeparatrixButton}
     	   state.wEqPref = wPrefBase
  	endelse
	end
    'APPLY':begin				; alternative to returns
      ; Apply the preference.
	state = state.efit->Equilibrium(Info=state,machine=state.machine)
	Widget_Control,ev.id,Sensitive=0
	end
    'CANCEL':begin
      ; Apply the preference and UnMap the preference window
	state.PrefControl = 0
	state = state.efit->Equilibrium(Info=state,machine=state.machine)
	Widget_Control,ev.top,map=0
	end
    'CONTOUR':begin
      ; Invoked by the contour radio buttons
	state = state.efit->Equilibrium(Info=state,machine=state.machine)
	end
    'SEPARATRIX':begin
      ; Invoked by the separatrix radio buttons
	state = state.efit->Equilibrium(Info=state,machine=state.machine)
	end
     else: help,uval
  endcase ; uvalue

  Widget_Control,viewer,Set_UValue=state,/No_Copy

  return, 0
END ; eq_pref_create_event

;==============================================================================
PRO eq_pref_setv,wPrefBase,struct
;==============================================================================
;Message,'',/Informational
; 04-08-98
; set value to the preference window

  Widget_Control,wPrefBase,Get_UValue=uvalue

; Set the value to the cw_struct widget.
  Widget_Control,uvalue.wPrefStruct,Set_Value=struct

; Set the contour and separatrix button
  Widget_Control,uvalue.wContour,Set_Value=struct.contour
  Widget_Control,uvalue.wSeparatrix,Set_Value=struct.separatrix

END ; eq_pref_setv

;==============================================================================
FUNCTION eq_pref_getv,wPrefBase
;==============================================================================
;Message,'',/Informational
; 04-08-98
; get value from the preference window

  Widget_Control,wPrefBase,Get_UValue=uvalue

; Get the value to the cw_struct widget.
  Widget_Control,uvalue.wPrefStruct,Get_Value=struct

; Get the contour and the separatrix button values
  Widget_Control,uvalue.wContour,Get_Value=contour
  Widget_Control,uvalue.wSeparatrix,Get_Value=separatrix
  struct = Create_Struct(struct,'Contour',contour,'Separatrix',separatrix)

  return, struct
END ; eq_pref_getv

;==============================================================================
FUNCTION eq_overlay_create_event,ev
;==============================================================================
;Message,'',/Informational
  if getenv_('DEBUG') eq '' then catch, error_status else error_status=0
  if error_status ne 0 then begin
     if n_elements(state) then $
	Widget_Control,viewer,Set_UValue=state,/No_Copy
     ok = Dialog_Message(!Err_String,Dialog_Parent=ev.id)
     return, 0
  endif

; Invoked by OverlayButton. If overlay window widget state.wOverlay 
; is valid, put it on/off, otherwise, create one

  Widget_Control,/HOURGLASS
  Widget_Control,ev.top,Get_UValue=uvalue,/No_Copy
  viewer = uvalue.viewer				; tlb of efit_viewer
  Widget_Control,ev.top,Set_UValue=uvalue,/No_Copy
  Widget_Control,viewer,Get_UValue=state,/No_Copy

  Widget_Control,ev.id,Get_UValue=uval

  case uval[0] of
    'CANCEL': begin
	state.OverlayControl = 0
	Widget_Control,ev.top,Map=0
	if WIDGET_INFO(state.wOverlayOption,/VALID) then $
	   Widget_Control,state.wOverlayOption,Map=0
	end
    'LABELS': begin
	state.overlayannotate=ev.select
  	state = state.efit->Equilibrium(Info=state,machine=state.machine)	; replot
	end
    'OPTION': begin
	if WIDGET_INFO(state.wOverlayOption,/VALID) then begin
	   Widget_Control,state.wOverlayOption,MAP=1
	endif else begin
	 ; Create the overlay option window
	   wOverlayOptionBase = Widget_Base(/Column,$
		GROUP_LEADER=state.wOverlay,/FLOATING,TLB_FRAME_ATTR=2,$
		/Base_Align_Center,Title='Overlay Options')
	   wOverlayOptionBase0 = Widget_Base(wOverlayOptionBase,/Column)

	   wOverlayOptionBase1 = Widget_Base(wOverlayOptionBase0,/Row,/Frame,$
		Event_Func='eq_overlay_event')
	   wOverlayOptionButtons = CW_BGroup(wOverlayOptionBase1,$
		['alternative shot','as-run (default)','present','future'],$
		/EXCLUSIVE,/NO_RELEASE,Set_Value=1)
	   ; temporarily disable 'present' and 'future' buttons
	   Widget_Control,wOverlayOptionButtons+2+3,Sensitive=0
	   Widget_Control,wOverlayOptionButtons+2+4,Sensitive=0

	   wShotField = CW_Field(wOverlayOptionBase1,/Long,Title=' ',$
		Value='',XSize=6,/Return_Event)

	   wOverlayOptionBase2 = Widget_Base(wOverlayOptionBase0,/Col,/Frame,$
	   	Event_Func='eq_overlay_event')
	   wProButton = CW_BGroup(wOverlayOptionBase2,$
	   	['Other overlay, path and calling sequence:'],/NONEXCLUSIVE)
	   wProPathField = CW_Field(wOverlayOptionBase2,Title='path',XSize=40,$
           	/Return_Event)
	   wProCallField = CW_Field(wOverlayOptionBase2,Title='call',XSize=40,$
	   	/Return_Event)

	   ;; Disable other overlays when running on an IDL VM license 
           if (lmgr(/vm) gt 0) then widget_control,wOverlayOptionBase2,sensitive=0
	
	   wOverlayOptionBase3 = Widget_Base(wOverlayOptionBase,/Column)
	   wOverlayOptionDoneButton = Widget_Button(wOverlayOptionBase3,$
		Value=' Done ',UValue='OPTIONCANCEL',$
		Event_Func='eq_overlay_create_event')

     	   Widget_Control,wOverlayOptionBase,/REALIZE,$
		Set_UValue={viewer:viewer,wOptionChoice:wOverlayOptionButtons,$
		wShot:wShotField,wOtherChoice:wProButton,$
		wOtherPath:wProPathField,wOther:wProCallField} 
	   state.wOverlayOption = wOverlayOptionBase
	end
	end
    'OPTIONCANCEL':Widget_Control,ev.top,Map=0
    'OVERLAY': begin
	state.OverlayControl = ev.select
	if WIDGET_INFO(state.wOverlay,/VALID) then begin
         ; Put on Overlay control and window and its sub windows.
     	   Widget_Control,state.wOverlay,MAP=1,Get_UValue=wIds
         ; Hide or map sub-window of overlay window.
	   if struct_hastag(*state.overlay,'echres') then begin
     	     if (*state.overlay).echres and Widget_Info(wIds.wEch,/Valid) then $
	        Widget_Control,wIds.wEch,Map=ev.select
	     if (*state.overlay).ichres and Widget_Info(wIds.wIch,/Valid) then $
	        Widget_Control,wIds.wIch,Map=ev.select
     	     if (*state.overlay).mds_tang and Widget_Info(wIds.wMDS,/Valid) then $
	         Widget_Control,wIds.wMDS,Map=ev.select
	     if (*state.overlay).cerpaths and Widget_Info(wIds.wCER,/Valid) then $
	        Widget_Control,wIds.wCER,Map=ev.select
	     if (*state.overlay).cerpaths_main and Widget_Info(wIds.wCERM,/Valid) then $
                Widget_Control,wIds.wCERM,Map=ev.select
	     if (*state.overlay).coils and Widget_Info(wIds.wCoils,/Valid) then $
                Widget_Control,wIds.wCoils,Map=ev.select
             if (*state.overlay).nbiplot and Widget_Info(wIds.wNbi,/Valid) then $
                Widget_Control,wIds.wNbi,Map=ev.select
	     if (*state.overlay).eceoverlap and Widget_Info(wIds.wEceOverlap,/Valid) then $
                Widget_Control,wIds.wEceOverlap,Map=ev.select
	     if (*state.overlay).scaleiter and Widget_Info(wIds.wScaleIter,/Valid) then $
                Widget_Control,wIds.wScaleIter,Map=ev.select
	   endif
	 ; Put on overlay optionn window if settings are not default
	   if WIDGET_INFO(state.wOverlayOption,/VALID) then begin
	      Widget_Control,state.wOverlayOption,Get_UValue=optionuval,/NO_CO
     	      Widget_Control,optionuval.wOptionChoice,Get_Value=choice
	      IF choice NE 1 THEN Widget_Control,state.wOverlayOption,/Map $
	      ELSE BEGIN
	         Widget_Control,optionuval.wOtherChoice,Get_Value=choice
		 IF choice EQ 1 THEN Widget_Control,state.wOverlayOption,/Map
	      END
	      Widget_Control,state.wOverlayOption,Set_UValue=optionuval,/NO_CO
           endif
   	endif else begin
         ; Create overlay window
           DEVICE,GET_SCREEN_SIZE=screen
           wOB_title = 'Diagnostics and Overlays ('+state.machine+')'
           wOverlayBase = Widget_Base(/Column,GROUP_LEADER=ev.top,/FLOATING,$
    		    title=wOB_title,TLB_FRAME_ATTR=2,/Base_Align_Center,UVALUE=$
		    {viewer:viewer,wMDS:0L,wEch:0L,wIch:0L,wCER:0L,wCERM:0L,wNbi:0L,wCoils:0l,wEceOverlap:0l,wScaleIter:0l})
         ; find out what value to set initially
           tagnames = strlowcase(Tag_Names(*state.overlay))
           diagnoses_table,state.machine,name=button_uvalue,label=labels
	   if n_elements(labels) ne 0 then begin
              value = intarr(N_Elements(button_uvalue))
     	      for i=0, N_Elements(button_uvalue)-1 do begin
	         itag = where(tagnames eq button_uvalue(i),c)
	         if c ne 0 then value(i)=(*state.overlay).(itag(0))
              endfor
	      wDiagButtons = CW_BGroup(wOverlayBase,$
	   	   labels,UVALUE=button_uvalue,$
		   /NONEXCLUSIVE,/Frame,COLUMN=2,EVENT_FUNC='eq_overlay_event',$
		   BUTTON_UVALUE=button_uvalue,Set_Value=value)

	      wOverlayButtonBase = Widget_Base(wOverlayBase,/Row)
	      wOverlayCancelButton = Widget_Button(wOverlayButtonBase,$
	   	   Value=' Done ',UValue='CANCEL',$
		   Event_Func='eq_overlay_create_event')
	      wOverlayOptionButton = Widget_Button(wOverlayButtonBase,$
		   Value='More Options',UValue='OPTION',$
		   Event_Func='eq_overlay_create_event')

	      wAnnotateButton = CW_BGroup(wOverlayButtonBase,["Labels"],$
		   /NonExclusive,/Row,UValue=['LABELS'],Set_Value=1,$
		   Event_Func='eq_overlay_create_event')
           endif else begin
	      ; No overlays found for state.machine.  Display message.
              label = widget_label(wOverlayBase,value='No diagnostics found'+$
                          ' for machine="'+state.machine+'"   ') 	
	   end  

     	   Widget_Control,wOverlayBase,/REALIZE,Map=ev.select
           state.wOverlay = wOverlayBase		; store the widgetID
  	endelse
	end
     else:
  endcase

  Widget_Control,viewer,Set_UValue=state,/No_Copy
  return, 0
END ; eq_overlay_create_event


;==============================================================================
FUNCTION eq_overlay_event,ev
;==============================================================================
;Message,'',/Informational
; Invoked by selection on the overlay options
; overlay on the contour plots
; 02-20-98

  if getenv_('DEBUG') eq '' then catch, error_status else error_status=0
  if error_status ne 0 then begin
     if n_elements(state) then $
	Widget_Control,viewer,Set_UValue=state,/No_Copy
     if n_elements(wIds) then $
	Widget_Control,ev.top,Set_UValue=wIds,/No_Copy
     ok = Dialog_Message(!Err_String,Dialog_Parent=ev.id)
     return, 0
  endif

  Widget_Control,/HOURGLASS
; The uvalue of tlb of overlay window is the wid of the tlb of efit_viewer
  Widget_Control,ev.top,Get_UValue=wIds
  viewer=wIds.viewer
  Widget_Control,viewer,Get_UValue=state,/No_Copy

; return if contour window does NOT exist
  if not WIDGET_INFO(state.plots.(0),/VALID) then goto,done

; overlay

; store the overlay switch
  itag = where(tag_names(*state.overlay) eq strupcase(ev.value),count)
  if (count ne 0) and struct_hastag(ev,'select') then $
     (*state.overlay).(itag(0)) = ev.select
  
  if strmid(ev.value,0,4) eq 'mds_' then begin	; MDS tangential
    case ev.value of
    'mds_tang': Widget_Control,ev.id,EVENT_PRO='mds_create_event',$
	SEND_EVENT=Create_Struct(ev,'shot',state.efit->GetShot())
    'mds_new': mdsindex = indgen(10)+1
    'mds_old': mdsindex = indgen(10)+1
    'mds_new1': mdsindex = [2,3,4,5,6]
    'mds_new2': mdsindex = [4,5,6,9,10]
     else: if strlen(ev.value) eq 6 then mdsindex = long(strmid(ev.value,4,2))
    endcase
    if ev.value ne 'mds_tang' then begin
  ; update buttons and state.overlay
    Widget_Control,wIds.wBsingle,Get_Value=value,/No_Copy
    itag = where(tag_names(*state.overlay) eq 'MDS_01')
    for j=0,n_elements(mdsindex)-1 do begin
	index = mdsindex[j]-1
	value[index] = ev.select
	(*state.overlay).(itag[0]+index) = ev.select
    endfor
    Widget_Control,wIds.wBsingle,Set_Value=value,/No_Copy
    endif
  endif
; Plot everything to avoid using graphics function which generates
; unpredicatable color
  state = state.efit->Equilibrium(Info=state,machine=state.machine)

done:
  Widget_Control,viewer,Set_UValue=state,/No_Copy
  return, 0
END ; eq_overlay_event

;==============================================================================
PRO efit_viewer_setv,tlb,value
;==============================================================================
;Message,'',/Informational
; 05-15-98

  Widget_Control,tlb,Get_UValue=state,/No_Copy
  state.shot = value.shot
  state.time = value.time

; Read data
  a = reada(state.shot,state.time,/Exact,MKS=state.mdsplus_installed)
  g = readg(state.shot,state.time,/Exact)
  mse_calib_dir = getenv('MSE_CALIB_DIR')+'/'
  if file_test(mse_calib_dir) then begin
  	m = readm_m(state.shot,state.time,/Exact)
  endif else begin
	m = readm(state.shot,state.time,/Exact)
	m = Create_Struct(m,'vport',0) 
  endelse

; Store the new data in the state
	ng = N_Elements(*state.ptr_g)
	if not state.oplot or ((ng eq 1) $
	   and ((*(*state.ptr_g)[0]).error ne 0) $
	   and ((*(*state.ptr_a)[0]).error ne 0)) $
	then begin
	 ; Either single plot or the first valid data.
	 ; clean up first - do it later !!!
	   for i=0, ng-1 do begin
	       if Ptr_Valid((*state.ptr_a)[i]) then Ptr_Free,(*state.ptr_a)[i]
	       if Ptr_Valid((*state.ptr_g)[i]) then Ptr_Free,(*state.ptr_g)[i]
	   endfor
	   *state.ptr_a = Ptr_New(a)
	   *state.ptr_g = Ptr_New(g)
	   *state.ptr_m = m
	   *state.ptr_time = state.time
	endif else begin
	 ; Oplot.  Store the points to structure in arrays. 
	 ; (assume identical structures) and the latest is the first.
	   *state.ptr_a = [Ptr_New(a), *state.ptr_a]
	   *state.ptr_g = [Ptr_New(g), *state.ptr_g]
	   *state.ptr_m = m
	   *state.ptr_time = [state.time, *state.ptr_time]
	endelse
	; fake a ptr_picked
	*state.ptr_picked = {CW_EFITPICK_DUMMY,shot:state.shot}

; Put on plots
  all_plots,tlb,state
  
  Widget_Control,tlb,Set_UValue=state,/No_Copy

END ; efit_viewer_setv

;==============================================================================
FUNCTION efit_viewer_getv,tlb
;==============================================================================
;Message,'',/Informational
; Not needed. For the sake of completeness.
  return, 0
END ; efit_viewer_getv

;==============================================================================
;PRO overlay__define
FUNCTION overlay_construct,machine=machine
;==============================================================================

;; if machine is not specified, use default ('')
if not(keyword_set(machine)) then machine=''
diagnoses_table,machine,name=names
IF N_Elements(names) EQ 1 AND names[0] EQ '' THEN BEGIN
   ;overlay = {overlay,non:0}
   Return, {non:0}
ENDIF

cmd = "overlay = {"
FOR i=0,N_Elements(names)-1 DO $
  IF i EQ 0 THEN cmd = cmd + names[i] + ":0" $
            ELSE cmd = cmd + "," + names[i] + ":0"
cmd = cmd + "}"
overlay = create_struct(names(0),0)
FOR i=1,N_Elements(names)-1 DO overlay = create_struct(overlay,names(i), 0)

mds_struct = {$
	mds_new:0,mds_old:0,mds_new1:0,mds_new2:0,$
	mds_01:0,mds_02:0,mds_03:0,mds_04:0,$
	mds_05:0,mds_06:0,mds_07:0,mds_08:0,$
	mds_09:0,mds_10:0}

Return, Create_Struct(overlay,mds_struct)

END

;==============================================================================
FUNCTION efit_viewer,parent,nopicker=nopicker,_extra=e
;==============================================================================
;Message,'',/Informational

Forward_Function cw_efitpick, mdsplus_installed

  if getenv_('DEBUG') eq '' then catch, error_status else error_status=0
  if error_status ne 0 then begin
     ok = Dialog_Message(!Err_String)
     XManager
     return, 0
  endif

;------------------------------------------------------------------------------
; INITIALIZATION
;------------------------------------------------------------------------------
; set the device.
  if (!VERSION.OS eq 'MacOS') then set_plot,'mac' else set_plot,'x'

; set up color table
  color_setup,/Reverse

; supress math error message.
  !Except = 0

; Set the charsize for vector-drawn fonts so that it is platform independent.
; It takes effect only after a window has been created. (It is moved after 
; the color_setup which creates a pixmap window).
  Device,Set_Character_Size=[10,15]

  if N_Elements(shot) eq 0 then shot=0L
  if N_Elements(time) eq 0 then time=0.0

;------------------------------------------------------------------------------
; CREATE AND REALIZE THE WIDGETS
;------------------------------------------------------------------------------
  
; base widgets
  Widget_Control,DEFAULT_FONT=$
	"-misc-fixed-bold-r-normal--13-100-100-100-c-70-iso8859-1"
;	"-adobe-helvetica-bold-r-normal--17-120-100-100-p-92-iso8859-1"
  if N_Elements(parent) NE 0 then $
     wEfitViewBase = Widget_Base(parent,/COLUMN,_extra=e,$
	Event_Pro='efit_viewer_event') $
  else begin
     IF !Version.OS_Family EQ 'unix' THEN $
     Spawn,['uname','-n'],host,/NoShell ELSE host = !Version.OS
     wEfitViewBase = Widget_Base(TITLE='EFIT VIEWER',/COLUMN,$
		MBAR=wMBarBase,_extra=e,Event_Pro='efit_viewer_event')
     wFileMenu = Widget_Button(wMBarBase,VALUE='File',/MENU)
     printObj = Obj_New('GA_PRINTER',wEfitViewBase,Title='Printing All Active Plot Windows')
     wPrintMenu = Widget_Button(wFileMenu,Value='Print All',UVALUE=printObj,$
		Event_Func='efit_viewer_print_event')
     wQuitButton = Widget_Button(wFileMenu,VALUE='Quit',UVALUE='EXIT',$
		EVENT_FUNC='efit_viewer_quit_event')
     wHelpMenu = Widget_Button(wMBarBase,VALUE='Help',/HELP)
  endelse ; parent 

  if not Keyword_Set(nopicker) then begin
; efit picker
  wPickerBase = Widget_Base(wEfitViewBase,/COLUMN,UValue=wEfitViewBase)
  wPicker = cw_efitpick(wPickerBase,mode='File',types=['a','g'],$
		UVALUE='PICKER',HELP_EVENT=picker_help_event,shot=0, $
 		PATH=getenv("PWD"))

  IF N_Elements(parent) EQ 0 THEN BEGIN
  if KEYWORD_SET(picker_help_event) then $		; put on print help
  wAboutButton = Widget_Button(wHelpMenu,VALUE='About EFIT Viewer',$
                EVENT_PRO='efit_viewer_help_event',UValue='ABOUT')
  wHelpPickerButton = Widget_Button(wHelpMenu,VALUE='EFIT Picker',$
		EVENT_PRO=picker_help_event)
  wHelpGaPlotButton = Widget_Button(wHelpMenu,VALUE='Plot Object Window',$
		EVENT_PRO='efit_viewer_help_event',UVALUE='GAPLOT')
  printObj = Obj_New('GA_PRINTER',wEfitViewBase)
  wHelpGaPrintButton = Widget_Button(wHelpMenu,VALUE='Printing',$
		EVENT_PRO='efit_viewer_help_event',UValue=printObj)
  endif ; nopicker
  ENDIF ; no parent

; plot options
; wViewBase = Widget_Base(wEfitViewBase,/FRAME,/Column,UValue=wEfitViewBase)
  wViewTopBase = Widget_Base(wEfitViewBase,/Column,/Frame,UValue=wEfitViewBase)
  IF getenv_('DEBUG') THEN debug = 1 ELSE debug = 0
  theEfit = Obj_New('IEFIT',0,0,Debug=debug,/NoRead,/NonInteractive)
  wInteractive = LonArr(N_Elements(theEfit->GetPlotProperty(pages=-1)))
  wPlots = cw_page_select(theEfit->GetPlotProperty(pages=-1),wViewTopBase,$
	Title='Select Plots',/NonExclusive,Column=2,UValue=wEfitViewBase)

  wViewBottomBase = Widget_Base(wEfitViewBase,/Row,/Frame,UValue=wEfitViewBase)
  wOverlayButton = CW_BGroup(wViewBottomBase,['on','off'],$
		Label_Left='Slices Overlay',Set_Value=1,$
		/Exclusive,/No_Release,/Row,UValue='OPLOT')

  wViewRefBase = Widget_Base(wViewBottomBase,/Col,UValue=wEfitViewBase)
  wRefButton = Widget_Button(wViewRefBase,Value='Set as ref',$
	UValue='REF_ON',Event_Func='efit_viewer_ref_event')

  wRunOnBase = Widget_base(wEfitViewBase,/Col,UValue=wRunOnBase)
  wRunOn = Widget_Label(wRunOnBase,value='Running on: '+host[0])

  ;IF N_Elements(parent) EQ 0 THEN BEGIN
  ;  wRunBase = Widget_Base(wViewBottomBase,/Column,UValue=wEfitViewBase)
  ;  wRunButton = Widget_Button(wRunBase,Value=' Run EFIT ',$
  ;	UValue='RUNEFIT',Event_Func='efit_viewer_run_event')
  ;  IF (!VERSION.OS EQ "vms") OR (!VERSION.OS EQ "MacOS") $
  ;  THEN Widget_Control, wRunButton, SENSITIVE=0
  ;ENDIF
  ;widget_control,wRunButton,sensitive=0

;------------------------------------------------------------------------------
;  SAVE STATUS STRUCTURE AND ACTIVATE EVENT LOOP MANAGER
;------------------------------------------------------------------------------
  ptr_a = Ptr_New(/ALLOCATE_HEAP)
  ptr_g = Ptr_New(/ALLOCATE_HEAP)
  ptr_m = Ptr_New(/ALLOCATE_HEAP)
  ptr_picked = Ptr_New(/ALLOCATE_HEAP)
  *ptr_a = Ptr_New({error:1,shot:0L,time:0.0})	; no data yet
  *ptr_g = Ptr_New({error:1,shot:0L,time:0.0})
  *ptr_m = {error:1,shot:0L,time:0.0}

  mdsplus_installed = mdsplus_installed()
  IF StrUpcase(getenv_('MDSPLUS')) EQ "NO" THEN mdsplus_installed = 0

  state = { $
	efit:theEfit,$				; An IEFIT object.
	onewindow:0,$				; needed by cw_page_select
	mode:'',$				; file/mdsplus mode
	run:'',$				; efit runid for MDSplus
	shot:shot,$
	time:time,$
        machine:'',$     
	mdsplus_installed:mdsplus_installed,$	; whether mdsplus exists
	ptr_time:Ptr_New(time),$		; pointer to time array
	ptr_picked:ptr_picked,$			; pointer to ev structure
						; returned by efit_picker
	nwindow:5,$				; number of plot windows
	x:'r',$					; xcoord of plots - r, rho, psi
	plot2:['J','P','Er','q','Bz','BzEr'],$	; plot names
	plot3:['ffprime','pprime','shear','Br','mse','Gamma'],$
	OverlayControl:0,$			; overlay control on/off
	overlay:Ptr_New(overlay_construct()),$  ; pointer to overlay switches
	overlayannotate:1,$			; overlay labels on/off switch
	oplot:0,$				; shot overlay switch
	PrefControl:0,$				; eq window pref control on/off
	ptr_eqpref:Ptr_New(/Allocate_Heap),$	; ptr to epref struct
	$					; widget IDs of
	wAnimate:0L,$				; wId of the animation widget
	wInteractive:wInteractive,$		; wIds of the plot windows.
	wPlotBase:wViewTopBase,$		; base that hold plot selects.
	wPlot:wPlots,$				; plot select base
	wRunEFIT:0L,$				; run efit base
	wOverlay:0L,$				; overlay base, set later
	wOverlayOption:0L,$			; overlay option base
	wEqPref:0L,$				; eq preference button
	wPara:0L,$				; text widget for parameters
	plots:{ plot_select,$			; widget Ids of
		id0:0L,$			;   contour plot window
		id1:0L,$			;   fitting quality window
		id2:0L,$			;   profiles 1 (MSE) window
		id3:0L,$			;   profiles 2 (ER) window
		id4:0L,$			;   pointnames window
		$				; draw widget Ids of
		w0:0L,$				;   contour plot
		w1:0L,$				;   fitting quality
		w2:lonarr(3),$			;   profiles 1 - r, rho, psi
		w3:lonarr(3)}, $		;   profiles 2 - r, rho, psi
	$
	ptr_a:ptr_a,$				; a pointer to A structures
	ptr_g:ptr_g,$				;	       G
	ptr_m:ptr_m $				;	       mse
	}

  Widget_Control,wEfitViewBase,Set_UValue=state,/No_Copy,$
	Pro_Set_Value='efit_viewer_setv'

  ; If top level widget, starts its own xmanager
  if N_Elements(parent) EQ 0 then begin
     Widget_Control,wEfitViewBase,/Realize
     XManager,'efit_viewer',wEfitViewBase,EVENT_HANDLER='efit_viewer_event'
  endif

  return, wEfitViewBase
END ; efit_viewer
