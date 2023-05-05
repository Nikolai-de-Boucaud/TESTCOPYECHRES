;************************************************************************
;+
; NAME:
;	CW_POINTNAME
;
; PURPOSE:
;	Creates a compound widget to view the time trace of 1-D EFIT 
;	data from A file or MDSplus equivalent.
;
; CATEGORY:
;	Widget
;
; CALLING SEQUENCE:
;	CW_POINTNAME,shot
;
; INPUTS:
;	shot	The shot number (optional).
;	parent	The widget ID of the parent (optional).  A top level 
;		widget is created if there is no parent.
;
; KEYWORD PARAMETERS:
;	wid		The widget ID returned by this function.
;			If passed in and valid, the widget will be mapped
;			instead of being created.
;	group		The group leader.
;	caller		The ID of a widget that holds information. (Not used) 
;	cancel_event	The event the 'Done' button calls.  Exit by default.
;	_extra		The extra keywords that are appliable to the top
;			or first level widget_base.
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
;	Calling the cw_pointname only creats the widget.  The plot 
;	has to be put on by setting the value.
;
; PROCEDURE:
;	The plot widget is created using the plot object GA_PLOT with
;	blank data.  The name and the decription of the variables are 
;	read from MDSplus and listed on the side.  Data are read in 
;	the setting value routine.
;
;	Event handling routings include
;
;	PRO 	 cw_pointname_event	 - handles variable selection
;	FUNCTION cw_pointname_quit_event - exit. replaceble by cancel_event
;	PRO 	 cw_pointname_setv	 - takes in new shot and reads the data
;	The value is of structure {shot:0L,oplot:0,mode:'',run:'',path:''}
;	where mode, run and path are those returned by efit picker.
;
; EXAMPLE:
;	wId = cw_pointname()
;	Widget_Control,wId,Set_Value=$
;		{shot:shot,oplot:0,mode:'FILE',run:'',path='./'}
;	see EFIT_VIEWER
;
; HISTORY:
;	03-15-98 Q. Peng first created
;	04-27-98 Q. Peng added window resizing. Instead of reading A files 
;		 when the widget is created, they are read only when
;		 a variable is selected.  Fixed a bug when there is no A file.
;	05-19-98 Q. Peng replace getenv with CMG's getenv_ for MacOS.
;	05-27-98 Q. Peng re-read data to get the lastest updates whenever 
;		 the value is set even it is same as the previous one.
;	10-06-98 Q. Peng replaced the old ga_plot objects with the new
;		 version. Rewrote the resizing.
;	02-08-99 Q. Peng If MDSplus is not installed, get the a varilable
;		 names and descriptions from a pre-saved file 'anames.sav'.
;		 The file is saved at DIII-D site and transferred.
;	01-12-2001 Q.P. Use MDSPLUS environment variable in conjunction with
;		 mdsplus_installed to decide whether to use MDSplus or not.
;-
;************************************************************************

;==============================================================================
FUNCTION cw_pointname_readfile,state
;==============================================================================
; 4-27-98 created for reading data from A files. Used by both event handler
;	  and the set value routine.

; Read the A files
  afiles = getafiles(state.shot,ierr,mode=state.mode,$
		run=state.run,path=state.path)
  if ierr then begin
     state.error = 1
     state.msg = "There isn't any A file for shot"+strcompress(state.shot)+'.'
     ok = Dialog_Message(state.msg,Dialog_Parent=state.wPointBase)
     Return, 1
  endif

; if not value.oplot then *state.ptr_a = afiles $
;	             else *state.ptr_a = [afiles,*state.ptr_a]
  *state.ptr_a = afiles
  n = n_elements(afiles)
  time = fltarr(n)
  for i=0,n-1 do time[i]=(afiles[i]).time
; if not value.oplot then *state.ptr_time = time $
;		     else *state.ptr_time = [Ptr_New(time),state.ptr_time]
  *state.ptr_time = time

  state.error = 0
  Return, 0
END ; cw_pointname_read

;==============================================================================
PRO CW_Pointname_Print,top
;==============================================================================
  if getenv_('DEBUG') eq '' then catch, error_status else error_status=0
  if error_status ne 0 then begin
     if n_elements(state) then $
	Widget_Control,top,Set_UValue=state,/No_Copy
     ok = Widget_Message(!Err_String)
     return
  endif

Widget_Control,top,Get_UValue=state,/No_Copy
IF Obj_Valid(state.window) THEN state.window->Draw
Widget_Control,top,Set_UValue=state,/No_Copy
END

;==============================================================================
FUNCTION CW_Pointname_Print_Event,ev
;==============================================================================
Widget_Control,ev.id,Get_UValue=printObj
printObj->DialogPrint,'cw_pointname_print',ev.top
END

;==============================================================================
PRO cw_pointname_event,ev
;==============================================================================
;Message,'',/Informational

  if getenv_('DEBUG') eq '' then catch, error_status else error_status=0
  if error_status ne 0 then begin
     if n_elements(state) then $
	Widget_Control,ev.top,Set_UValue=state,/No_Copy
     ok = Widget_Message(!Err_String)
     return
  endif

  WIDGET_CONTROL,/HOURGLASS
  WIDGET_CONTROL,ev.top,GET_UVALUE=state,/NO_COPY  

  event_type = TAG_NAMES(ev,/STRUCTURE_NAME)
  case event_type of
    'WIDGET_LIST': begin
      ;------------------------------------------------------------------------
      ; Invoked by a click on the list of the variable names
      ;------------------------------------------------------------------------
	WIDGET_CONTROL,ev.id,GET_UVALUE=uvalue,/NO_COPY
      ; find out the name of the variable to plot
	state.index = ev.index
	name = uvalue[ev.index]
	WIDGET_CONTROL,ev.id,SET_UVALUE=uvalue,/NO_COPY
      ; put on the brief description for the variable
	WIDGET_CONTROL,state.wStatus,SET_VALUE=(state.names.labels)[ev.index]
      ; If state.error is recorded with no data, return
        if state.error then begin
	   ok = Dialog_Message(state.msg,Dialog_Parent=state.wPointBase)
	   goto, done
	endif
	if state.mode eq 'FILE' then begin
	 ; Read data if the first time
	   if N_Elements(*state.ptr_a) eq 0 then begin
	      ierr = cw_pointname_readfile(state)
	      if ierr then goto, done
	   endif
	 ; Convert from MDSplus name to File name
	   aname = (state.names.reada)[ev.index]
	   tagnames = TAG_NAMES(((*state.ptr_a)[0]).d)
	   itag = where(tagnames eq aname,count)
	   if count gt 0 then begin
	    ; extract the values of the variable
	      status = 1
	      n = n_elements(*state.ptr_a)
	      var = fltarr(n)
	      for i=0, n-1 do var[i]=(((*state.ptr_a)[i]).d).(itag(0))
	   endif else status = 0
	endif else begin
	 ; MDSPLUS mode, retrive variable as function of time individually.
	   mdsopen,state.run,state.shot,/quiet,status=status
	   if status then begin
	      tag = state.machine_tag + name
	      var = mdsvalue(tag)
	      *state.ptr_time = mdsvalue('DIM_OF('+tag+')',$
				status=status,/quiet)
	   endif
	endelse
	if status then begin
	 ; Put on the plot
	   state.plotObj->Set_Data_Property,XData=*state.ptr_time,YData=var,$
		/AutoRange,Color=color_index('Blue')
	   state.plotObj->Set_Plot_Property,$
		Title=name+' of'+strcompress(state.shot),$
		xtitle='time (msec)',ytitle=(state.names.units)[ev.index]
	   state.window->Draw
	   state.window->Copy_To_Pixmap
	endif
	end
    'WIDGET_BASE': begin
      ;------------------------------------------------------------------------
      ; Window Resize
      ;------------------------------------------------------------------------
	newXsize = (ev.x - Total(state.xsizes)) > 1
	newYsize = (ev.y - Total(state.ysizes)) > 1
	Widget_Control,ev.top,Update=0
	FOR i=0, N_Elements(state.xbases)-1 DO $
   	   IF Widget_Info(state.xbases[i],/Valid) THEN $
      	      Widget_Control,state.xbases[i],XSize=state.xsizes[i],$
		YSize=newYsize
	FOR i=0, N_Elements(state.ybases)-1 DO $
   	   IF Widget_Info(state.ybases[i],/Valid) THEN $
      	      Widget_Control,state.ybases[i],YSize=state.ysizes[i]
	state.window->Resize,newXsize,newYsize
	Widget_Control,ev.top,Update=1
	state.window->Draw
	state.window->Copy_To_Pixmap
        end
;    'CW_EFITPICK_SERVER': begin
;      ;------------------------------------------------------------------------
;      ; MDSplus server changed, update anames list
;      ;------------------------------------------------------------------------
;        help,ev,/st
;        machine_tag = '\'
;        IF state.mdsplus_installed THEN BEGIN
;           anames = mds_efit_labels('a')
;           IF mdsvalue('MACHINE()') EQ 'CMOD' THEN machine_tag = '\efit_aeqdsk:'
;        ENDIF ELSE restore,getenv_('EFITVIEW')+'anames.sav'
;        state.machine_tag = machine_tag
;        state.names = Temporary(anames)
;        end
     else:
  endcase

done:
  if WIDGET_INFO(ev.top,/VALID) then $
  WIDGET_CONTROL,ev.top,SET_UVALUE=state,/NO_COPY  

  return
END ; cw_pointname_event

;==============================================================================
FUNCTION cw_pointname_quit_event,ev
;==============================================================================
;print,'cw_pointname_quit_event'

  if getenv_('DEBUG') eq '' then catch, error_status else error_status=0
  if error_status ne 0 then begin
     return, 0
  endif

; The default quit event invoked by Cancel button, if keyword cancel_event 
; was not set when the cw_pointname widget was created.
; Clean up before exit.
  WIDGET_CONTROL,ev.top,GET_UVALUE=state,/NO_COPY
  if PTR_VALID(state.ptr_a) then PTR_FREE,state.ptr_a
  if PTR_VALID(state.ptr_time) then PTR_FREE,state.ptr_time
  WIDGET_CONTROL,ev.top,/DESTROY

  return,0
END ; cw_pointname_quit_event

;==============================================================================
PRO cw_pointname_setv,tlb,value
;==============================================================================
;print,'cw_pointname_setv'

; Set_Value
; The value is of structure {shot:shot,oplot:oplot,mode:mode,run:run,path:path}

  WIDGET_CONTROL,tlb,GET_UVALUE=state,/NO_COPY  

; Return if inputs are same as the current to avoid re-read for same shot,
; but different time.
;  if value.shot eq state.shot and value.mode eq state.mode and $
;     value.run eq state.run and string(value.path) eq state.path then begin
;	Widget_Control,tlb,Set_UValue=state,/No_Copy
;	return
;  endif

; Store shot, mode, data and time 

  state.shot = value.shot
  state.mode = value.mode
  state.run = value.run
  state.path = string(value.path)
  state.oplot = value.oplot

  index = state.index

; If the index is real (Index is initialized to -1 before any selection),
; read from files.  If in MDSplus mode, .
; reading from mdsplus is handled later when the variable is selected.
  if index ge 0 and value.mode eq 'FILE' then begin
     ierr = cw_pointname_readfile(state)
     if ierr then begin
        WIDGET_CONTROL,tlb,SET_UVALUE=state,/NO_COPY  
	Return
     endif
  endif

  wName = state.wName
  WIDGET_CONTROL,tlb,SET_UVALUE=state,/NO_COPY  

; Send a psudo event to plot the new data of the selected variable
; if it is a real index.  Index is initialized to -1 before any selection.
  if index ge 0 then $
  WIDGET_CONTROL,wName,SET_LIST_SELECT=index,$
	SEND_EVENT={WIDGET_LIST,$
	ID:wName,Top:tlb,Handler:0L,Index:index,Clicks:1L}

END ; cw_pointname_setv

;==============================================================================
FUNCTION cw_pointname,shot,run,parent,wid=wid,group=group,caller=caller,$
		      page=page,cancel_event=cancel_event,_extra=e
;==============================================================================

Forward_Function mdsplus_installed, getenv_

  mdsplus_installed = mdsplus_installed()
  IF StrUpcase(getenv_('MDSPLUS')) EQ "NO" THEN mdsplus_installed = 0

; Check keywords
  if not KEYWORD_SET(wid) then wid=long(0)
  if not KEYWORD_SET(group) then group=long(0)
  if not KEYWORD_SET(caller) then caller=long(0)
  if N_Elements(page) EQ 0 THEN page = -1
  if not KEYWORD_SET(cancel_event) then $
			cancel_event='cw_pointname_quit_event'

; Invoked by Pointname button of the main menu. 

  WIDGET_CONTROL,/HOURGLASS
  
  if WIDGET_INFO(wid,/VALID) then begin		; put on/off PointNameB
     WIDGET_CONTROL,wid,MAP=1
     return,wid
  endif

  if N_Elements(shot) eq 0 then shot=0L

; Create the pointname widgets

  DEVICE,GET_SCREEN_SIZE=screen
  
  if KEYWORD_SET(parent) then $
  wPointNameBase = WIDGET_BASE(parent,/COLUMN,TITLE='EFIT Pointnames',$
	GROUP_LEADER=group,YOFFSET=screen(1)/2,/TLB_SIZE_EVENT,_extra=e) else $
  wPointNameBase = WIDGET_BASE(/COLUMN,TITLE='EFIT Pointnames',$
	GROUP_LEADER=group,YOFFSET=screen(1)/2,/TLB_SIZE_EVENT,$
	MBar=wMenuBase,_extra=e)

   ; File Menu

  IF N_Elements(parent) EQ 0 THEN BEGIN
     wFileMenu = Widget_Button(wMenuBase,Value='File')
     printObj = Obj_New('GA_Printer',wPointNameBase,Title='Printing')
     wPrintMenu = Widget_Button(wFileMenu,Value='Print',UValue=printObj,$
		Event_Func='cw_pointname_print_event')
     wCloseMenu = Widget_Button(wFileMenu,Value='Close',$
		UValue='CW_POINTNAME_CANCEL',Event_Func=cancel_event)
  ENDIF

  wStatusBase = WIDGET_BASE(wPointNameBase,/ROW)
  wPointBase = WIDGET_BASE(wPointNameBase,/ROW)

; get the names of A
  machine_tag = '\'
  anames = mds_efit_labels('a',shot,run)
  if anames.status eq 0 then restore,getenv_('EFITVIEW')+'/anames.sav' 
  atags = anames.signals
  wNameBase = Widget_Base(wPointBase,/Row,YSize=400)
  wNameList = WIDGET_LIST(wNameBase,VALUE=atags,/FRAME,$
		XSIZE=8,UVALUE=atags)

  wLabel = WIDGET_LABEL(wStatusBase,VALUE='Description:')
  wStatusText = WIDGET_TEXT(wStatusBase,SCR_XSIZE=350,$
		VALUE='No variable currently selected.')
  wCursorText = Widget_Text(wStatusBase,Value='',Xsize=20)

   ; Create the plot window.

wGa_Plot = CW_GAWindow(wPointBase,XSize=500,YSize=400,$
	Color=color_index('Red'),Grid=[1,1],/NoPreferences,/MBar,/Debug)
Widget_Control,wGa_Plot,Get_Value=obj
obj->SetColorFlag,1,/Reverse
obj->Set_Status_Window,wCursorText
obj->Set_Crosshairs,1
obj->SetCurrentTool,'ZOOM'
plotObj = Obj_New('GA_PLOT',[0.,0.],[0.,0.],$
	Color=color_index('Blue'),Background=color_index('Background'))
plotObj->Set_Data_Property,YName=''
obj->Add_Plot,plotObj

statusGeom = Widget_Info(wStatusBase,/Geometry)
ybases = [wStatusBase]
ysizes = [statusGeom.ysize]
nameGeom = Widget_Info(wNameBase,/Geometry)
xbases = [wNameBase]
xsizes = [nameGeom.xsize]

  ptr_a = PTR_NEW(/ALLOCATE_HEAP)
  ptr_time = PTR_NEW(/ALLOCATE_HEAP)

  WIDGET_CONTROL,wPointNameBase,/REALIZE

  Widget_Control,wPointNameBase,Tlb_Get_Size = wsize
  state = {$
	   window:obj,$				; Pointer to Ga_Plot window.
	   plotObj:plotObj,$			; Pointer to Ga_Plot obj.
	   shot:shot,$
	   mode:'',$
	   run:'',$
	   path:'',$
	   ptr_time:ptr_time,$
	   ptr_a:ptr_a,$
	   names : anames,$
	   mdsplus_intalled : mdsplus_installed,$ ; whether mdsplus exists.
           machine_tag: machine_tag,$
	   wPointBase:wPointBase,$		; used for resizing
	   wStatus:wStatusText,$
;	   wDraw:wDrawBase,$
	   wName:wNameList,$
	   wCaller:Caller,$
	   wsize:wsize,$			; widget size
	   xbases:xbases,$			; keep xsize if resize
	   ybases:ybases,$			; keep ysize if resize
	   xsizes:xsizes,ysizes:ysizes,$	; x,ysizes of x,ybases
	   efitpage:page,$			; The page number of efit plots
	   index:-1L,$
	   oplot:0,$
	   error:0,$				; error eg. no data
	   msg:''}				; error message

  WIDGET_CONTROL,wPointNameBase,SET_UVALUE=state,/NO_COPY,$
	Pro_Set_Value='cw_pointname_setv'
  XMANAGER,'cw_pointname',wPointNameBase,EVENT_HANDLER='cw_pointname_event',$
	/No_Block

  return, wPointNameBase
END ; cw_pointname
