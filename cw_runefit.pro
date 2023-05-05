;==============================================================================
PRO CW_RunEFIT_SetV,top,value
;==============================================================================

   ; Get info.

Widget_Control,top,Get_UValue=state,/No_Copy

   ; Set value.

Widget_Control,state.wInput,Get_Value=input
input.shot = value.shot
input.start = value.time
Widget_Control,state.wInput,Set_Value=input

Widget_Control,top,Set_UValue=state,/No_Copy
END ; cw_runefit_setv

;==============================================================================
PRO CW_RunEFIT_Event,ev
;==============================================================================

   ; Get info.

sid = Widget_Info(ev.id,/Parent)
Widget_Control,sid,Get_UValue=top
Widget_Control,top,Get_UValue=state,/No_Copy

Widget_Control,ev.id,Get_UValue=uvalue
CASE uvalue[0] OF
  'OK':BEGIN						; Spawn an efit. 
      ; Use status label instead of pop-up window to display message.
	;wid = cw_process('Spawning EFIT',Group_Leader=ev.top)
	Widget_Control,state.wStatus,Set_Value='Spawning EFIT ...'
	Widget_Control,state.wInput,Get_Value=input
      ; Change the diretory
	z = file_search(input.dir,count=c)
	IF c EQ 0 THEN BEGIN
	 ; invalid directory, pop up the pickdir widget.
	   input.dir = pickdir(Title='Invalid directory - Pick a directory:',$
			Group=top,/Floating)
	   if input.dir eq '' then input.dir = getenv_('PWD')
	   Widget_Control,state.wInput,Set_Value=input
	ENDIF
      ; check the total number of steps - limit to 200
	if input.nstep gt 200 then begin
	   ok = Widget_Message('Number of time steps must be <= 200')
	   Widget_Control,state.wStatus,Set_Value=''
	   goto, skip
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

      ; Upmap the widget if no parent.

	;Widget_Control,wid,Set_Value=''
	Widget_control,state.wStatus,Set_Value=''
        IF NOT state.parent THEN Widget_Control,top,map=0
	END
  'CANCEL':BEGIN				; Upmap widget w/o spawn efit.
	Widget_Control,top,Map=0
	END
   ELSE:help,uvalue
ENDCASE

skip:
Widget_Control,top,Set_UValue=state,/No_Copy
END ; cw_runefit_event

;==============================================================================
FUNCTION CW_RunEFIT, parent,shot=shot,time=time,_Extra=extra
;==============================================================================

   ; Check keywords.

IF N_Elements(shot) EQ 0 THEN shot = 0L
IF N_Elements(time) EQ 0 THEN time = 0D

   ; Create the first base.

IF N_Elements(parent) EQ 0 THEN BEGIN

   ; Top level widget.

   wRunEFITBase = Widget_Base(Title='Run EFIT',/Column,$
	/Base_Align_Center,_Extra=extra,Event_Pro='cw_runefit_event')

ENDIF ELSE BEGIN
 
   ; Embedded in another widget.

   wRunEFITBase = Widget_Base(parent,/Column,$
	/Base_Align_Center,_Extra=extra,Event_Pro='cw_runefit_event')

ENDELSE


input = {dir:getenv_('PWD'),$
	shot:shot,start:time,step:100,nstep:1,snap:'mses'}

wInputStruct = cw_struct(input,wRunEFITBase,$
	Labels=['directory',$
		'shot number',$
		'start time (ms)',$
		'time step (ms)',$
		'number of time slices',$
		'snap extension'],$
	/Left,Field_Len=25,/Frame)

wRunBase = Widget_Base(wRunEFITBase,/Row,UValue=wRunEFITBase)
IF N_Elements(parent) EQ 0 THEN BEGIN
   wRunCancelButton = Widget_Button(wRunBase,Value=' Cancel ',$
	UValue='CANCEL');,Event_Pro='cw_runefit_event')
   wRunOKButton = Widget_Button(wRunBase,Value=' OK ',$
	UValue='OK');,Event_Pro='cw_runefit_event')
ENDIF ELSE $
   wRunOKButton = Widget_Button(wRunBase,Value=' RUN ',$
	UValue='OK');,Event_Pro='cw_runefit_event')

wStatusLabel = Widget_Label(wRunBase,Value='',/Dynamic_Resize)

Widget_Control,wRunEFITBase,$
	Pro_Set_Value='cw_runefit_setv',$
	Set_UValue={wInput:wInputStruct,parent:N_Elements(parent),$
		    wStatus:wStatusLabel}

IF N_Elements(parent) EQ 0 THEN BEGIN
   Widget_Control,wRunEFITBase,/Realize
   XManager,'cw_runefit',wRunEFITBase,Event_Handler='cw_runefit_event'
ENDIF

Return, wRunEFITBase
END 
