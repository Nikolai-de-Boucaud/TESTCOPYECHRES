pro picker_event,ev

	; Retrieve the name of the event
	event_name = TAG_NAMES(ev,/STRUCTURE_NAME)

	; MDSplus mode event (once a run & time have been selected)
	if (event_name eq 'CW_EFITPICK_MDSTIME') then begin

		run  = ev.run[0]
		shot = ev.shot[0] 
		time = ev.time[0]
	        a = reada(shot,time,Mode='MDSPLUS',RunId=run)
                g = readg(shot,time,Mode='MDSPLUS',RunId=run)

	; Ignore all other efitpicker events in this example
	endif else begin
		print,'Event '+event_name+' will not be handled in this example.'
	endelse

end

pro test_efitpicker

  	; Restore the efitpicker & create the main window
  	restore,'cw_efitpick.compile'
  	wMainBase = Widget_Base(TITLE='Efitpicker Test Example',/COLUMN,_extra=e,Event_Pro='picker_event')

  	; Create the efitpicker
  	wPickerBase = Widget_Base(wMainBase,/COLUMN,UValue=wMainBase)
  	wPicker = cw_efitpick(wPickerBase,mode='File', types=['a','g'],		$
                              UVALUE='PICKER', shot=0, PATH=getenv("PWD"))

  	; Display the window and connect all events to the event handler
  	Widget_Control,wMainBase,/Realize
  	XManager,'efit picker',wMainBase,EVENT_HANDLER='picker_event'

end

