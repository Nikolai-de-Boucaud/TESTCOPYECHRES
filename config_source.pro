; config_source.pro

pro config_source_event, ev
common ECHCOM
 
catch, an_error
if an_error ne 0 then begin
	print, '  !!! config_source_event: error: ', !err_string
	PrgDat.Status.OutputsValid=0
	return
endif

widget_control, ev.top, get_uvalue=info
widget_control, ev.id, get_uvalue=wid
case wid of	
	'CANCEL':
	'OK': begin
	
		id=widget_info(ev.top, /child) + 4	; get the config-shot data
		widget_control, id, get_value=cshot
		goto, do_setupshot 
		
	end
	
	'ReadSetupFile': begin	; new setup file picked
	
		Clear_Com	; clear tables for new input
		widget_control, info.WidIDs.ConfigTable, set_value=ECHSys.Tank
		widget_control, info.WidIDs.InputTable, set_value=ECHSys.InputTable
		
		; read the archive
		setupdata=read_setup_file(setup_dir=PrgDat.Setup_Dir)
		if not setupdata.success then message, 'Did not get setup file'
		PrgDat.Setup_Dir=setupdata.setup_dir
		PrgDat.Prefs.nlinputfile=''
		PrgDat.config_shot=setupdata.shot
		widget_control, info.WidIDs.UsePresent, set_value=0
		PrgDat.Status.ArchivedData=0
		widget_control, info.WidIDs.Archive, set_value=PrgDat.Status.ArchivedData
		
		configdata=resolve_setup(setupdata) 
		if configdata.success then fill_com, configdata, success=success else $
			message, 'resolve_setup failed'
		if not success then message, 'fill_com failed'
	
		; put data into the inputs table
		widget_control, info.WidIDs.ConfigTable, set_value=ECHSys.Tank
		
		; designate the outputs table as invalid
		PrgDat.Status.OutputsValid=0
		;widget_control, info.WidIDs.OutsValid, set_value=PrgDat.Status.OutputsValid
		
		; set the likely antenna numbers for ray tracing
		widget_control, info.WidIds.RayTrace, set_value=PrgDat.Status.PlotRays
		
		if PrgDat.config_shot gt PrgDat.first_shot_newest_config then $
			widget_control, info.WidIDs.UsePresent, set_value=1
			
	end

	'SetupShot': begin	; new MDSplus setup shot picked
	
		widget_control, ev.id, get_value=cshot
		PrgDat.Config_Shot=cshot

		do_setupshot:
		
		Clear_Com	; clear tables for new input
		widget_control, info.WidIDs.ConfigTable, set_value=ECHSys.Tank
		widget_control, info.WidIDs.InputTable, set_value=ECHSys.InputTable

		read_MDS_setup, PrgDat.config_shot, success=success
		
		if success then begin
			; set ArchivedData button to reflect the success of the read
			widget_control, info.WidIDs.Archive, set_value=PrgDat.Status.ArchivedData
			
			if PrgDat.Status.ArchivedData EQ 1 then begin
			
				; put data into the inputs table
				widget_control, info.WidIDs.ConfigTable, set_value=ECHSys.Tank
				
				; designate the outputs table as invalid
				PrgDat.Status.OutputsValid=0
				;widget_control, info.WidIDs.OutsValid, set_value=PrgDat.Status.OutputsValid
				widget_control, info.WidIDs.Eqb, set_value= $
					string(PrgDat.Shot, format='(i6.6)') + '.'+ $
					string(PrgDat.Time, format='(i5.5)')
					
				; set the likely antenna numbers for ray tracing
				widget_control, info.WidIds.RayTrace, set_value=PrgDat.Status.PlotRays
				
				; clear the profiles and widgets
				PrgDat.Profiles.CentralDensity=0.0
				PrgDat.Profiles.enein[*]=0.0
				PrgDat.Profiles.InputDensity=0.0
				PrgDat.Profiles.CentralTe=0.0
				PrgDat.Profiles.tein[*]=0.0
				PrgDat.Profiles.InputTe=0.0
	
				; put the calculated values into the output table:
				widget_control, info.WidIDs.InputTable, set_value=ECHSys.InputTable
				widget_control, info.WidIDs.Eqb, $
					set_value=string(PrgDat.Shot)+strmid(string(PrgDat.Time/100000.), $
					strpos(string(PrgDat.Time/100000.),'.'),6)
				widget_control, info.WidIDs.CentralDensity, set_value=PrgDat.Profiles.CentralDensity
				widget_control, info.WidIDs.CentralTe, set_value=PrgDat.Profiles.CentralTe
				
				; set the output data button to verified
				PrgDat.Status.OutputsValid=0
				;widget_control, info.WidIds.OutsValid, set_value=PrgDat.Status.OutputsValid
				

			endif
			
		endif
		
	end
	else: 
endcase

; Replot the rays
PrgDat.Status.recalc_rays[*]=1

widget_control, ev.top, /destroy

return

end

;************************************************
pro config_source, info
common ECHCOM

if xregistered('config_source') then return

catch, an_error
if an_error ne 0 then begin
	print, '  !!! config_source_event: error: ', !err_string
	PrgDat.Status.OutputsValid=0
	return
endif

wBase=widget_base(/column, title='Configuration Source')
wBaseButtons=Widget_Base(wBase, /column)
wDuymmy=cw_field(wBaseButtons, title='MDS config not available for shot ', $
	value=PrgDat.shot, /integer, /noedit)
wSetupShot=cw_field(wBaseButtons, title='Use MDS config from shot ', $
	value=PrgDat.Config_Shot, uvalue='SetupShot', /long, /return_events)
wReadSetupFile=widget_button(wBaseButtons, value=' Pick a setup file ', $
	uvalue='ReadSetupFile')
wOK=widget_button(wBaseButtons, value=' OK ', uvalue='OK')
wCancel=widget_button(wBaseButtons, value=' CANCEL ', uvalue='CANCEL')

widget_control, wBase, /realize, set_uvalue=info

xmanager, 'config_source', wBase

end
