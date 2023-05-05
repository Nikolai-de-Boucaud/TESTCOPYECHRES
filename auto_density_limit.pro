;subroutine based on what was wroten by Niko de Boucaud: deboucaudn@fusion.gat.com on Feb 22, 2023
;automatically finds the density limit when reflected ECH power (30rays as default) hits one of the ECH port 
;regardless of total absorption

PRO auto_density_limit_EVENT, event

	Common ECHCOM
	Common REFLECTION_CHECK
	Common DENS_LIM_AUTO
	Common DensWidIDs
	Common AUTOMATION
	Common AutoWidIDs

	;Use "uval" to determine which event happened
    WIDGET_CONTROL,event.id,GET_UVALUE=uval

	;Get certain widget information from "auto_stash" struct
    WIDGET_CONTROL, event.TOP, GET_UVALUE=auto_stash

	;Get the IDs of each widget
	status_id = WIDGET_INFO(event.top, find_by_uname='status')
	trial_id = WIDGET_INFO(event.top, find_by_uname='trialnum')
	cdens_id = WIDGET_INFO(event.top, find_by_uname='centraldens')
	step_id = WIDGET_INFO(event.top, find_by_uname='stepsize')
	hitwall_id = WIDGET_INFO(event.top, find_by_uname='hitwall')
	hitport_id = WIDGET_INFO(event.top, find_by_uname='hitport')
	finaldens_id = WIDGET_INFO(event.top, find_by_uname='finaldens')

    CASE uval OF

		'run': BEGIN ;If run button is pushed
		
			WIDGET_CONTROL, status_id, set_value='RUNNING'

			running_auto_mode = 1B
			
			; booleans to check for reflections
			hit_wall=0B
			hit_port=0B

			print, ''
			print, '  *** Getting density limit for reflected power into ECH ports...'
			; give them an hourglass to hold
			; widget_control, /hourglass
			; set time of first iteration
			t00=systime(1)

			; not needed
			; get systems that are included
			; sys_to_run=PrgDat.Status.Plotrays
			; nsys_to_run=n_elements(sys_to_run)
			; calc=where(sys_to_run gt 0, nsys_to_calc)

			; beginning density set to zipfits density
			try_ne=PrgDat.Profiles.CentralDensity
			; iteration values for step size
			bigstep=1.5e13
			step=8.0e12
			step_reduction_factor=0.4
			minimum_step=1.0e12

			WIDGET_CONTROL, step_id, set_value=strtrim(step)
			
			first_hit=0B ; useful for edge cases, set to true once a ray first hits the wall in the iteration

			iteration=1 ; count iterations

			; begin the iteration loop while step size is larger than the minimum step size !!!AND CANCEL BUTTON IS NOT PUSHED!!!
			WHILE ABS(step) GT minimum_step and widget_info(auto_stash.Cancel, /BUTTON_SET) ne 1 DO BEGIN

				; start trial with whatever density is next
				print, ' '
				print, '  *** Trial '+strtrim(iteration)+ $
					', central density='+strtrim(try_ne)

				WIDGET_CONTROL, trial_id, set_value=strtrim(iteration)
				WIDGET_CONTROL, cdens_id, set_value=strtrim(try_ne)

				; calc a density multiplier 
				dens_mult=(try_ne/PrgDat.Profiles.CentralDensity)

				num_g_rays=4 ; see Num_Rays function for key (4=30 rays)

				v=run_toray_echres( /save_data, dens_mult=dens_mult, number_gzones=num_g_rays )

				;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				; cleanup again so as not to have a bunch of toray windows
				;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

				;CLOSE the 'TORAY Plot Window' launched from previous run
				pltorayid=LookupManagedWidget('plot_toray')
				IF pltorayid NE 0L THEN widget_control,pltorayid,/destroy
				

				;CLOSE the opened 'Unabsorbed EC Power on the Wall' window when'run_Toray' is clicked
				raywallid=LookupManagedWidget('plot_rayonwall')
				IF raywallid NE 0L THEN widget_control,raywallid,/destroy

				;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				; end cleanup
				;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				
				; check if the toray run succeeded
				; begin simple alorithm to converge on a density limit
				if v.success then begin
					; check if the reflections hit the wall
					if hit_wall then begin
						first_hit = 1B
						print, 'hit wall'
						WIDGET_CONTROL, hitwall_id, set_value='YES'
						if hit_port then begin
							print, 'hit port'
							WIDGET_CONTROL, hitport_id, set_value='YES'
							; if hit port and density was going up make it now go down with reduced step size
							if step GT 0 then begin
								print, 'incrementing down in density'
								step=-step*step_reduction_factor
								print, 'step size:'+strtrim(step)
								WIDGET_CONTROL, step_id, set_value=strtrim(step)
							endif
						endif else begin
							print, 'did not hit port'
							WIDGET_CONTROL, hitport_id, set_value='NO'
							; if didn't hit port and density was going down make it go back up with reduced step size
							if step LT 0 then begin
								print, 'incrementing up in density'
								step=-step*step_reduction_factor
								print, 'step size:'+strtrim(step)
								WIDGET_CONTROL, step_id, set_value=strtrim(step)
							endif
						endelse
						; increment density with step
						try_ne=try_ne+step
					; if it didn't hit the wall at all then increase density with bigstep
					endif else begin
						print, 'did not hit wall'
						WIDGET_CONTROL, hitwall_id, set_value='NO'
						if first_hit then begin
							; edge case in which it increments down past the dens where it hits the wall, without this it continues to increment down forever
							if step LT 0 then begin
								step=-step*step_reduction_factor
								WIDGET_CONTROL, step_id, set_value=strtrim(step)
							endif
							try_ne=try_ne+step
						endif else begin 
							; if it didn't hit the wall and it still has yet to hit the wall increment up with bigstep
							try_ne=try_ne+bigstep
						endelse
					endelse
					if try_ne GT 2.0e14 then begin
						print, "NO PORT HITS AT DENS > 2.0e14"
						result_f=dialog_message(['AUTO REFL DENS', "NO PORT HITS AT DENS > 2.0e14, EXITING"])
						break
					endif

				endif else break

				; reset boolean values
				hit_wall=0B
				hit_port=0B
				iteration = iteration + 1

			ENDWHILE

			;Check first if code was stopped
			if widget_info(auto_stash.Cancel, /BUTTON_SET) eq 1 then begin
				WIDGET_CONTROL, status_id, set_value='STOPPED'
				WIDGET_CONTROL, finaldens_id, set_value = 0
			
			endif else begin
				; check if the toray run succeeded
				if v.success then begin
					; after loop is complete run toray one more time to view reflections

					; calc a density multiplier (input density / zipfits calculated density?)
					dens_mult=(try_ne/PrgDat.Profiles.CentralDensity)

					v=run_toray_echres( /save_data, dens_mult=dens_mult, number_gzones=num_g_rays )

					print, "FOUND REFL DENS LIMIT!!!"
					print, strtrim(try_ne)
					; result_d=dialog_message(['FOUND REFL DENSITY LIMIT:', strtrim(try_ne)])

					WIDGET_CONTROL, status_id, set_value='FINISHED'
					WIDGET_CONTROL, finaldens_id, set_value=strtrim(try_ne)
				endif
			endelse

			;Check if Density Limit Calculator is open
			if ISA(stash) eq 1 then begin
				
				if WIDGET_INFO(stash.DensCalcBase, /VALID_ID) eq 1 then begin

					;Calculate ECH Density Limit
					WIDGET_CONTROL, refl_id, set_value=try_ne
					WIDGET_CONTROL, line_avg_id, get_value=line_avg_final
					WIDGET_CONTROL, zipfits_id, get_value=zipfits_final
					WIDGET_CONTROL, refl_id, get_value=refl_final

					WIDGET_CONTROL, dens_lim_id, set_value=(line_avg_final)*(refl_final)/(zipfits_final)

					;Re-initialize zoom window
					WIDGET_CONTROL, zoom_start_id, set_value=-2000
					WIDGET_CONTROL, zoom_stop_id, set_value=8000
					WIDGET_CONTROL, stash.ZoomPlot, SEND_EVENT={id:0L, top:0L, handler:0l}
				endif

			endif

			running_auto_mode = 0B

        END

		ELSE: RETURN

    ENDCASE
end

PRO auto_density_limit

	Common AutoWidIDs

	;Create Base for GUI
	wBase = WIDGET_BASE(/column, title='Auto Reflection Density', uvalue='auto_refl_base')

	;Define First Layer
	wFirstLayer = widget_base(wBase, /row)
	wStatus = cw_field(wFirstLayer, /row, /NOEDIT, title='Status:', value='STANDBY', uvalue='status', uname='status', xsize=12)
	wTrialNum = cw_field(wFirstLayer, /row, /NOEDIT, title='Trial #:', value=0, uvalue='trialnum', uname='trialnum', xsize=5)

	;Define Second Layer
	wSecondLayer = widget_base(wBase, /exclusive, /row)
	wRun = widget_button(wSecondLayer, value=' RUN AUTO REFLECTION ', uvalue='run')
	wCancel = widget_button(wSecondLayer, value=' STOP ', uvalue='cancel')

	;Define Third Layer
	wThirdLayer = widget_base(wBase, /row)
	wCentralDens = cw_field(wThirdLayer, /row, /NOEDIT, title='Central Density (/cm3):', value='n/a', uvalue='centraldens', uname='centraldens', xsize=12)

	;Define Fourth Layer
	wFourthLayer = widget_base(wBase, /row)
	wStep = cw_field(wFourthLayer, /row, /NOEDIT, title='Step Size (/cm3):', value='n/a', uvalue='stepsize', uname='stepsize', xsize=12)

	;Define Fifth Layer
	wFifthLayer = widget_base(wBase, /row)
	wHitWall = cw_field(wFifthLayer, /row, /NOEDIT, title='Hit Wall:', value='n/a', uvalue='hitwall', uname='hitwall', xsize=5)
	wHitPort = cw_field(wFifthLayer, /row, /NOEDIT, title='Hit Port:', value='n/a', uvalue='hitport', uname='hitport', xsize=5)

	;Define Sixth Layer
	wSixthLayer = widget_base(wBase, /row)
	wFinalDens = cw_field(wSixthLayer, /row, /NOEDIT, title='Final Refl Dens (/cm3):', value='n/a', uvalue='finaldens', uname='finaldens', xsize=12)

 	;Realize the menu
    WIDGET_CONTROL, wBase, /REALIZE

	;Store certain widget information in "auto_stash" struct
    auto_stash = {Cancel:wCancel, AutoBase: wBase, Run:wRun}
    WIDGET_CONTROL, wBase, SET_UVALUE=auto_stash

    ;Register the GUI (Hand control to X-Manager)
    XMANAGER, 'auto_density_limit', wBase

END
