; auto_aimer, called by echres_event.pro
; Starts a process to aim ECH antennas

pro auto_aimer_event, ev
common ECHCOM
common AIMER, obj, rhotarget, Bt0, Bt0max, Bt0min, eccdsign, $
	gauszones, polmin, polmax, azimin, azimax, npol, nazi, nbt, aawids

Btsign=gstruct.bcentr/abs(gstruct.bcentr)
Ipsign=gstruct.cpasma/abs(gstruct.cpasma)
;vals=findgen(11)*5.+155.

widget_control, ev.id, get_uvalue=wid

case wid of
	'Objective': if ev.select then begin
		obj=ev.value
		case obj of

			0:begin ; Minimize ECCD
					azimin=170.
					azimax=190.
			end

			1:begin ; Maximize I_ECCD
				if eccdsign*Btsign*Ipsign lt 0. then begin
					azimin=185.
					azimax=215.
				endif else begin
					azimin=145.
					azimax=175.
				endelse
			end

			2:begin ; Maximize j_ECCD
				if eccdsign*Btsign*Ipsign lt 0. then begin
					azimin=185.
					azimax=195.
				endif else begin
					azimin=165.
					azimax=175.
				endelse
				
			end

		endcase
		
	endif
	
	'rho': rhotarget=ev.value

	'ECCDSign':  if ev.select then begin
		eccdsign=ev.value
		case obj of
			
			0: begin
				azimin=170.
				azimax=190.
			end
			
			1:begin ; Maximize I_ECCD
				if eccdsign*Btsign*Ipsign lt 0. then begin
					azimin=185.
					azimax=205.
				endif else begin
					azimin=155.
					azimax=175.
				endelse
			end
			
			2:begin ; Maximize j_ECCD
				if eccdsign*Btsign*Ipsign lt 0. then begin
					azimin=185.
					azimax=195.
				endif else begin
					azimin=165.
					azimax=175.
				endelse
			end
			
		endcase

	endif

	'gzones':  if ev.select then begin
		gauszones=ev.value+1
		print, '  *** gauss zones='+strtrim(gauszones,2)
	endif
	'polmin': polmin=ev.value
	'polmax': polmax=ev.value
	'azimin': azimin=ev.value
	'azimax': azimax=ev.value
	'npol': if ev.select then begin
				npol=ev.value
				nazi=npol
			endif
	
	'go': begin
		widget_control, /hourglass
		
		e=survey_steering_angles(obj, rhotarget, $
			Bt0, Bt0max, Bt0min, eccdsign, $
			gauszones, polmin, polmax, npol, azimin, azimax, nazi, nbt)

		if e.success then plot_survey, e, ''

	end
	
	'close': begin
		widget_control, ev.top, /destroy
		return
	end
	
	else: stop

endcase

widget_control, aawids.azimin, set_value=azimin
widget_control, aawids.azimax, set_value=azimax
widget_control, aawids.polmin, set_value=polmin
widget_control, aawids.polmax, set_value=polmax		

end

pro auto_aimer, WidIDs
common ECHCOM
common AIMER, obj, rhotarget, Bt0, Bt0max, Bt0min, eccdsign, $
	gauszones, polmin, polmax, azimin, azimax, npol, nazi, nbt, aawids

if xregistered('auto_aimer') then return

obj=0
rhotarget=0.5
Bt0=gstruct.bcentr
Bt0max=Bt0
Bt0min=Bt0
eccdsign=1.0
gauszones=4
polmin=85.
polmax=125.
azimin=180.
azimax=190.
npol=5
nazi=5
nbt=1

if eccdsign gt 0. then ieccdsign=0 else ieccdsign=1

wBase=widget_base(/column, title='Auto Aimer', group_leader=WidIDs.Base)
wObjective=cw_bgroup(wBase, /exclusive, ['No ECCD', 'Max ECCD', 'Max j_ECCD'], $
	/row, label_left='Objective:', button_uvalue=[0,1,2], $
	uvalue='Objective', set_value=obj)
wECCDSign=cw_bgroup(wBase, /exclusive, ['+', '-'], /row, label_left='Sign ECCD', $
	button_uvalue=[1.0,-1.0], uvalue='ECCDSign', set_value=ieccdsign) 
wRho=cw_field(wBase, /floating, /return_events, /row, $
	title='Target value of rho:', value=rhotarget, uvalue='rho', xsize=8)
wNrays=cw_bgroup(wBase, /exclusive, ['1', '6', '18', '30', '74'], $
	/row, label_left='Number of rays:', button_uvalue=[0,1,2,3,5], $
	uvalue='gzones', set_value=gauszones-1)
wNPol=cw_bgroup(wBase, /exclusive, ['6', '9', '12', '18', '24'], $
	/row, label_left='Number polar and azi angles:', button_uvalue=[6,9,12,18,24], $
	uvalue='npol', set_value=1)
	
wAziRange=widget_base(wBase, /row)
wAziMin=cw_field(wAziRange, /floating, /return_events, /row, $
	title='Min azi ang:', uvalue='azimin', value=azimin)
wAziMax=cw_field(wAziRange, /floating, /return_events, /row, $
	title='Max azi ang:', uvalue='azimax', value=azimax)

wPolRange=widget_base(wBase, /row)
wPolMin=cw_field(wPolRange, /floating, /return_events, /row, $
	title='Min pol ang:', uvalue='polmin', value=polmin)
wPolMax=cw_field(wPolRange, /floating, /return_events, /row, $
	title='Max pol ang:', uvalue='polmax', value=polmax)

wGo=Widget_Button(wBase, value=' Start ', uvalue='go')
wClose=Widget_Button(wBase, value=' Close ', uvalue='close')
aawids={AziMin:wAziMin, AziMax:wAziMax, PolMin:wPolMin, PolMax:wPolMax}

;aawids={null:0}

widget_control, wBase, /realize
xmanager, 'auto_aimer', wBase

end
