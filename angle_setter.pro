@antenna_angles
@ang

pro antenna_event, ev
common IDs, WidIDs, d, dGA, shot
d_old=d
wrng = '     Houston, we have a problem...'
dtr=!PI/180.
@$ECHRES_PATH/ECHSysData

print, 'Angle_setter: Shot=', shot

widget_control, ev.id, get_uvalue=wid
case wid of
	'NewShot': begin
		widget_control, ev.id, get_value=shot
		get_mir_angles, $
			Antenna_data[2].pol_id*dtr, Antenna_data[2].azi_id*dtr, d[0].polar*dtr, d[0].azi*dtr, $
			Antenna_data[2].offset_angle*dtr, pol_n, azi_n, tilt, facet, scan, crank, 0.0
		d[0:1].tilt=tilt
		d[0].facet=facet
		d[0:1].tiltcnts=P1999_cradle_counts(tilt, shot)
		d[0].facetcnts=P1999_M1_counts(facet, shot)

		d[1].facet=-d[0].facet
		get_reflected_angles, d[1].tilt, d[1].facet, $
			Antenna_data[3].pol_id*dtr, Antenna_data[3].azi_id*dtr, $
			Antenna_data[3].offset_angle*dtr, pol_r, azi_r, 0.0, antenna_style='P1999'
		d[1].polar=pol_r/dtr
		d[1].azi=azi_r/dtr
		d[1].facetcnts=P1999_M2_counts(facet, shot)

		get_mir_angles, $
			Antenna_data[6].pol_id*dtr, Antenna_data[6].azi_id*dtr, $
			d[2].polar*dtr, d[2].azi*dtr, Antenna_data[6].offset_angle*dtr, $
			pol_n, azi_n, tilt, facet, scan, crank, $
			Antenna_data[6].antenna_inclinationd
		d[2].tilt=scan
		d[2].facet=crank
		d[2].tiltcnts=P2001_M1scan_counts(scan, shot)
		d[2].facetcnts=P2001_M1crank_counts(crank, shot)

		get_mir_angles, $
			Antenna_data[7].pol_id*dtr, Antenna_data[7].azi_id*dtr, $
			d[3].polar*dtr, d[3].azi*dtr, Antenna_data[7].offset_angle*dtr, $
			pol_n, azi_n, tilt, facet, scan, crank, $
			Antenna_data[7].antenna_inclinationd
		d[3].tilt=scan	; scan <--> tilt
		d[3].facet=crank; crank <--> facet
		d[3].tiltcnts=P2001_M2scan_counts(scan, shot)
		d[3].facetcnts=P2001_M2crank_counts(crank, shot)

		dGA[0:1].tilt=GA1_tilt(dGA[0].shimtk, shot)
		dGA[0].facet=Antenna_data[0].fixed_facet
		dGA[1].facet=Antenna_data[1].fixed_facet

		get_reflected_angles, $
			dGA[0].tilt, dGA[0].facet, Antenna_data[0].pol_id*dtr, Antenna_data[0].azi_id*dtr, $
			Antenna_data[0].offset_angle*dtr, pol_r, azi_r
		dga[0].polar=pol_r/dtr
		dga[0].azi=azi_r/dtr

		get_reflected_angles, $
			dga[1].tilt, dga[1].facet, Antenna_data[1].pol_id*dtr, Antenna_data[1].azi_id*dtr, $
			Antenna_data[1].offset_angle*dtr, pol_r, azi_r
		dga[1].polar=pol_r/dtr
		dga[1].azi=azi_r/dtr

		widget_control, WidIDs.Table, set_value=d
		widget_control, WidIDs.Table2, set_value=dGA
	end
	'Table': begin
		widget_control, ev.id, get_value=data
		case ev.type of
			0: begin
				if ev.ch ne 13 then goto, done
				old_data=d
				d=data
				case ev.y of
					0: begin	; P99 M1 <--> Antenna_data[2] selected
						case ev.x of
							0: d[0].gyro=old_data[0].gyro	; antenna number uneditable
							1: begin	; polar angle
								get_mir_angles, $
									Antenna_data[2].pol_id*dtr, Antenna_data[2].azi_id*dtr, $
									d[0].polar*dtr, d[0].azi*dtr, $
									Antenna_data[2].offset_angle*dtr, $
									pol_n, azi_n, tilt, facet, scan, crank, 0.0
								d[0].tilt=tilt
								d[0].facet=facet
								d[0].tiltcnts=P1999_cradle_counts(tilt, shot)
								d[0].facetcnts=P1999_M1_counts(facet, shot)
								d[1].tilt=tilt
								d[1].tiltcnts=d[0].tiltcnts
								get_reflected_angles, d[1].tilt, d[1].facet, $
									Antenna_data[3].pol_id*dtr, Antenna_data[3].azi_id*dtr, $
									Antenna_data[3].offset_angle*dtr, pol_r, azi_r, 0.0, $
									antenna_style='P1999'
								d[1].polar=pol_r/dtr
								d[1].azi=azi_r/dtr
							end
							2: begin	; azimuthal angle
								get_mir_angles, $
									Antenna_data[2].pol_id*dtr, Antenna_data[2].azi_id*dtr, $
									d[0].polar*dtr, d[0].azi*dtr, $
									Antenna_data[2].offset_angle*dtr, $
									pol_n, azi_n, tilt, facet, scan, crank, 0.0
								d[0].tilt=tilt
								d[0].facet=facet
								d[0].tiltcnts=P1999_cradle_counts(tilt, shot)
								d[0].facetcnts=P1999_M1_counts(facet, shot)
								d[1].tilt=tilt
								d[1].tiltcnts=d[0].tiltcnts
								get_reflected_angles, d[1].tilt, d[1].facet, $
									Antenna_data[3].pol_id*dtr, Antenna_data[3].azi_id*dtr, $
									Antenna_data[3].offset_angle*dtr, pol_r, azi_r, 0.0, $
									antenna_style='P1999'
								d[1].polar=pol_r/dtr
								d[1].azi=azi_r/dtr
							end
							3: begin	; tilt angle
								d[0].tiltcnts=P1999_cradle_counts(d[0].tilt, shot)
								d[1].tiltcnts=d[0].tiltcnts
								d[1].tilt=d[0].tilt
								get_reflected_angles, d[0].tilt, d[0].facet, $
									Antenna_data[2].pol_id*dtr, Antenna_data[2].azi_id*dtr, $
									Antenna_data[2].offset_angle*dtr, pol_r, azi_r, 0.0, $
									antenna_style='P1999'
								d[0].polar=pol_r/dtr
								d[0].azi=azi_r/dtr
								get_reflected_angles, d[1].tilt, d[1].facet, $
									Antenna_data[3].pol_id*dtr, Antenna_data[3].azi_id*dtr, $
									Antenna_data[3].offset_angle*dtr, pol_r, azi_r, 0.0, $
									antenna_style='P1999'
								d[1].polar=pol_r/dtr
								d[1].azi=azi_r/dtr
							end
							4: begin	; facet angle
								d[0].facetcnts=P1999_M1_counts(d[0].facet, shot)
								get_reflected_angles, d[0].tilt, d[0].facet, $
									Antenna_data[2].pol_id*dtr, Antenna_data[2].azi_id*dtr, $
									Antenna_data[2].offset_angle*dtr, pol_r, azi_r, 0.0, $
									antenna_style='P1999'
								d[0].polar=pol_r/dtr
								d[0].azi=azi_r/dtr
							end
							5: begin	; tilt counts
								d[0].tilt=P1999_cradle_angle(d[0].tiltcnts, shot)
								d[1].tilt=d[0].tilt
								d[1].tiltcnts=d[0].tiltcnts
								get_reflected_angles, d[0].tilt, d[0].facet, $
									Antenna_data[2].pol_id*dtr, Antenna_data[2].azi_id*dtr, $
									Antenna_data[2].offset_angle*dtr, pol_r, azi_r, 0.0, $
									antenna_style='P1999'
								d[0].polar=pol_r/dtr
								d[0].azi=azi_r/dtr
								get_reflected_angles, d[1].tilt, d[1].facet, $
									Antenna_data[3].pol_id*dtr, Antenna_data[3].azi_id*dtr, $
									Antenna_data[3].offset_angle*dtr, pol_r, azi_r, 0.0, $
									antenna_style='P1999'
								d[1].polar=pol_r/dtr
								d[1].azi=azi_r/dtr
							end
							6: begin	; facet counts
								d[0].facet=P1999_M1_angle(d[0].facetcnts, shot)
								get_reflected_angles, d[0].tilt, d[0].facet, $
									Antenna_data[2].pol_id*dtr, Antenna_data[2].azi_id*dtr, $
									Antenna_data[2].offset_angle*dtr, pol_r, azi_r, 0.0, $
									antenna_style='P1999'
								d[0].polar=pol_r/dtr
								d[0].azi=azi_r/dtr
							end
							else: print, wrng, '1'
						endcase
					end		; end P99 M1
					
					1: begin	; P99 M2 <--> Antenna_data[3] selected
						case ev.x of
							0: d[0].gyro=old_data[0].gyro	; antenna number uneditable
							1: begin	; polar angle
								get_mir_angles, $
									Antenna_data[3].pol_id*dtr, Antenna_data[3].azi_id*dtr, $
									d[1].polar*dtr, d[1].azi*dtr, $
									Antenna_data[3].offset_angle*dtr, $
									pol_n, azi_n, tilt, facet, scan, crank, 0.0
								d[1].tilt=tilt
								d[1].facet=facet
								d[1].tiltcnts=P1999_cradle_counts(tilt, shot)
								d[1].facetcnts=P1999_M1_counts(facet, shot)
								d[0].tilt=tilt
								d[0].tiltcnts=d[1].tiltcnts
								get_reflected_angles, d[0].tilt, d[0].facet, $
									Antenna_data[2].pol_id*dtr, Antenna_data[2].azi_id*dtr, $
									Antenna_data[2].offset_angle*dtr, pol_r, azi_r, 0.0, $
									antenna_style='P1999'
								d[0].polar=pol_r/dtr
								d[0].azi=azi_r/dtr
							end
							2: begin	; azimuthal angle
								get_mir_angles, $
									Antenna_data[3].pol_id*dtr, Antenna_data[3].azi_id*dtr, $
									d[1].polar*dtr, d[1].azi*dtr, $
									Antenna_data[3].offset_angle*dtr, $
									pol_n, azi_n, tilt, facet, scan, crank, 0.0
								d[1].tilt=tilt
								d[1].facet=facet
								d[1].tiltcnts=P1999_cradle_counts(tilt, shot)
								d[1].facetcnts=P1999_M1_counts(facet, shot)
								d[0].tilt=tilt
								d[0].tiltcnts=d[1].tiltcnts
								get_reflected_angles, d[0].tilt, d[0].facet, $
									Antenna_data[2].pol_id*dtr, Antenna_data[2].azi_id*dtr, $
									Antenna_data[2].offset_angle*dtr, pol_r, azi_r, 0.0, $
									antenna_style='P1999'
								d[0].polar=pol_r/dtr
								d[0].azi=azi_r/dtr
							end
							3: begin	; tilt angle
								d[1].tiltcnts=P1999_cradle_counts(d[1].tilt, shot)
								d[0].tiltcnts=d[1].tiltcnts
								d[0].tilt=d[1].tilt
								get_reflected_angles, d[1].tilt, d[1].facet, $
									Antenna_data[3].pol_id*dtr, Antenna_data[3].azi_id*dtr, $
									Antenna_data[3].offset_angle*dtr, pol_r, azi_r, $
									Antenna_data[3].antenna_inclinationd, antenna_style='P1999'
								d[1].polar=pol_r/dtr
								d[1].azi=azi_r/dtr
								get_reflected_angles, d[0].tilt, d[0].facet, $
									Antenna_data[2].pol_id*dtr, Antenna_data[2].azi_id*dtr, $
									Antenna_data[2].offset_angle*dtr, pol_r, azi_r, $
									Antenna_data[2].antenna_inclinationd, antenna_style='P1999'
								d[0].polar=pol_r/dtr
								d[0].azi=azi_r/dtr
							end
							4: begin	; facet angle
								d[1].facetcnts=P1999_M2_counts(d[1].facet, shot)
								get_reflected_angles, d[1].tilt, d[1].facet, $
									Antenna_data[3].pol_id*dtr, Antenna_data[3].azi_id*dtr, $
									Antenna_data[3].offset_angle*dtr, pol_r, azi_r, $
									Antenna_data[3].antenna_inclinationd, antenna_style='P1999'
								d[1].polar=pol_r/dtr
								d[1].azi=azi_r/dtr
							end
							5: begin	; tilt counts
								d[1].tilt=P1999_cradle_angle(d[1].tiltcnts, shot)
								d[0].tilt=d[1].tilt
								d[0].tiltcnts=d[1].tiltcnts
								get_reflected_angles, d[1].tilt, d[1].facet, $
									Antenna_data[3].pol_id*dtr, Antenna_data[3].azi_id*dtr, $
									Antenna_data[3].offset_angle*dtr, pol_r, azi_r, $
									Antenna_data[3].antenna_inclinationd, antenna_style='P1999'
								d[1].polar=pol_r/dtr
								d[1].azi=azi_r/dtr
								get_reflected_angles, d[0].tilt, d[0].facet, $
									Antenna_data[2].pol_id*dtr, Antenna_data[2].azi_id*dtr, $
									Antenna_data[2].offset_angle*dtr, pol_r, azi_r, $
									Antenna_data[2].antenna_inclinationd, antenna_style='P1999'
								d[0].polar=pol_r/dtr
								d[0].azi=azi_r/dtr
							end
							6: begin	; facet counts
								d[1].facet=P1999_M2_angle(d[1].facetcnts, shot)
								get_reflected_angles, d[1].tilt, d[1].facet, $
									Antenna_data[3].pol_id*dtr, Antenna_data[3].azi_id*dtr, $
									Antenna_data[3].offset_angle*dtr, pol_r, azi_r, $
									Antenna_data[3].antenna_inclinationd, antenna_style='P1999'
								d[1].polar=pol_r/dtr
								d[1].azi=azi_r/dtr
							end
							else: print, wrng, '2'
						endcase
					end
					2: begin	; P01 M1
						case ev.x of
							0: d[2].gyro=old_data[2].gyro	; antenna number uneditable
							1: begin	; polar angle
								get_mir_angles, $
									Antenna_data[6].pol_id*dtr, Antenna_data[6].azi_id*dtr, $
									d[2].polar*dtr, d[2].azi*dtr, $
									Antenna_data[6].offset_angle*dtr, $
									pol_n, azi_n, tilt, facet, scan, crank, $
									Antenna_data[6].antenna_inclinationd
								d[2].tilt=scan
								d[2].facet=crank
								d[2].tiltcnts=P2001_M1scan_counts(scan, shot)
								d[2].facetcnts=P2001_M1crank_counts(crank, shot)
							end
							2: begin	; azimuthal angle
								get_mir_angles, $
									Antenna_data[6].pol_id*dtr, Antenna_data[6].azi_id*dtr, $
									d[2].polar*dtr, d[2].azi*dtr, $
									Antenna_data[6].offset_angle*dtr, $
									pol_n, azi_n, tilt, facet, scan, crank, $
									Antenna_data[6].antenna_inclinationd
								d[2].tilt=scan
								d[2].facet=crank
								d[2].tiltcnts=P2001_M1scan_counts(scan, shot)
								d[2].facetcnts=P2001_M1crank_counts(crank, shot)
							end
							3: begin	; scan angle ("tilt")
								d[2].tiltcnts=P2001_M1scan_counts(d[2].tilt, shot)
								get_reflected_angles, d[2].tilt, d[2].facet, $
									Antenna_data[6].pol_id*dtr, Antenna_data[6].azi_id*dtr, $
									Antenna_data[6].offset_angle*dtr, pol_r, azi_r, $
									Antenna_data[6].antenna_inclinationd, $
									antenna_style='P2001'
								d[2].polar=pol_r/dtr
								d[2].azi=azi_r/dtr
							end
							4: begin	; crank angle
								d[2].facetcnts=P2001_M1crank_counts(d[2].facet, shot)
								get_reflected_angles, d[2].tilt, d[2].facet, $
									Antenna_data[6].pol_id*dtr, Antenna_data[6].azi_id*dtr, $
									Antenna_data[6].offset_angle*dtr, pol_r, azi_r, $
									Antenna_data[6].antenna_inclinationd, $
									antenna_style='P2001'
								d[2].polar=pol_r/dtr
								d[2].azi=azi_r/dtr
							end
							5: begin	; "tilt"  scan counts
								d[2].tilt=P2001_M1scan_angle(d[2].tiltcnts, shot)
								get_reflected_angles, d[2].tilt, d[2].facet, $
									Antenna_data[6].pol_id*dtr, Antenna_data[6].azi_id*dtr, $
									Antenna_data[6].offset_angle*dtr, pol_r, azi_r, $
									Antenna_data[6].antenna_inclinationd, $
									antenna_style='P2001'
								d[2].polar=pol_r/dtr
								d[2].azi=azi_r/dtr
							end
							6: begin	; "facet" crank counts
								d[2].facet=P2001_M1crank_angle(d[2].facetcnts, shot)
								get_reflected_angles, d[2].tilt, d[2].facet, $
									Antenna_data[6].pol_id*dtr, Antenna_data[6].azi_id*dtr, $
									Antenna_data[6].offset_angle*dtr, pol_r, azi_r, $
									Antenna_data[6].antenna_inclinationd, $
									antenna_style='P2001'
								d[2].polar=pol_r/dtr
								d[2].azi=azi_r/dtr
							end						
							else: print, wrng, '3'
						endcase
					end
					
					3: begin	; P01 M2
						case ev.x of
							0: d[3].gyro=old_data[3].gyro	; antenna number uneditable
							1: begin	; polar angle
								get_mir_angles, $
									Antenna_data[7].pol_id*dtr, Antenna_data[7].azi_id*dtr, $
									d[3].polar*dtr, d[3].azi*dtr, $
									Antenna_data[7].offset_angle*dtr, $
									pol_n, azi_n, tilt, facet, scan, crank, $
									Antenna_data[7].antenna_inclinationd
								d[3].tilt=scan	; scan <--> tilt
								d[3].facet=crank; crank <--> facet
								d[3].tiltcnts=P2001_M2scan_counts(scan, shot)
								d[3].facetcnts=P2001_M2crank_counts(crank, shot)
							end
							2: begin	; azimuthal angle
								get_mir_angles, $
									Antenna_data[7].pol_id*dtr, Antenna_data[7].azi_id*dtr, $
									d[3].polar*dtr, d[3].azi*dtr, $
									Antenna_data[7].offset_angle*dtr, $
									pol_n, azi_n, tilt, facet, scan, crank, $
									Antenna_data[7].antenna_inclinationd
								d[3].tilt=scan
								d[3].facet=crank
								d[3].tiltcnts=P2001_M2scan_counts(scan, shot)
								d[3].facetcnts=P2001_M2crank_counts(crank, shot)
							end
							3: begin	; scan angle ("tilt")
								d[3].tiltcnts=P2001_M2scan_counts(d[3].tilt, shot)
								get_reflected_angles, d[3].tilt, d[3].facet, $
									Antenna_data[7].pol_id*dtr, Antenna_data[7].azi_id*dtr, $
									Antenna_data[7].offset_angle*dtr, pol_r, azi_r, $
									Antenna_data[7].antenna_inclinationd, $
									antenna_style='P2001'
								d[3].polar=pol_r/dtr
								d[3].azi=azi_r/dtr
							end
							4: begin	; crank ("facet") angle
								d[3].facetcnts=P2001_M2crank_counts(d[3].facet, shot)
								get_reflected_angles, d[3].tilt, d[3].facet, $
									Antenna_data[7].pol_id*dtr, Antenna_data[7].azi_id*dtr, $
									Antenna_data[7].offset_angle*dtr, pol_r, azi_r, $
									Antenna_data[7].antenna_inclinationd, $
									antenna_style='P2001'
								d[3].polar=pol_r/dtr
								d[3].azi=azi_r/dtr
							end
							5: begin	; "tilt" counts
								d[3].tilt=P2001_M2_scan_angle(d[3].tiltcnts, shot)
								get_reflected_angles, d[3].tilt, d[3].facet, $
									Antenna_data[7].pol_id*dtr, Antenna_data[7].azi_id*dtr, $
									Antenna_data[7].offset_angle*dtr, pol_r, azi_r, $
									Antenna_data[7].antenna_inclinationd, $
									antenna_style='P2001'
								d[3].polar=pol_r/dtr
								d[3].azi=azi_r/dtr
							end
							6: begin	; "facet" crank counts
								d[3].facet=P2001_M2crank_angle(d[3].facetcnts, shot)
								get_reflected_angles, d[3].tilt, d[3].facet, $
									Antenna_data[7].pol_id*dtr, Antenna_data[7].azi_id*dtr, $
									Antenna_data[7].offset_angle*dtr, pol_r, azi_r, $
									Antenna_data[7].antenna_inclinationd, $
									antenna_style='P2001'
								d[3].polar=pol_r/dtr
								d[3].azi=azi_r/dtr
							end
							else: print, wrng, '4'
						else: print, wrng, '5'
						end
					endcase
					else: print, wrng, '6'
				endcase
			end
			4: begin	; cell was selected
				if ev.sel_left EQ -1 then goto, Done
				widget_control, ev.id, edit_cell=[ev.sel_top, ev.sel_left]
			end
			else: begin
				goto, done
			end
		endcase
	widget_control, ev.id, set_value=d 
	end
	
	'TableGA': begin
		widget_control, ev.id, get_value=dataga
		case ev.type of
			0: begin
				if ev.ch ne 13 then goto, done
				old_dataga=dGA
				dGA=dataga
				case ev.y of
					0: begin	; GA1 M1 selected
						case ev.x of
							0: dga[0].gyro=old_dataga[0].gyro	; antenna number uneditable
							1: begin	; polar angle
								get_mir_angles, $
									Antenna_data[0].pol_id*dtr, Antenna_data[0].azi_id*dtr, $
									dga[0].polar*dtr, dga[0].azi*dtr, $
									Antenna_data[0].offset_angle*dtr, $
									pol_n, azi_n, tilt, facet, scan, crank, 0.0
								shim_tk=GA1_shim(tilt, shot)
								get_best_shims, shim_tk, shim1, shim2, act_tk
								shim_tk=act_tk
								dga[0:1].tilt=tilt
								dga[0:1].shim1=shim1
								dga[0:1].shim2=shim2
								goto, point_1
							end
							2: begin	; azimuthal angle
								dga[0].azi=old_dataga[0].azi		; uneditable
							end
							3: begin	; tilt angle
								shim_tk=GA1_shim(dga[0].tilt, shot)
								get_best_shims, shim_tk, shim1, shim2, act_tk
								dga[0:1].shim1=shim1
								dga[0:1].shim2=shim2
								shim_tk=act_tk
								goto, Point_1
							end
							4: begin	; facet angle
								dga[0].facet=Antenna_data[0].fixed_facet
							end
							5: begin	; shim1
								dga[1].shim1=dga[0].shim1
								shim_tk=shmtk[dga[0].shim1]+shmtk[dga[0].shim2]
								point_1:
								dga[0:1].shimtk=shim_tk
								dga[0:1].tilt=GA1_tilt(shim_tk, shot)
								get_reflected_angles, $
									dga[0].tilt, dGA[0].facet, $
									Antenna_data[0].pol_id*dtr, Antenna_data[0].azi_id*dtr, $
									Antenna_data[0].offset_angle*dtr, $
									pol_r, azi_r
								dga[0].polar=pol_r/dtr
								dga[0].azi=azi_r/dtr
								get_reflected_angles, $
									dga[1].tilt, dga[1].facet, $
									Antenna_data[1].pol_id*dtr, Antenna_data[1].azi_id*dtr, $
									Antenna_data[1].offset_angle*dtr, $
									pol_r, azi_r
								dga[1].polar=pol_r/dtr
								dga[1].azi=azi_r/dtr
							end
							6: begin	; shim2
								dga[1].shim2=dga[0].shim2
								shim_tk=shmtk[dga[0].shim1]+shmtk[dga[0].shim2]
								goto, point_1
							end
							7: begin	; shim thickness
								get_best_shims, dga[0].shimtk, shim1, shim2, act_tk
								shim_tk=act_tk
								dga[0:1].shim1=shim1
								dga[0:1].shim2=shim2
								goto, Point_1
							end
							else: print, wrng, '7'
						endcase ; of ev.x
					end

					1: begin	; GA1 M2 selected
						case ev.x of
							0: dga[1].ant=old_dataga[1].ant	; antenna number uneditable
							1: begin	; polar angle
								get_mir_angles, $
									Antenna_data[1].pol_id*dtr, Antenna_data[1].azi_id*dtr, $
									dga[1].polar*dtr, dga[1].azi*dtr, $
									Antenna_data[1].offset_angle*dtr, $
									pol_n, azi_n, tilt, facet, scan, crank, 0.0
								shim_tk=GA1_shim(tilt, shot)
								get_best_shims, shim_tk, shim1, shim2, act_tk
								shim_tk=act_tk
								dga[0:1].tilt=tilt
								dga[0:1].shim1=shim1
								dga[0:1].shim2=shim2
								goto, point_2
								end
							2: begin	; azimuthal angle
								dga[1].azi=old_dataga[1].azi		; uneditable
							end
							3: begin	; tilt angle
								shim_tk=GA1_shim(dga[1].tilt, shot)
								get_best_shims, shim_tk, shim1, shim2, act_tk
								dga[0:1].shim1=shim1
								dga[0:1].shim2=shim2
								shim_tk=act_tk
								goto, Point_2
							end
							4: begin	; facet angle
								dga[1].facet=Antenna_data[1].fixed_facet
							end
							5: begin	; shim1
								dga[0].shim1=dga[1].shim1
								shim_tk=shmtk[dga[1].shim1]+shmtk[dga[1].shim2]
								point_2:
								dga[0:1].shimtk=shim_tk
								dga[0:1].tilt=GA1_tilt(shim_tk, shot)
								get_reflected_angles, $
									dga[1].tilt, dga[1].facet, $
									Antenna_data[1].pol_id*dtr, Antenna_data[1].azi_id*dtr, $
									Antenna_data[1].offset_angle*dtr, $
									pol_r, azi_r
								dga[1].polar=pol_r/dtr
								dga[1].azi=azi_r/dtr
								get_reflected_angles, $
									dga[0].tilt, dga[0].facet, $
									Antenna_data[0].pol_id*dtr, Antenna_data[0].azi_id*dtr, $
									Antenna_data[0].offset_angle*dtr, $
									pol_r, azi_r
								dga[0].polar=pol_r/dtr
								dga[0].azi=azi_r/dtr
							end
							6: begin	; shim2
								dga[0].shim2=dga[1].shim2
								shim_tk=shmtk[dga[1].shim1]+shmtk[dga[1].shim2]
								goto, point_2
							end
							7: begin	; shim thickness
								get_best_shims, dga[1].shimtk, shim1, shim2, act_tk
								shim_tk=act_tk
								dga[0:1].shim1=shim1
								dga[0:1].shim2=shim2
								goto, Point_2
							end
							else: print, wrng, '7'
						endcase ; of ev.x
					end
					else: print, wrng, '9'
				endcase	; of ev.y
			end
			else: goto, done
		endcase
	widget_control, ev.id, set_value=dga	
	end
	else: goto, done
endcase
done:
end

; main
common IDs, WidIDs, d, dGA, shot
dtr=!PI/180.
shot=1000000
@$IDLSOURCE/echres/ECHSysData

d={Gyro:GyroToAnt[2], polar:90.0, azi:180.0, tilt:0.0, facet:0.0, tiltcnts:long(0), facetcnts:long(0)}
d=replicate(d,4)
for ii=0,3 do d[ii].Gyro=GyroToAnt[ii+2]

P01=0 & P99=0
for i=0, NumberOfAntennas-1 do begin
	if Antenna_data[i].DwgNo eq 'P1999' then P99=i
	if Antenna_data[i].DwgNo eq 'P2001' then P01=i
endfor
;if P99 ne 0 then begin
;	d[0].ant=P99
;	d[1].ant=P99+1
;endif
;if P01 ne 0 then begin
;	d[2].ant=P01
;	d[3].ant=P01+1
;endif

; enter initial data
get_mir_angles, $
	Antenna_data[2].pol_id*dtr, Antenna_data[2].azi_id*dtr, d[0].polar*dtr, d[0].azi*dtr, $
	Antenna_data[2].offset_angle*dtr, pol_n, azi_n, tilt, facet, scan, crank, 0.0
d[0:1].tilt=tilt
d[0].facet=facet
d[0:1].tiltcnts=P1999_cradle_counts(tilt, shot)
d[0].facetcnts=P1999_M1_counts(facet, shot)

d[1].facet=-d[0].facet
get_reflected_angles, d[1].tilt, d[1].facet, $
	Antenna_data[3].pol_id*dtr, Antenna_data[3].azi_id*dtr, $
	Antenna_data[3].offset_angle*dtr, pol_r, azi_r, 0.0, antenna_style='P1999'
d[1].polar=pol_r/dtr
d[1].azi=azi_r/dtr
d[1].facetcnts=P1999_M2_counts(facet, shot)

get_mir_angles, $
	Antenna_data[6].pol_id*dtr, Antenna_data[6].azi_id*dtr, $
	d[2].polar*dtr, d[2].azi*dtr, Antenna_data[6].offset_angle*dtr, $
	pol_n, azi_n, tilt, facet, scan, crank, $
	Antenna_data[6].antenna_inclinationd
d[2].tilt=scan
d[2].facet=crank
d[2].tiltcnts=P2001_M1scan_counts(scan, shot)
d[2].facetcnts=P2001_M1crank_counts(crank, shot)

get_mir_angles, $
	Antenna_data[7].pol_id*dtr, Antenna_data[7].azi_id*dtr, $
	d[3].polar*dtr, d[3].azi*dtr, Antenna_data[7].offset_angle*dtr, $
	pol_n, azi_n, tilt, facet, scan, crank, $
	Antenna_data[7].antenna_inclinationd
d[3].tilt=scan	; scan <--> tilt
d[3].facet=crank; crank <-->facet
d[3].tiltcnts=P2001_M2scan_counts(scan, shot)
d[3].facetcnts=P2001_M2crank_counts(crank, shot)

col_labs=['Gyro', 'Polar', 'Azimuthal', 'Tilt', 'Facet', 'T. Cnts', 'F. Cnts']
row_labs=['P99 M1', 'P99 M2', 'P01 M1', 'P01 M2']

form=[ ['(a9)', '(f7.2)', '(f7.2)', '(f7.2)',  '(f7.2)', '(i6)', '(i6)'], $
	['(a9)', '(f7.2)', '(f7.2)', '(f7.2)', '(f7.2)', '(i6)', '(i6)'], $
	['(a9)', '(f7.2)', '(f7.2)', '(f7.2)', '(f7.2)', '(i6)', '(i6)'], $
	['(a9)', '(f7.2)', '(f7.2)', '(f7.2)', '(f7.2)', '(i6)', '(i6)'] ]

; *** Now table for GA antenna ***
dGA={gyro:GyroToAnt[0], polar:0.0, azi:0.0, tilt:0.0, facet:0.0, shim1:5, shim2:0, shimtk:0.601}
dGA=replicate(dGA, 2)
dGA[1].gyro=GyroToAnt[1]

col_labsGA=['Gyro', 'Polar', 'Azimuthal', 'Tilt', 'Facet', 'Shim1', 'Shim2'];, 'ShimTk']
row_labsGA=['GA1 M1', 'GA1 M2']
formGA=[ ['(a9)', '(f7.2)', '(f7.2)', '(f7.2)', '(f7.2)', '(i2)', '(i2)', '(f7.3)'], $
	['(a9)', '(f7.2)', '(f7.2)', '(f7.2)', '(f7.2)', '(i2)', '(i2)', '(f7.3)'] ]

dGA[0:1].tilt=GA1_tilt(dGA[0].shimtk, shot)
dGA[0].facet=Antenna_data[0].fixed_facet
dGA[1].facet=Antenna_data[1].fixed_facet

get_reflected_angles, $
	dga[0].tilt, dga[0].facet, Antenna_data[0].pol_id*dtr, Antenna_data[0].azi_id*dtr, $
	Antenna_data[0].offset_angle*dtr, pol_r, azi_r
dga[0].polar=pol_r/dtr
dga[0].azi=azi_r/dtr

get_reflected_angles, $
	dga[1].tilt, dga[1].facet, Antenna_data[1].pol_id*dtr, Antenna_data[1].azi_id*dtr, $
	Antenna_data[1].offset_angle*dtr, pol_r, azi_r
dga[1].polar=pol_r/dtr
dga[1].azi=azi_r/dtr

; *** Now realize the widgets ***
wBase=widget_base(/column, title='  ECH Antennas')
wTable=widget_table(wBase, /editable, /row_major, /all_events, $
	column_labels=col_labs, row_labels=row_labs, $
	alignment=1, uvalue='Table', xsize=n_elements(col_labs), $
	value=d, ysize=n_elements(row_labs), format=form)
wTableGA=widget_table(wBase, /editable, /row_major, /all_events, $
	column_labels=col_labsGA, row_labels=row_labsGA, $
	alignment=1, uvalue='TableGA', xsize=n_elements(col_labsGA), $
	value=dGA, ysize=n_elements(row_labsGA), format=formGA)

; could have a shot number widget, but now set always to be current shot
; wShot=cw_field(wBase, /floating, /return_events, /row, $
;	title='  Shot:', value=shot, uvalue='NewShot')

WidIDs={Table:wTable, Table2:wTableGA}
widget_control, wBase, /realize
xmanager, 'antenna', wBase
end
