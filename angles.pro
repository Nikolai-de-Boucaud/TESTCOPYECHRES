;*********angles_event*************************************
pro angles_event, ev
common ECHCOM
common ANGCOM, dP, dG, blankP, blankG, AngIDs

wrng = '  !!!! angles.pro: Houston, we have a problem...'
dtr=!PI/180.
shot=PrgDat.config_shot
@$ECHRES_PATH/ECHSysData

;catch, an_error
;if an_error ne 0 then begin
;	print, '  !!! angles_event: error: ', !err_string
;	PrgDat.Status.OutputsValid=0
;	return
;endif

widget_control, ev.top, get_uvalue=AngIDs

widget_control, ev.id, get_uvalue=wid
case wid of	
	'Table': begin
		widget_control, ev.id, get_value=data
		case ev.type of
			0: begin
				if ((ev.ch ne 13) and (ev.ch ne 10)) then goto, done
				old_data=dP
				dP=data
				case ev.y of
					0: begin	; P99 M1 <--> Antenna_data[2] selected
						case ev.x of
							0: dP[0].gyro=old_data[0].gyro	; antenna number uneditable
							1: begin	; polar angle
								get_mir_angles, $
									Antenna_data[2].pol_id*dtr, Antenna_data[2].azi_id*dtr, $
									dP[0].polar*dtr, dP[0].azi*dtr, $
									Antenna_data[2].offset_angle*dtr, $
									pol_n, azi_n, tilt, facet, scan, crank, 0.0
								Point_P11:
								dP[0].tilt=tilt
								dP[0].facet=facet
								dP[0].tiltcnts=P1999_cradle_counts(dP[0].tilt, shot)
								dP[0].facetcnts=P1999_M1_counts(dP[0].facet, shot)
								dP[1].tilt=dP[0].tilt
								dP[1].tiltcnts=dP[0].tiltcnts
								get_reflected_angles, dP[1].tilt, dP[1].facet, $
									Antenna_data[3].pol_id*dtr, Antenna_data[3].azi_id*dtr, $
									Antenna_data[3].offset_angle*dtr, pol_r, azi_r, 0.0, $
									antenna_style='P1999'
								dP[1].polar=pol_r/dtr
								dP[1].azi=azi_r/dtr
							end
							2: begin	; azimuthal angle
								get_mir_angles, $
									Antenna_data[2].pol_id*dtr, Antenna_data[2].azi_id*dtr, $
									dP[0].polar*dtr, dP[0].azi*dtr, $
									Antenna_data[2].offset_angle*dtr, $
									pol_n, azi_n, tilt, facet, scan, crank, 0.0
								goto, Point_P11
							end
							3: begin	; tilt angle
								dP[0].tiltcnts=P1999_cradle_counts(dP[0].tilt, shot)
								Point_P12:
								dP[1].tiltcnts=dP[0].tiltcnts
								dP[1].tilt=dP[0].tilt
								get_reflected_angles, dP[0].tilt, dP[0].facet, $
									Antenna_data[2].pol_id*dtr, Antenna_data[2].azi_id*dtr, $
									Antenna_data[2].offset_angle*dtr, pol_r, azi_r, 0.0, $
									antenna_style='P1999'
								dP[0].polar=pol_r/dtr
								dP[0].azi=azi_r/dtr
								get_reflected_angles, dP[1].tilt, dP[1].facet, $
									Antenna_data[3].pol_id*dtr, Antenna_data[3].azi_id*dtr, $
									Antenna_data[3].offset_angle*dtr, pol_r, azi_r, 0.0, $
									antenna_style='P1999'
								dP[1].polar=pol_r/dtr
								dP[1].azi=azi_r/dtr
							end
							4: begin	; facet angle
								dP[0].facetcnts=P1999_M1_counts(dP[0].facet, shot)
								Point_P13:
								get_reflected_angles, dP[0].tilt, dP[0].facet, $
									Antenna_data[2].pol_id*dtr, Antenna_data[2].azi_id*dtr, $
									Antenna_data[2].offset_angle*dtr, pol_r, azi_r, 0.0, $
									antenna_style='P1999'
								dP[0].polar=pol_r/dtr
								dP[0].azi=azi_r/dtr
							end
							5: begin	; tilt counts
								dP[0].tilt=P1999_cradle_angle(dP[0].tiltcnts, shot)
								goto, Point_P12
							end
							6: begin	; facet counts
								dP[0].facet=P1999_M1_angle(dP[0].facetcnts, shot)
								goto, Point_P13
							end
							else: print, wrng + ' 1'
						endcase
					end		; end P99 M1
					
					1: begin	; P99 M2 <--> Antenna_data[3] selected
						case ev.x of
							0: dP[0].gyro=old_data[0].gyro	; antenna number uneditable
							1: begin	; polar angle
								get_mir_angles, $
									Antenna_data[3].pol_id*dtr, Antenna_data[3].azi_id*dtr, $
									dP[1].polar*dtr, dP[1].azi*dtr, $
									Antenna_data[3].offset_angle*dtr, $
									pol_n, azi_n, tilt, facet, scan, crank, 0.0
								Point_P21:
								dP[1].tilt=tilt
								dP[1].facet=facet
								dP[1].tiltcnts=P1999_cradle_counts(dP[1].tilt, shot)
								dP[1].facetcnts=P1999_M2_counts(dP[1].facet, shot)
								dP[0].tilt=dP[1].tilt
								dP[0].tiltcnts=dP[1].tiltcnts
								get_reflected_angles, dP[0].tilt, dP[0].facet, $
									Antenna_data[2].pol_id*dtr, Antenna_data[2].azi_id*dtr, $
									Antenna_data[2].offset_angle*dtr, pol_r, azi_r, 0.0, $
									antenna_style='P1999'
								dP[0].polar=pol_r/dtr
								dP[0].azi=azi_r/dtr
							end
							2: begin	; azimuthal angle
								get_mir_angles, $
									Antenna_data[3].pol_id*dtr, Antenna_data[3].azi_id*dtr, $
									dP[1].polar*dtr, dP[1].azi*dtr, $
									Antenna_data[3].offset_angle*dtr, $
									pol_n, azi_n, tilt, facet, scan, crank, 0.0
								goto, Point_P21
							end
							3: begin	; tilt angle
								dP[1].tiltcnts=P1999_cradle_counts(dP[1].tilt, shot)
								Point_P22:
								dP[0].tiltcnts=dP[1].tiltcnts
								dP[0].tilt=dP[1].tilt
								get_reflected_angles, dP[1].tilt, dP[1].facet, $
									Antenna_data[3].pol_id*dtr, Antenna_data[3].azi_id*dtr, $
									Antenna_data[3].offset_angle*dtr, pol_r, azi_r, $
									Antenna_data[3].antenna_inclinationd, antenna_style='P1999'
								dP[1].polar=pol_r/dtr
								dP[1].azi=azi_r/dtr
								get_reflected_angles, dP[0].tilt, dP[0].facet, $
									Antenna_data[2].pol_id*dtr, Antenna_data[2].azi_id*dtr, $
									Antenna_data[2].offset_angle*dtr, pol_r, azi_r, $
									Antenna_data[2].antenna_inclinationd, antenna_style='P1999'
								dP[0].polar=pol_r/dtr
								dP[0].azi=azi_r/dtr
							end
							4: begin	; facet angle
								dP[1].facetcnts=P1999_M2_counts(dP[1].facet, shot)
								Point_P23:
								get_reflected_angles, dP[1].tilt, dP[1].facet, $
									Antenna_data[3].pol_id*dtr, Antenna_data[3].azi_id*dtr, $
									Antenna_data[3].offset_angle*dtr, pol_r, azi_r, $
									Antenna_data[3].antenna_inclinationd, antenna_style='P1999'
								dP[1].polar=pol_r/dtr
								dP[1].azi=azi_r/dtr
							end
							5: begin	; tilt counts
								dP[1].tilt=P1999_cradle_angle(dP[1].tiltcnts, shot)
								goto, Point_P22
							end
							6: begin	; facet counts
								dP[1].facet=P1999_M2_angle(dP[1].facetcnts, shot)
								goto, Point_P23
							end
							else: print, wrng + ' 2'
						endcase
					end
					2: begin	; P01 M1
						case ev.x of
							0: dP[2].gyro=old_data[2].gyro	; antenna number uneditable
							1: begin
								Point_P31:
								get_mir_angles, $
									Antenna_data[6].pol_id*dtr, Antenna_data[6].azi_id*dtr, $
									dP[2].polar*dtr, dP[2].azi*dtr, $
									Antenna_data[6].offset_angle*dtr, $
									pol_n, azi_n, tilt, facet, scan, crank, $
									Antenna_data[6].antenna_inclinationd				
								facet=crank
								tilt=scan
								facetcnts=P2001_M1crank_counts(facet, shot)
								tiltcnts=P2001_M1scan_counts(tilt, shot)
								dP[2].facet=facet
								dP[2].tilt=tilt
								dP[2].facetcnts=facetcnts
								dP[2].tiltcnts=tiltcnts
								goto, Point_P32
							end
							2: goto, Point_P31
							3: begin	; scan angle ("tilt")
								facet=dP[2].facet
								tilt=dP[2].tilt
								facetcnts=P2001_M1crank_counts(facet, shot)
								tiltcnts=P2001_M1scan_counts(tilt, shot)
								dP[2].facet=facet
								dP[2].tilt=tilt
								dP[2].facetcnts=facetcnts
								dP[2].tiltcnts=tiltcnts
								Point_P32:
								get_reflected_angles, dP[2].tilt, dP[2].facet, $
									Antenna_data[6].pol_id*dtr, Antenna_data[6].azi_id*dtr, $
									Antenna_data[6].offset_angle*dtr, pol_r, azi_r, $
									Antenna_data[6].antenna_inclinationd, $
									antenna_style='P2001'
								dP[2].polar=pol_r/dtr
								dP[2].azi=azi_r/dtr
							end
							4: begin	; crank angle
								facet=dP[2].facet
								dP[2].facetcnts=P2001_M1crank_counts(facet, shot)
								dP[2].facet=facet
								goto, Point_P32
							end
							5: begin	; "tilt"  scan counts
								tiltcnts=dP[2].tiltcnts
								facetcnts=dP[2].facetcnts
								dP[2].tilt=P2001_M1scan_angle(tiltcnts, shot)
								dP[2].tiltcnts=tiltcnts
								dP[2].facetcnts=facetcnts
								goto, Point_P32
							end
							6: begin	; "facet" crank counts
								facetcnts=dP[2].facetcnts
								dP[2].facet=P2001_M1crank_angle(facetcnts, shot)
								dP[2].facetcnts=facetcnts
								goto, Point_P32
							end						
							else: print, wrng + ' 3'
						endcase
					end
					
					3: begin	; P01 M2
						case ev.x of
							0: dP[3].gyro=old_data[3].gyro	; antenna number uneditable
							1: begin	; polar angle
								Point_P41:
								get_mir_angles, $
									Antenna_data[7].pol_id*dtr, Antenna_data[7].azi_id*dtr, $
									dP[3].polar*dtr, dP[3].azi*dtr, $
									Antenna_data[7].offset_angle*dtr, $
									pol_n, azi_n, tilt, facet, scan, crank, $
									Antenna_data[7].antenna_inclinationd
								dP[3].tilt=scan	; scan <--> tilt
								dP[3].facet=crank; crank <--> facet
								dP[3].tiltcnts=P2001_M2scan_counts(scan, shot)
								dP[3].facetcnts=P2001_M2crank_counts(crank, shot)
							end
							2: goto, Point_P41
							3: begin	; scan angle ("tilt")
								dP[3].tiltcnts=P2001_M2scan_counts(dP[3].tilt, shot)
								Point_P42:
								get_reflected_angles, dP[3].tilt, dP[3].facet, $
									Antenna_data[7].pol_id*dtr, Antenna_data[7].azi_id*dtr, $
									Antenna_data[7].offset_angle*dtr, pol_r, azi_r, $
									Antenna_data[7].antenna_inclinationd, $
									antenna_style='P2001'
								dP[3].polar=pol_r/dtr
								dP[3].azi=azi_r/dtr
							end
							4: begin	; crank ("facet") angle
								dP[3].facetcnts=P2001_M2crank_counts(dP[3].facet, shot)
								goto, Point_P42
							end
							5: begin	; "tilt" counts
								dP[3].tilt=P2001_M2scan_angle(dP[3].tiltcnts, shot)
								goto, Point_P42
							end
							6: begin	; "facet" crank counts
								dP[3].facet=P2001_M2crank_angle(dP[3].facetcnts, shot)
								goto, Point_P42
							end
							else: print, wrng + ' 4'
						else: print, wrng + ' 5'
						endcase
					end	
						
					;********************************************************
					4: begin	; P02M1
						case ev.x of
							0: dP[4].gyro=old_data[4].gyro	; antenna number uneditable
							1: begin
								Point_P51:
								get_mir_angles, $
									Antenna_data[8].pol_id*dtr, Antenna_data[8].azi_id*dtr, $
									dP[4].polar*dtr, dP[4].azi*dtr, $
									Antenna_data[8].offset_angle*dtr, $
									pol_n, azi_n, tilt, facet, scan, crank, $
									Antenna_data[8].antenna_inclinationd				
								facet=crank
								tilt=scan
								facetcnts=P2002_M1crank_counts(facet, shot)
								tiltcnts=P2002_M1scan_counts(tilt, shot)
								dP[4].facet=facet
								dP[4].tilt=tilt
								dP[4].facetcnts=facetcnts
								dP[4].tiltcnts=tiltcnts
								goto, Point_P52
							end
							2: goto, Point_P51
							3: begin	; scan angle ("tilt")
								facet=dP[4].facet
								tilt=dP[4].tilt
								facetcnts=P2002_M1crank_counts(facet, shot)
								tiltcnts=P2002_M1scan_counts(tilt, shot)
								dP[4].facet=facet
								dP[4].tilt=tilt
								dP[4].facetcnts=facetcnts
								dP[4].tiltcnts=tiltcnts
								Point_P52:
								get_reflected_angles, dP[4].tilt, dP[4].facet, $
									Antenna_data[8].pol_id*dtr, Antenna_data[8].azi_id*dtr, $
									Antenna_data[8].offset_angle*dtr, pol_r, azi_r, $
									Antenna_data[8].antenna_inclinationd, $
									antenna_style='P2002'
								dP[4].polar=pol_r/dtr
								dP[4].azi=azi_r/dtr
							end
							4: begin	; crank angle
								facet=dP[4].facet
								dP[4].facetcnts=P2002_M1crank_counts(facet, shot)
								dP[4].facet=facet
								goto, Point_P52
							end
							5: begin	; "tilt"  scan counts
								tiltcnts=dP[4].tiltcnts
								facetcnts=dP[4].facetcnts
								dP[4].tilt=P2002_M1scan_angle(tiltcnts, shot)
								dP[4].tiltcnts=tiltcnts
								dP[4].facetcnts=facetcnts
								goto, Point_P52
							end
							6: begin	; "facet" crank counts
								facetcnts=dP[4].facetcnts
								dP[4].facet=P2002_M1crank_angle(facetcnts, shot)
								dP[4].facetcnts=facetcnts
								goto, Point_P52
							end						
							else: print, wrng + ' 6'
						endcase
					end
					
					5: begin	; P02 M2
						case ev.x of
							0: dP[5].gyro=old_data[5].gyro	; antenna number uneditable
							1: begin	; polar angle
								Point_P61:
								get_mir_angles, $
									Antenna_data[9].pol_id*dtr, Antenna_data[9].azi_id*dtr, $
									dP[5].polar*dtr, dP[5].azi*dtr, $
									Antenna_data[9].offset_angle*dtr, $
									pol_n, azi_n, tilt, facet, scan, crank, $
									Antenna_data[9].antenna_inclinationd
								dP[5].tilt=scan	; scan <--> tilt
								dP[5].facet=crank; crank <--> facet
								dP[5].tiltcnts=P2002_M2scan_counts(scan, shot)
								dP[5].facetcnts=P2002_M2crank_counts(crank, shot)
							end
							2: goto, Point_P61
							3: begin	; scan angle ("tilt")
								dP[5].tiltcnts=P2002_M2scan_counts(dP[5].tilt, shot)
								Point_P62:
								get_reflected_angles, dP[5].tilt, dP[5].facet, $
									Antenna_data[9].pol_id*dtr, Antenna_data[9].azi_id*dtr, $
									Antenna_data[9].offset_angle*dtr, pol_r, azi_r, $
									Antenna_data[9].antenna_inclinationd, $
									antenna_style='P2002'
								dP[5].polar=pol_r/dtr
								dP[5].azi=azi_r/dtr
							end
							4: begin	; crank ("facet") angle
								dP[5].facetcnts=P2002_M2crank_counts(dP[5].facet, shot)
								goto, Point_P62
							end
							5: begin	; "tilt" counts
								dP[5].tilt=P2002_M2scan_angle(dP[5].tiltcnts, shot)
								goto, Point_P62
							end
							6: begin	; "facet" crank counts
								dP[5].facet=P2002_M2crank_angle(dP[5].facetcnts, shot)
								goto, Point_P62
							end
							else: print, wrng + ' 7'
						else: print, wrng + ' 8'
						endcase
					end


;!!!!!!!!!!!!!
					6: begin	; P06M1
						case ev.x of
							0: dP[6].gyro=old_data[6].gyro	; antenna number uneditable
							1: begin
								Point_P71:
								get_mir_angles, $
									Antenna_data[10].pol_id*dtr, Antenna_data[10].azi_id*dtr, $
									dP[6].polar*dtr, dP[6].azi*dtr, $
									Antenna_data[10].offset_angle*dtr, $
									pol_n, azi_n, tilt, facet, scan, crank, $
									Antenna_data[10].antenna_inclinationd				
								facet=crank
								tilt=scan
								facetcnts=P2006_M1crank_counts(facet, shot)
								tiltcnts=P2006_M1scan_counts(tilt, shot)
								dP[6].facet=facet
								dP[6].tilt=tilt
								dP[6].facetcnts=facetcnts
								dP[6].tiltcnts=tiltcnts
								goto, Point_P72
							end
							2: goto, Point_P71
							3: begin	; scan angle ("tilt")
								facet=dP[6].facet
								tilt=dP[6].tilt
								facetcnts=P2006_M1crank_counts(facet, shot)
								tiltcnts=P2006_M1scan_counts(tilt, shot)
								dP[6].facet=facet
								dP[6].tilt=tilt
								dP[6].facetcnts=facetcnts
								dP[6].tiltcnts=tiltcnts
								Point_P72:
								get_reflected_angles, dP[6].tilt, dP[6].facet, $
									Antenna_data[10].pol_id*dtr, Antenna_data[10].azi_id*dtr, $
									Antenna_data[10].offset_angle*dtr, pol_r, azi_r, $
									Antenna_data[10].antenna_inclinationd, $
									antenna_style='P2006'
								dP[6].polar=pol_r/dtr
								dP[6].azi=azi_r/dtr
							end
							4: begin	; crank angle
								facet=dP[6].facet
								dP[6].facetcnts=P2006_M1crank_counts(facet, shot)
								dP[6].facet=facet
								goto, Point_P72
							end
							5: begin	; "tilt"  scan counts
								tiltcnts=dP[6].tiltcnts
								facetcnts=dP[6].facetcnts
								dP[6].tilt=P2006_M1scan_angle(tiltcnts, shot)
								dP[6].tiltcnts=tiltcnts
								dP[6].facetcnts=facetcnts
								goto, Point_P72
							end
							6: begin	; "facet" crank counts
								facetcnts=dP[6].facetcnts
								dP[6].facet=P2006_M1crank_angle(facetcnts, shot)
								dP[6].facetcnts=facetcnts
								goto, Point_P72
							end						
							else: print, wrng + ' 9'
						endcase
					end
					
					7: begin	; P06 M2
						case ev.x of
							0: dP[7].gyro=old_data[7].gyro	; antenna number uneditable
							1: begin	; polar angle
								Point_P81:
								get_mir_angles, $
									Antenna_data[11].pol_id*dtr, Antenna_data[11].azi_id*dtr, $
									dP[7].polar*dtr, dP[7].azi*dtr, $
									Antenna_data[11].offset_angle*dtr, $
									pol_n, azi_n, tilt, facet, scan, crank, $
									Antenna_data[11].antenna_inclinationd
								dP[7].tilt=scan	; scan <--> tilt
								dP[7].facet=crank; crank <--> facet
								dP[7].tiltcnts=P2006_M2scan_counts(scan, shot)
								dP[7].facetcnts=P2006_M2crank_counts(crank, shot)
							end
							2: goto, Point_P81
							3: begin	; scan angle ("tilt")
								dP[7].tiltcnts=P2006_M2scan_counts(dP[7].tilt, shot)
								Point_P82:
								get_reflected_angles, dP[7].tilt, dP[7].facet, $
									Antenna_data[11].pol_id*dtr, Antenna_data[11].azi_id*dtr, $
									Antenna_data[11].offset_angle*dtr, pol_r, azi_r, $
									Antenna_data[11].antenna_inclinationd, $
									antenna_style='P2006'
								dP[7].polar=pol_r/dtr
								dP[7].azi=azi_r/dtr
							end
							4: begin	; crank ("facet") angle
								dP[7].facetcnts=P2006_M2crank_counts(dP[7].facet, shot)
								goto, Point_P82
							end
							5: begin	; "tilt" counts
								dP[7].tilt=P2006_M2scan_angle(dP[7].tiltcnts, shot)
								goto, Point_P82
							end
							6: begin	; "facet" crank counts
								dP[7].facet=P2006_M2crank_angle(dP[7].facetcnts, shot)
								goto, Point_P82
							end
							else: print, wrng + ' 10'
						else: print, wrng + ' 11'
						endcase
					end
					else: print, wrng + ' 12'
				endcase
			end
			else: begin
				goto, done
			end
		endcase
	widget_control, ev.id, set_value=dP
	end
	
	'TableGA': begin
		widget_control, ev.id, get_value=dataga
		case ev.type of
			0: begin
				if ((ev.ch ne 13) and (ev.ch ne 10)) then goto, done
				old_dataga=dG
				dG=dataga
	
				case ev.y of
					0: begin	; GA1 M1 selected
						case ev.x of
							0: dG[0].gyro=old_dataga[0].gyro	; antenna number uneditable
							1: begin	; polar angle
								get_mir_angles, $
									Antenna_data[0].pol_id*dtr, Antenna_data[0].azi_id*dtr, $
									dG[0].polar*dtr, dG[0].azi*dtr, $
									Antenna_data[0].offset_angle*dtr, $
									pol_n, azi_n, tilt, facet, scan, crank, 0.0
								Point_G2:
								shim_tk=GA1_shim(dG[0].tilt, shot)
								get_best_shims, shim_tk, shim1, shim2, act_tk
								shim_tk=act_tk
								dG[0:1].shim1=shim1
								dG[0:1].shim2=shim2
								goto, Point_G1
							end
							2: dG[0].azi=old_dataga[0].azi		; uneditable
							4: dG[0].facet=Antenna_data[0].fixed_facet
							3: begin
								tilt=dG[0].tilt
								goto, Point_G2	; tilt angle
							end
							5: begin	; shim1
								dG[1].shim1=dG[0].shim1
								Point_G1:
								shim_tk=shmtk[dG[0].shim1]+shmtk[dG[0].shim2]
								dG[0:1].shimtk=shim_tk
								dG[0:1].tilt=GA1_tilt(shim_tk, shot)
								get_reflected_angles, dG[0].tilt, dG[0].facet, $
									Antenna_data[0].pol_id*dtr, Antenna_data[0].azi_id*dtr, $
									Antenna_data[0].offset_angle*dtr, pol_r, azi_r
								dG[0].polar=pol_r/dtr
								dG[0].azi=azi_r/dtr
								get_reflected_angles, dG[1].tilt, dG[1].facet, $
									Antenna_data[1].pol_id*dtr, Antenna_data[1].azi_id*dtr, $
									Antenna_data[1].offset_angle*dtr, pol_r, azi_r
								dG[1].polar=pol_r/dtr
								dG[1].azi=azi_r/dtr
							end
							6: begin	; shim2
								dG[1].shim2=dG[0].shim2
								goto, Point_G1
							end
							else: print, wrng + ' G7'
						endcase ; of ev.x
					end

					1: begin	; GA1 M2 selected
						case ev.x of
							0: dG[1].ant=old_dataga[1].ant	; antenna number uneditable
							1: begin	; polar angle
								get_mir_angles, $
									Antenna_data[1].pol_id*dtr, Antenna_data[1].azi_id*dtr, $
									dG[1].polar*dtr, dG[1].azi*dtr, $
									Antenna_data[1].offset_angle*dtr, $
									pol_n, azi_n, tilt, facet, scan, crank, 0.0
								Point_G3:
								shim_tk=GA1_shim(tilt, shot)
								get_best_shims, shim_tk, shim1, shim2, act_tk
								shim_tk=act_tk
								dG[0:1].shim1=shim1
								dG[0:1].shim2=shim2
								goto, Point_G4
								end
							2: dG[1].azi=old_dataga[1].azi		; uneditable
							4: dG[1].facet=Antenna_data[1].fixed_facet
							3: begin	; tilt angle
								tilt=dG[1].tilt
								goto, Point_G3
							end
							5: begin	; shim1
								dG[0].shim1=dG[1].shim1
								Point_G4:
								shim_tk=shmtk[dG[1].shim1]+shmtk[dG[1].shim2]
								dG[0:1].shimtk=shim_tk
								dG[0:1].tilt=GA1_tilt(shim_tk, shot)
								get_reflected_angles, dG[1].tilt, dG[1].facet, $
									Antenna_data[1].pol_id*dtr, Antenna_data[1].azi_id*dtr, $
									Antenna_data[1].offset_angle*dtr, pol_r, azi_r
								dG[1].polar=pol_r/dtr
								dG[1].azi=azi_r/dtr
								get_reflected_angles, dG[0].tilt, dG[0].facet, $
									Antenna_data[0].pol_id*dtr, Antenna_data[0].azi_id*dtr, $
									Antenna_data[0].offset_angle*dtr, pol_r, azi_r
								dG[0].polar=pol_r/dtr
								dG[0].azi=azi_r/dtr
							end
							6: begin	; shim2
								dG[0].shim2=dG[1].shim2
								goto, Point_G4
							end
							else: print, wrng + ' G8'
						endcase ; of ev.x
					end

					2: begin	; GA2 M1 selected: dG[2], Antenna_data[4]
						case ev.x of
							0: dG[2].gyro=old_dataga[2].gyro	; antenna number uneditable
							1: begin	; polar angle
								get_mir_angles, $
									Antenna_data[4].pol_id*dtr, Antenna_data[4].azi_id*dtr, $
									dG[2].polar*dtr, dG[2].azi*dtr, $
									Antenna_data[4].offset_angle*dtr, $
									pol_n, azi_n, tilt, facet, scan, crank, 0.0
								Point_G22:
								shim_tk=GA2_shim(tilt, shot)
								get_best_shims, shim_tk, shim1, shim2, act_tk
								shim_tk=act_tk
								dG[2:3].shim1=shim1
								dG[2:3].shim2=shim2
								goto, Point_G21
							end
							2: dG[2].azi=old_dataga[2].azi		; uneditable
							4: dG[2].facet=Antenna_data[4].fixed_facet
							3: begin
								tilt=dG[2].tilt
								goto, Point_G22	; tilt angle
							end
							5: begin	; shim1
								dG[3].shim1=dG[2].shim1
								Point_G21:
								shim_tk=shmtk[dG[2].shim1]+shmtk[dG[2].shim2]
								dG[2:3].shimtk=shim_tk
								dG[2:3].tilt=GA2_tilt(shim_tk, shot)
								get_reflected_angles, dG[2].tilt, dG[2].facet, $
									Antenna_data[4].pol_id*dtr, Antenna_data[4].azi_id*dtr, $
									Antenna_data[4].offset_angle*dtr, pol_r, azi_r
								dG[2].polar=pol_r/dtr
								dG[2].azi=azi_r/dtr
								get_reflected_angles, dG[3].tilt, dG[3].facet, $
									Antenna_data[5].pol_id*dtr, Antenna_data[5].azi_id*dtr, $
									Antenna_data[5].offset_angle*dtr, pol_r, azi_r
								dG[3].polar=pol_r/dtr
								dG[3].azi=azi_r/dtr
							end
							6: begin	; shim2
								dG[3].shim2=dG[2].shim2
								goto, Point_G21
							end
							else: print, wrng + ' G9'
						endcase ; of ev.x
					end

					3: begin	; GA2 M2 selected -> dG[3], Antenna_data[5]
						case ev.x of
							0: dG[3].ant=old_dataga[3].ant	; antenna number uneditable
							1: begin	; polar angle
								get_mir_angles, $
									Antenna_data[5].pol_id*dtr, Antenna_data[5].azi_id*dtr, $
									dG[3].polar*dtr, dG[3].azi*dtr, $
									Antenna_data[5].offset_angle*dtr, $
									pol_n, azi_n, tilt, facet, scan, crank, 0.0
								Point_G23:
								shim_tk=GA2_shim(tilt, shot)
								get_best_shims, shim_tk, shim1, shim2, act_tk
								shim_tk=act_tk
								dG[2:3].shim1=shim1
								dG[2:3].shim2=shim2
								goto, Point_G24
								end
							2: dG[3].azi=old_dataga[3].azi		; uneditable
							4: dG[3].facet=Antenna_data[5].fixed_facet
							3: begin	; tilt angle
								tilt=dG[3].tilt
								goto, Point_G23
							end
							5: begin	; shim1
								dG[2].shim1=dG[3].shim1
								Point_G24:
								shim_tk=shmtk[dG[3].shim1]+shmtk[dG[3].shim2]
								dG[2:3].shimtk=shim_tk
								dG[2:3].tilt=GA2_tilt(shim_tk, shot)
								get_reflected_angles, dG[3].tilt, dG[3].facet, $
									Antenna_data[5].pol_id*dtr, Antenna_data[5].azi_id*dtr, $
									Antenna_data[5].offset_angle*dtr, pol_r, azi_r
								dG[3].polar=pol_r/dtr
								dG[3].azi=azi_r/dtr
								get_reflected_angles, dG[2].tilt, dG[2].facet, $
									Antenna_data[4].pol_id*dtr, Antenna_data[4].azi_id*dtr, $
									Antenna_data[4].offset_angle*dtr, pol_r, azi_r
								dG[2].polar=pol_r/dtr
								dG[2].azi=azi_r/dtr
							end
							6: begin	; shim2
								dG[2].shim2=dG[3].shim2
								goto, Point_G24
							end
							else: print, wrng + ' G10'
						endcase ; of ev.x
					end
					else: print, wrng + ' G11'
				endcase	; of ev.y
			end
			else: goto, done
		endcase
		widget_control, ev.id, set_value=dG
	end
	'download': begin	; download tilt and facet angles
		for dd=0, n_elements(dP)-1 do dP[dd]=blankP
		for dd=0, n_elements(dG)-1 do dG[dd]=blankG
		set_angles, AngIDs
		return
	end
	'checkbounds': begin
		allok=1
		for kk=0, n_elements(dP)-1 do begin
			if dP[kk].gyro ne '' then begin
				antn=[2,3,6,7,8,9,10,11]
				success=counts_valid(dP[kk].tiltcnts, dP[kk].facetcnts, antn[kk], dP[kk].gyro)
				if not success then begin
					result=dialog_message('Counts out of bounds for ' + dP[kk].gyro)
					allok=0
				endif
			endif
		endfor
		if allok eq 1 then result=dialog_message('All counts are in bounds')
	end
	'close': widget_control, ev.top, /destroy
	else: goto, done
endcase

; fill data into argument structures
for ii=0, ECHSys.NumAntennas-1 do begin
	case ECHSys.Tank[ii].Antenna of 
		'GA1_M1': begin
			ECHSys.Tank[ii].TiltAng=dG[0].tilt
			ECHSys.Tank[ii].FacetAng=dG[0].facet
		end
		'GA1_M2': begin
			ECHSys.Tank[ii].TiltAng=dG[1].tilt
			ECHSys.Tank[ii].FacetAng=dG[1].facet
		end
		'GA2_M1': begin
			ECHSys.Tank[ii].TiltAng=dG[2].tilt
			ECHSys.Tank[ii].FacetAng=dG[2].facet
		end
		'GA2_M2': begin
			ECHSys.Tank[ii].TiltAng=dG[3].tilt
			ECHSys.Tank[ii].FacetAng=dG[3].facet
		end
		'P1999_M1': begin
			ECHSys.Tank[ii].TiltAng=dP[0].tilt
			ECHSys.Tank[ii].FacetAng=dP[0].facet
		end
		'P1999_M2': begin
			ECHSys.Tank[ii].TiltAng=dP[1].tilt
			ECHSys.Tank[ii].FacetAng=dP[1].facet
		end
		'P2001_M1': begin
			ECHSys.Tank[ii].TiltAng=dP[2].tilt
			ECHSys.Tank[ii].FacetAng=dP[2].facet
		end
		'P2001_M2': begin
			ECHSys.Tank[ii].TiltAng=dP[3].tilt
			ECHSys.Tank[ii].FacetAng=dP[3].facet
		end
		'P2002_M1': begin
			ECHSys.Tank[ii].TiltAng=dP[4].tilt
			ECHSys.Tank[ii].FacetAng=dP[4].facet
		end
		'P2002_M2': begin
			ECHSys.Tank[ii].TiltAng=dP[5].tilt
			ECHSys.Tank[ii].FacetAng=dP[5].facet
		end
		'P2006_M1': begin
			ECHSys.Tank[ii].TiltAng=dP[6].tilt
			ECHSys.Tank[ii].FacetAng=dP[6].facet
		end
		'P2006_M2': begin
			ECHSys.Tank[ii].TiltAng=dP[7].tilt
			ECHSys.Tank[ii].FacetAng=dP[7].facet
		end
		else: begin
			ECHSys.Tank[ii].TiltAng=0.
			ECHSys.Tank[ii].FacetAng=0.
			ECHSys.Tank[ii].GyroName=''
		end
	endcase
endfor

; update the main window input table
PrgDat.Status.OutputsValid=0
;widget_control, AngIDs.OutsValid, set_value=PrgDat.Status.OutputsValid
PrgDat.Status.ArchivedData=0
widget_control, AngIds.Archived, set_value=PrgDat.Status.ArchivedData
widget_control, AngIDs.Inputs, set_value=ECHSys.Tank

done:
end			; angles_event

;*****set_angles.pro**************************************
pro set_angles, widgetIDs
common ECHCOM
common ANGCOM, dP, dG, blankP, blankG, AngIDs

dtr=!PI/180.

catch, an_error
if an_error ne 0 then begin
	print, '  !!! set_angles: error: ', !err_string
	PrgDat.Status.OutputsValid=0
	return
endif

shot=PrgDat.config_shot
@$IDLSOURCE/echres/ECHSysData
dG.gyro[*]=''
dP.gyro[*]=''

; fill data into argument structures
for ii=0, ECHSys.NumAntennas-1 do begin
	case ECHSys.Tank[ii].Antenna of 
		'GA1_M1': begin
			dG[0].gyro=ECHSys.Tank[ii].Gyroname
			dG[0].tilt=ECHSys.Tank[ii].TiltAng
			dG[0].facet=Antenna_data[0].fixed_facet
		end
		'GA1_M2': begin
			dG[1].gyro=ECHSys.Tank[ii].Gyroname
			dG[1].tilt=ECHSys.Tank[ii].TiltAng
			dG[1].facet=Antenna_data[1].fixed_facet
		end
		'GA2_M1': begin
			dG[2].gyro=ECHSys.Tank[ii].Gyroname
			dG[2].tilt=ECHSys.Tank[ii].TiltAng
			dG[2].facet=Antenna_data[4].fixed_facet
		end
		'GA2_M2': begin
			dG[3].gyro=ECHSys.Tank[ii].Gyroname
			dG[3].tilt=ECHSys.Tank[ii].TiltAng
			dG[3].facet=Antenna_data[5].fixed_facet
		end
		'P1999_M1': begin
			dP[0].gyro=ECHSys.Tank[ii].GyroName
			dP[0].tilt=ECHSys.Tank[ii].TiltAng
			dP[0].facet=ECHSys.Tank[ii].FacetAng
		end
		'P1999_M2': begin
			dP[1].gyro=ECHSys.Tank[ii].GyroName
			dP[1].tilt=ECHSys.Tank[ii].TiltAng
			dP[1].facet=ECHSys.Tank[ii].FacetAng
		end
		'P2001_M1': begin
			dP[2].gyro=ECHSys.Tank[ii].GyroName
			dP[2].tilt=ECHSys.Tank[ii].TiltAng
			dP[2].facet=ECHSys.Tank[ii].FacetAng
		end
		'P2001_M2': begin
			dP[3].gyro=ECHSys.Tank[ii].GyroName
			dP[3].tilt=ECHSys.Tank[ii].TiltAng
			dP[3].facet=ECHSys.Tank[ii].FacetAng
		end
		'P2002_M1': begin
			dP[4].gyro=ECHSys.Tank[ii].GyroName
			dP[4].tilt=ECHSys.Tank[ii].TiltAng
			dP[4].facet=ECHSys.Tank[ii].FacetAng
		end
		'P2002_M2': begin
			dP[5].gyro=ECHSys.Tank[ii].GyroName
			dP[5].tilt=ECHSys.Tank[ii].TiltAng
			dP[5].facet=ECHSys.Tank[ii].FacetAng
		end
		'P2006_M1': begin
			dP[6].gyro=ECHSys.Tank[ii].GyroName
			dP[6].tilt=ECHSys.Tank[ii].TiltAng
			dP[6].facet=ECHSys.Tank[ii].FacetAng
		end
		'P2006_M2': begin
			dP[7].gyro=ECHSys.Tank[ii].GyroName
			dP[7].tilt=ECHSys.Tank[ii].TiltAng
			dP[7].facet=ECHSys.Tank[ii].FacetAng
		end
		else: 			
	endcase
endfor

; gyro, tilt, facet are inputs; calculate other quantities
; *** P1999 launcher ***
if (dP[0].gyro ne '') then begin
	dP[0].tiltcnts=P1999_cradle_counts(dP[0].tilt, shot)
	dP[0].facetcnts=P1999_M1_counts(dP[0].facet, shot)
	get_reflected_angles, dP[0].tilt, dP[0].facet, $	
		Antenna_data[2].pol_id*dtr, Antenna_data[2].azi_id*dtr, $
		Antenna_data[2].offset_angle*dtr, pol_r, azi_r, 0.0, antenna_style='P1999'
	dP[0].polar=pol_r/dtr
	dP[0].azi=azi_r/dtr 
	if dP[1].gyro ne '' then begin
		dP[1].tilt=dP[0].tilt	; give M1 dominance if conflict
		dP[1].tiltcnts=dP[0].tiltcnts
		dP[1].facetcnts=P1999_M2_counts(dP[1].facet, shot)	
		get_reflected_angles, dP[1].tilt, dP[1].facet, $
			Antenna_data[3].pol_id*dtr, Antenna_data[3].azi_id*dtr, $
			Antenna_data[3].offset_angle*dtr, pol_r, azi_r, 0.0, antenna_style='P1999'
		dP[1].polar=pol_r/dtr
		dP[1].azi=azi_r/dtr
	endif
endif else if dP[1].gyro ne '' then begin
	dP[1].tiltcnts=P1999_cradle_counts(dP[1].tilt, shot)
	dP[1].facetcnts=P1999_M2_counts(dP[1].facet, shot)
	get_reflected_angles, dP[1].tilt, dP[1].facet, $
		Antenna_data[3].pol_id*dtr, Antenna_data[3].azi_id*dtr, $
		Antenna_data[3].offset_angle*dtr, pol_r, azi_r, 0.0, antenna_style='P1999'
	dP[1].polar=pol_r/dtr
	dP[1].azi=azi_r/dtr
endif

; *** P2001 launcher ***
if dP[2].gyro ne '' then begin
	get_reflected_angles, dP[2].tilt, dP[2].facet, $
		Antenna_data[6].pol_id*dtr, Antenna_data[6].azi_id*dtr, $
		Antenna_data[6].offset_angle*dtr, pol_r, azi_r, $
		Antenna_data[6].antenna_inclinationd, antenna_style='P2001'
	dP[2].polar=pol_r/dtr
	dP[2].azi=azi_r/dtr
	facet=dP[2].facet
	tilt=dP[2].tilt
	facetcnts=P2001_M1crank_counts(facet, shot)
	tiltcnts=P2001_M1scan_counts(tilt, shot)
	dP[2].facet=facet
	dP[2].tilt=tilt
	dP[2].facetcnts=facetcnts
	dP[2].tiltcnts=tiltcnts
endif

if dP[3].gyro ne '' then begin
	get_reflected_angles, dP[3].tilt, dP[3].facet, $
		Antenna_data[7].pol_id*dtr, Antenna_data[7].azi_id*dtr, $
		Antenna_data[7].offset_angle*dtr, pol_r, azi_r, $
		Antenna_data[7].antenna_inclinationd, antenna_style='P2001'
	dP[3].polar=pol_r/dtr
	dP[3].azi=azi_r/dtr
	dP[3].tiltcnts=P2001_M2scan_counts(dP[3].tilt, shot)
	dP[3].facetcnts=P2001_M2crank_counts(dP[3].facet, shot)
endif

; *** P2002launcher ***
if dP[4].gyro ne '' then begin
	get_reflected_angles, dP[4].tilt, dP[4].facet, $
		Antenna_data[8].pol_id*dtr, Antenna_data[8].azi_id*dtr, $
		Antenna_data[8].offset_angle*dtr, pol_r, azi_r, $
		Antenna_data[8].antenna_inclinationd, antenna_style='P2002'
	dP[4].polar=pol_r/dtr
	dP[4].azi=azi_r/dtr
	facet=dP[4].facet
	tilt=dP[4].tilt
	facetcnts=P2002_M1crank_counts(facet, shot)
	tiltcnts=P2002_M1scan_counts(tilt, shot)
	dP[4].facet=facet
	dP[4].tilt=tilt
	dP[4].facetcnts=facetcnts
	dP[4].tiltcnts=tiltcnts
endif

if dP[5].gyro ne '' then begin
	get_reflected_angles, dP[5].tilt, dP[5].facet, $
		Antenna_data[9].pol_id*dtr, Antenna_data[9].azi_id*dtr, $
		Antenna_data[9].offset_angle*dtr, pol_r, azi_r, $
		Antenna_data[9].antenna_inclinationd, antenna_style='P2002'
	dP[5].polar=pol_r/dtr
	dP[5].azi=azi_r/dtr
	dP[5].tiltcnts=P2002_M2scan_counts(dP[5].tilt, shot)
	dP[5].facetcnts=P2002_M2crank_counts(dP[5].facet, shot)
endif

if dP[6].gyro ne '' then begin
	get_reflected_angles, dP[6].tilt, dP[6].facet, $
		Antenna_data[10].pol_id*dtr, Antenna_data[10].azi_id*dtr, $
		Antenna_data[10].offset_angle*dtr, pol_r, azi_r, $
		Antenna_data[10].antenna_inclinationd, antenna_style='P2006'
	dP[6].polar=pol_r/dtr
	dP[6].azi=azi_r/dtr
	dP[6].tiltcnts=P2006_M1scan_counts(dP[6].tilt, shot)
	dP[6].facetcnts=P2006_M1crank_counts(dP[6].facet, shot)
endif

if dP[7].gyro ne '' then begin
	get_reflected_angles, dP[7].tilt, dP[7].facet, $
		Antenna_data[11].pol_id*dtr, Antenna_data[11].azi_id*dtr, $
		Antenna_data[11].offset_angle*dtr, pol_r, azi_r, $
		Antenna_data[11].antenna_inclinationd, antenna_style='P2006'
	dP[7].polar=pol_r/dtr
	dP[7].azi=azi_r/dtr
	dP[7].tiltcnts=P2006_M2scan_counts(dP[7].tilt, shot)
	dP[7].facetcnts=P2006_M2crank_counts(dP[7].facet, shot)
endif

; *** GA1 antenna ***
if dG[0].gyro ne '' then begin
	shim_tk=GA1_shim(dG[0].tilt, shot)
	get_best_shims, shim_tk, shim1, shim2, act_tk
	dG[0].shim1=shim1
	dG[0].shim2=shim2
	dG[0].shimtk=act_tk
	dG[0].tilt=GA1_tilt(dG[0].shimtk, shot)
	get_reflected_angles, $
		dG[0].tilt, dG[0].facet, Antenna_data[0].pol_id*dtr, Antenna_data[0].azi_id*dtr, $
		Antenna_data[0].offset_angle*dtr, pol_r, azi_r
	dG[0].polar=pol_r/dtr
	dG[0].azi=azi_r/dtr
	if (dG[1].gyro ne '') then begin
		dG[1].shim1=dG[0].shim1
		dG[1].shim2=dG[0].shim2
		dG[1].shimtk=dG[0].shimtk
		dG[1].tilt=dG[0].tilt
		get_reflected_angles, $
			dG[1].tilt, dG[1].facet, Antenna_data[1].pol_id*dtr, Antenna_data[1].azi_id*dtr, $
			Antenna_data[1].offset_angle*dtr, pol_r, azi_r
		dG[1].polar=pol_r/dtr
		dG[1].azi=azi_r/dtr
	endif
endif else if dG[1].gyro ne '' then begin
	shim_tk=GA1_shim(dG[1].tilt, shot)
	get_best_shims, shim_tk, shim1, shim2, act_tk
	dG[1].shim1=shim1
	dG[1].shim2=shim2
	dG[1].shimtk=act_tk
	dG[1].tilt=GA1_tilt(dG[1].shimtk, shot)
	get_reflected_angles, $
		dG[1].tilt, dG[1].facet, Antenna_data[1].pol_id*dtr, Antenna_data[1].azi_id*dtr, $
		Antenna_data[1].offset_angle*dtr, pol_r, azi_r
	dG[1].polar=pol_r/dtr
	dG[1].azi=azi_r/dtr
endif

;*** GA2 antenna ***
if dG[2].gyro ne '' then begin
	shim_tk=GA2_shim(dG[2].tilt, shot)
	get_best_shims, shim_tk, shim1, shim2, act_tk
	dG[2].shim1=shim1
	dG[2].shim2=shim2
	dG[2].shimtk=act_tk
	dG[2].tilt=GA2_tilt(dG[2].shimtk, shot)
	get_reflected_angles, $
		dG[2].tilt, dG[2].facet, Antenna_data[4].pol_id*dtr, Antenna_data[4].azi_id*dtr, $
		Antenna_data[4].offset_angle*dtr, pol_r, azi_r
	dG[2].polar=pol_r/dtr
	dG[2].azi=azi_r/dtr
	if (dG[3].gyro ne '') then begin
		dG[3].shim1=dG[2].shim1
		dG[3].shim2=dG[2].shim2
		dG[3].shimtk=dG[2].shimtk
		dG[3].tilt=dG[2].tilt
		get_reflected_angles, $
			dG[3].tilt, dG[3].facet, Antenna_data[5].pol_id*dtr, Antenna_data[5].azi_id*dtr, $
			Antenna_data[5].offset_angle*dtr, pol_r, azi_r
		dG[3].polar=pol_r/dtr
		dG[3].azi=azi_r/dtr
	endif
endif else if dG[3].gyro ne '' then begin
	shim_tk=GA2_shim(dG[3].tilt, shot)
	get_best_shims, shim_tk, shim1, shim2, act_tk
	dG[3].shim1=shim1
	dG[3].shim2=shim2
	dG[3].shimtk=act_tk
	dG[3].tilt=GA2_tilt(dG[3].shimtk, shot)
	get_reflected_angles, $
		dG[3].tilt, dG[3].facet, Antenna_data[5].pol_id*dtr, Antenna_data[5].azi_id*dtr, $
		Antenna_data[5].offset_angle*dtr, pol_r, azi_r
	dG[3].polar=pol_r/dtr
	dG[3].azi=azi_r/dtr
endif

; fill data into argument structures
for ii=0, ECHSys.NumAntennas-1 do begin
	case ECHSys.Tank[ii].Antenna of 
		'GA1_M1': begin
			ECHSys.Tank[ii].TiltAng=dG[0].tilt
			ECHSys.Tank[ii].FacetAng=dG[0].facet
		end
		'GA1_M2': begin
			ECHSys.Tank[ii].TiltAng=dG[1].tilt
			ECHSys.Tank[ii].FacetAng=dG[1].facet
		end
		'GA2_M1': begin
			ECHSys.Tank[ii].TiltAng=dG[2].tilt
			ECHSys.Tank[ii].FacetAng=dG[2].facet
		end
		'GA2_M2': begin
			ECHSys.Tank[ii].TiltAng=dG[3].tilt
			ECHSys.Tank[ii].FacetAng=dG[3].facet
		end
		'P1999_M1': begin
			ECHSys.Tank[ii].TiltAng=dP[0].tilt
			ECHSys.Tank[ii].FacetAng=dP[0].facet
		end
		'P1999_M2': begin
			ECHSys.Tank[ii].TiltAng=dP[1].tilt
			ECHSys.Tank[ii].FacetAng=dP[1].facet
		end
		'P2001_M1': begin
			ECHSys.Tank[ii].TiltAng=dP[2].tilt
			ECHSys.Tank[ii].FacetAng=dP[2].facet
		end
		'P2001_M2': begin
			ECHSys.Tank[ii].TiltAng=dP[3].tilt
			ECHSys.Tank[ii].FacetAng=dP[3].facet
		end
		'P2002_M1': begin
			ECHSys.Tank[ii].TiltAng=dP[4].tilt
			ECHSys.Tank[ii].FacetAng=dP[4].facet
		end
		'P2002_M2': begin
			ECHSys.Tank[ii].TiltAng=dP[5].tilt
			ECHSys.Tank[ii].FacetAng=dP[5].facet
		end
		'P2006_M1': begin
			ECHSys.Tank[ii].TiltAng=dP[6].tilt
			ECHSys.Tank[ii].FacetAng=dP[6].facet
		end
		'P2006_M2': begin
			ECHSys.Tank[ii].TiltAng=dP[7].tilt
			ECHSys.Tank[ii].FacetAng=dP[7].facet			
		end
		else: 
	endcase
endfor

if xregistered('angles') then begin
	PrgDat.Status.OutputsValid=0;	
	;widget_control, widgetIDs.OutsValid, set_value=PrgDat.Status.OutputsValid
	ind=where(tag_names(widgetIDs) eq 'TABLE')
	if ind[0] gt -1 then begin ; didn't come in through Angle Helper door
		widget_control, widgetIDs.Table, set_value=dP
		widget_control, widgetIDs.Table2, set_value=dG
		widget_control, widgetIDs.Inputs, set_value=ECHSys.Tank
	endif
	return
endif
return
end	; set_angles

;******* angles ************************
pro angles, widgetids
common ECHCOM
common ANGCOM, dP, dG, blankP, blankG, AngIDs
Common WIDGETIDS, WidIDs

catch, an_error
if an_error ne 0 then begin
	print, '  !!! angles: error: ', !err_string
	return
endif

if xregistered('angles') then return

dtr=!PI/180.
shot=PrgDat.config_shot
blankP={Gyro:'', polar:0.0, azi:0.0, tilt:0.0, facet:0.0, $
	tiltcnts:long(0), facetcnts:long(0)}
dP=replicate(blankP, 8)	; 6 PPPL antennas

blankG={gyro:'', polar:0.0, azi:0.0, tilt:0.0, facet:0.0, $
	shim1:0, shim2:0, shimtk:0.000}
dG=replicate(blankG, 4)	; 4 GA antennas

set_angles, widgetids

; *** Now realize the widgets ***
col_labs=['Gyrotron', 'Polar', 'Azimuthal', 'Poloidal', 'Toroidal', 'Pol. Cnts', 'Tor. Cnts']
row_labs=['P99 M1', 'P99 M2', 'P01 M1', 'P01 M2', 'P02 M1', 'P02 M2', 'P06 M1', 'P06 M2']
fP=['(a9)', '(f8.3)', '(f8.3)', '(f8.3)',  '(f8.3)', '(i6)', '(i6)']
formP=[ [fP], [fP], [fP], [fP], [fP], [fP], [fp], [fp] ]
col_labsGA=['Gyrotron', 'Polar', 'Azimuthal', 'Tilt', 'Facet', 'Shim1', 'Shim2']
row_labsGA=['GA1 M1', 'GA1 M2', 'GA2 M1', 'GA2 M2']
fG=['(a9)', '(f8.3)', '(f8.3)', '(f8.3)', '(f8.3)', '(i2)', '(i2)', '(f8.3)']
formG=[ [fG], [fG], [fG], [fG] ]

wBase=widget_base(/column, title='ECH Antennas', group_leader=WidIDs.Base)
wTable=widget_table(wBase, /editable, /row_major, /all_events, $
	column_labels=col_labs, row_labels=row_labs, $
	alignment=1, uvalue='Table', xsize=n_elements(col_labs), $
	value=dP, ysize=n_elements(row_labs), format=formP)
wTableGA=widget_table(wBase, /editable, /row_major, /all_events, $
	column_labels=col_labsGA, row_labels=row_labsGA, $
	alignment=1, uvalue='TableGA', xsize=n_elements(col_labsGA), $
	value=dG, ysize=n_elements(row_labsGA), format=formG)
wBaseButtons=Widget_Base(wBase, /row)
;wDownLoad=widget_button(wBaseButtons, value=' Download tilts and facets ', uvalue='download')
wDownLoad=0
wCheckBounds=widget_button(wBaseButtons, value=' Check counts are valid ', uvalue='checkbounds')
wClose=widget_button(wBaseButtons, value='       Close window        ', uvalue='close')

AngIDs={Table:wTable, Table2:wTableGA, Download:wDownLoad, Close:wClose, $
	;OutsValid:widgetIDs.OutsValid, 
	Inputs:widgetIDs.Inputs, $
	Archived:widgetIDs.Inputs_Archived}
uval={AngIDs:AngIDs, dP:dP, dG:dG, shot:shot}
widget_control, wBase, /realize, set_uvalue=AngIDs
xmanager, 'angles', wBase
end
