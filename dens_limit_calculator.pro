;Event handler for GUI
PRO dens_limit_calculator_EVENT, event

    ;Define shared variables
    COMMON DensWidIDs, plot_id, line_avg_id, zipfits_id, refl_id, dens_lim_id, zoom_start_id, zoom_stop_id, stash
    COMMON mdsData, shotnum, dens, t_dens, pcs_total, t_pcs, TIME_ARRAY, time_raw, clicked_times, clicked_dens
    
    ;Use "uval" to determine which event happened
    WIDGET_CONTROL,event.id,GET_UVALUE=uval
    
    ;Get certain widget information from "stash" struct
    WIDGET_CONTROL, event.TOP, GET_UVALUE=stash

    ;Get the IDs of each widget
    shotnum_id = WIDGET_INFO(event.top, find_by_uname='shotnum')
    plot_density_button_id = WIDGET_INFO(event.top, find_by_uname='plot_density')
    time_slice_id = WIDGET_INFO(event.top, find_by_uname='timeslice')
    cursor_id = WIDGET_INFO(event.top, find_by_uname='cursor')
    plot_id = WIDGET_INFO(event.top, find_by_uname='plotdata')
    zoom_start_id = WIDGET_INFO(event.top, find_by_uname='zoom_start')
    zoom_stop_id = WIDGET_INFO(event.top, find_by_uname='zoom_stop')
    line_avg_id = WIDGET_INFO(event.top, find_by_uname='lineavg')
    zipfits_id = WIDGET_INFO(event.top, find_by_uname='zipfits')
    refl_id = WIDGET_INFO(event.top, find_by_uname='refldens')
    dens_lim_id = WIDGET_INFO(event.top, find_by_uname='denslimit')
    
    CASE uval OF

        'shotnum': BEGIN ;If return key is hit inside shot number box (automatically plot density data)
            goto, plot_the_density
        END

        'plot_density': BEGIN ;If Plot Density button was pushed
            
            plot_the_density:

            ;Initialize clicked points arrays
            clicked_times = []
            clicked_dens = []

            ;Initialize zoom window
            WIDGET_CONTROL, zoom_start_id, set_value=-2000
            WIDGET_CONTROL, zoom_stop_id, set_value=8000

            ;Pull density data from PTDATA for given shot number
            mdsconnect, 'atlas'
            WIDGET_CONTROL, shotnum_id, get_value=shotnum

            ;Pull most recent shot data if shotnum = 0, previous shot if shotnum = -1, etc.
            if LONG(shotnum) le 0 then begin
                shotnum = mdsvalue('current_shot("d3d")') + LONG(shotnum)
            endif

            dens = mdsvalue('PTDATA("ecsdensf",$)',LONG(shotnum))
            t_dens = mdsvalue('DIM_OF(PTDATA("ecsdensf",$))',LONG(shotnum))
            
            ;Pull PCS CMD data
            pcs_sys4 = BOOLEAN(mdsvalue('PTDATA("dacgyro4",$)',LONG(shotnum)))
            pcs_sys5 = BOOLEAN(mdsvalue('PTDATA("dacgyro5",$)',LONG(shotnum)))
            pcs_sys6 = BOOLEAN(mdsvalue('PTDATA("dacgyro6",$)',LONG(shotnum)))
            pcs_sys7 = BOOLEAN(mdsvalue('PTDATA("dacgyro7",$)',LONG(shotnum)))
            pcs_sys8 = BOOLEAN(mdsvalue('PTDATA("dacgyro8",$)',LONG(shotnum)))
            pcs_sys9 = BOOLEAN(mdsvalue('PTDATA("dacgyro9",$)',LONG(shotnum)))
            pcs_sys10 = BOOLEAN(mdsvalue('PTDATA("dacgyro10",$)',LONG(shotnum)))
            pcs_sys11 = BOOLEAN(mdsvalue('PTDATA("dacgyro11",$)',LONG(shotnum)))
            t_pcs = mdsvalue('DIM_OF(PTDATA("dacgyro4",$))',LONG(shotnum))
            
            ;Find total PCS CMD for all gyros
            pcs_total = pcs_sys4 or pcs_sys5 or pcs_sys6 or pcs_sys7 or pcs_sys8 or pcs_sys9 or pcs_sys10 or pcs_sys11

            ;Open EFIT01 branch and get array of possible timeslices
            mdsopen, 'EFIT01', LONG(shotnum)
            TIME_ARRAY = mdsvalue('\GTIME')
            
            ;Set plot as current window
            WIDGET_CONTROL, event.TOP, GET_UVALUE=stash
            WSET, stash.drawID
            ERASE

            ;Update the plot
            plot, t_dens, dens, title='Shot #' + STRTRIM(shotnum,2) + ' (black=density, blue=pcs cmd)', ytitle='Line Avg Density (/cm3)',$
            xtitle='Time (ms)', xrange=[-2000,8000]

            ;Overplot PCS CMD (blue)
            oplot, t_pcs, pcs_total, color=3
            
        END

        'zoom_start': BEGIN
            goto, set_zoom_plot ;If return key is hit inside zoom start box (automatically hits zoom plot button)
        END
        
        'zoom_stop': BEGIN ;If return key is hit inside zoom stop box (automatically hits zoom plot button)
            goto, set_zoom_plot
        END

        'zoom_plot': BEGIN ;If Zoom Plot button is pushed
            
            set_zoom_plot:
            
            ;Set plot as current window
            WSET, stash.drawID
            ERASE

            ;Get tmin and tmax values from zoom plot input boxes
            WIDGET_CONTROL, zoom_start_id, get_value=xmin
            WIDGET_CONTROL, zoom_stop_id, get_value=xmax
            
            ;Re-plot the data with new xrange
            plot, t_dens, dens, title='Shot #' + STRTRIM(shotnum,2) + ' (black=density, blue=pcs cmd)', ytitle='Line Avg Density (/cm3)',$
            xtitle='Time (ms)', xrange=[xmin,xmax]

            ;Replot user-selected density values
            if clicked_times ne [] and clicked_dens ne [] then begin
                oplot, [clicked_times], [clicked_dens], psym=2, symsize=2, color=2
            endif

            ;Overplot PCS CMD (blue)
            oplot, t_pcs, pcs_total, color=3

        END

        'timeslice': BEGIN ;If return key is hit inside timeslice box (automatically fill line avg density and oplot selected density)
            WIDGET_CONTROL, time_slice_id, get_value=time_raw
            GOTO, oplot_dens
        END

        'plotdata': BEGIN ;If user interacting with plot

            ;Set plot as current window
            WIDGET_CONTROL, event.TOP, GET_UVALUE=stash
            WSET, stash.drawID

            ;Convert coordinates from display window to data windo
            newCoords = Convert_Coord(event.x, event.y, /Device, /To_Data)
            newx = newCoords[0]
            newy = newCoords[1]
            
            ;Display coordinates in Cursor box
            coord_string = '(' + STRTRIM(newX) + ',' + STRTRIM(newY) + ')'
            WIDGET_CONTROL, cursor_id, set_value = coord_string.Compress()

            IF (event.release EQ 1) THEN BEGIN ;If user clicks inside plot
                
                ;Grab current x/y position on the graph
                time_raw = newx
                
                oplot_dens:
                
                sz_time_array = SIZE(TIME_ARRAY)
                ;Find closest timeslice to the user's selection
                min_difference = 999999999
                for i=0, sz_time_array[1]-1 do begin
                    time_difference = ABS(TIME_ARRAY[i] - time_raw)
                    if min_difference GT time_difference then begin
                        ind_of_closest_timeslice = i
                        min_difference = time_difference
                    endif
                endfor
                time_final = TIME_ARRAY[ind_of_closest_timeslice]

                ;Find density value at the closest time
                sz_dens_time_array = SIZE(t_dens)
                for i=0, sz_dens_time_array[1]-1 do begin
                    if t_dens[i] ge time_final then begin
                        dens_final_idx = i
                        break
                    endif
                endfor
                dens_final = dens[dens_final_idx]

                ;Plot red star at closest time and density where user clicks
                oplot, [time_final], [dens_final], psym=2, symsize=2, color=2

                ;Autofill time slice and line avg density boxes
                WIDGET_CONTROL, time_slice_id, set_value=time_final
                WIDGET_CONTROL, line_avg_id, set_value=dens_final * 1e13

                ;Append to clicked times and clicked dens arrays (to retain data during zoom plot)
                clicked_times = [clicked_times, time_final]
                clicked_dens = [clicked_dens, dens_final]

            ENDIF
        END

        'calculate_density_mds': BEGIN ;If calculate density with MDS button was pushed

            ;Calculate density limit with MDS setup
            WIDGET_CONTROL, time_slice_id, get_value=time_slice
            use_mds = 1B
            find_dens = 1B
            run_ech_res_timeslice, LONG(shotnum), FLOAT(time_slice), use_mds, find_dens

        END

        'calculate_density_setup': BEGIN ;If calculate density with setup file button was pushed

            ;Calculate density limit with setup file
            WIDGET_CONTROL, time_slice_id, get_value=time_slice
            use_mds = 0B
            find_dens = 1B
            run_ech_res_timeslice, LONG(shotnum), FLOAT(time_slice), use_mds, find_dens

        END

        'load_echres': BEGIN ;If load echres button was pushed

            ;Load shot info into echres but do not calculate density
            WIDGET_CONTROL, time_slice_id, get_value=time_slice
            use_mds = 0B
            find_dens = 0B
            run_ech_res_timeslice, LONG(shotnum), FLOAT(time_slice), use_mds, find_dens

        END

        'zipfits': BEGIN ;If return key is hit inside zipfits box (calculate ech dens limit based on other boxes)
            goto, calculate_ech_dens_limit
        END

        'refldens': BEGIN ;If return key is hit inside reflection dens box (calculate ech dens limit based on other boxes)
            
            calculate_ech_dens_limit:

        	WIDGET_CONTROL, line_avg_id, get_value=line_avg_final
		    WIDGET_CONTROL, zipfits_id, get_value=zipfits_final
		    WIDGET_CONTROL, refl_id, get_value=refl_final
            WIDGET_CONTROL, dens_lim_id, set_value=(line_avg_final)*(refl_final)/(zipfits_final)
            
        END

        ELSE: RETURN

    ENDCASE

END


;GUI definition
PRO dens_limit_calculator

    COMMON DensWidIDs

    ;Create Base for GUI
    wBase = WIDGET_BASE(/column, title='ECH Density Limit Calculator', uvalue='dens_calc_base')

    ;Shot Number Text Box
    wTopLayer = widget_base(wBase, /row)
    wShotnum = cw_field(wTopLayer, /return_events, /row, title='Shot Number:', value=0, uvalue='shotnum', uname='shotnum', xsize=10)
    
    ;Plot Line Avg Density Button
    wPlotDensity = WIDGET_BUTTON(wTopLayer, value=' PLOT LINE AVG DENSITY ', uvalue='plot_density')

    ;Cursor Indicator Box
    wCursorIndicator = cw_field(WTopLayer, /row, title='(t, dens)', value=0, uvalue='cursor', uname='cursor', xsize=20)

    ;Zoom Tstart
    wZoomStart = cw_field(WTopLayer, /return_events, /row, title='Zoom Window (ms):', value=-2000, uvalue='zoom_start', uname='zoom_start', xsize=10)

    ;Zoom Tstop
    wZoomStop = cw_field(WTopLayer, /return_events, /row, title='', value=8000, uvalue='zoom_stop', uname='zoom_stop', xsize=10)

    ;Zoom Plot Button
    wZoomPlot = WIDGET_BUTTON(wTopLayer, value=' ZOOM PLOT ', uvalue='zoom_plot')

    ;Density Plot
    wDraw = widget_draw(wBase, uvalue='plotdata', uname='plotdata', retain=2, xsize=1100, ysize=700, /Motion_Event, /Button_Event)

    ;Time Slice Text Box
    WBottomLayer = widget_base(wBase, /row)
    wTimeslice = cw_field(WBottomLayer, /return_events, /row, title='Time Slice (ms):', value=0, uvalue='timeslice', uname='timeslice', xsize=10)

    ;Calculate Density Button with MDS Setup
    wCalcButtonMDS = WIDGET_BUTTON(WBottomLayer, value=' CALCULATE DENS LIMIT via MDS SETUP ', uvalue='calculate_density_mds')

    ;Calculate Density Button with Setup File
    wCalcButtonSetup = WIDGET_BUTTON(WBottomLayer, value=' CALCULATE DENS LIMIT via SETUP FILE ', uvalue='calculate_density_setup')

    ;Load into ECHRES button
    wLoadECHRESButon = WIDGET_BUTTON(WBottomLayer, value=' LOAD INTO ECHRES ', uvalue='load_echres')   

    ;Density output boxes
    wDensities = widget_base(wBase, /row)

    wLineAvg = cw_field(wDensities, /floating, /row, title='Line Avg (/cm3):',$
    value=0, uvalue='lineavg', uname='lineavg', xsize=12)

    wZipfits = cw_field(wDensities, /floating, /return_events, /row, title='Zipfits (/cm3):',$
    value=0, uvalue='zipfits', uname='zipfits', xsize=12)

    wRefl = cw_field(wDensities, /floating, /return_events, /row, title='Reflection Dens (/cm3):',$
    value=0, uvalue='refldens', uname='refldens', xsize=12)

    wDensLimit = cw_field(wDensities, /floating, /row, title='ECH Dens Limit (/cm3): ',$
    value=0, uvalue='denslimit', uname='denslimit', xsize=12)

    ;Realize the menu
    WIDGET_CONTROL, wBase, /REALIZE
    
    ;Get window ID for the plot
    WIDGET_CONTROL, wDraw, GET_VALUE=drawID
    
    ;Store certain widget information in "stash" struct
    stash = {drawID:drawID, PlotDensity:wPlotDensity, ShotNum: wShotNum, ZoomPlot:wZoomPlot, DensCalcBase: wBase}
    WIDGET_CONTROL, wBase, SET_UVALUE=stash

    ;Register the GUI (Hand control to X-Manager)
    XMANAGER, 'dens_limit_calculator', wBase
END