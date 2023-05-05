;Function that replicates the 'do_the_plot" routine in echres.pro
PRO do_the_plot_dcalc
    
    Common ECHRES_INPUTS
    Common ECHCOM
    Common REFLECTION_CHECK
    Common DENS_LIM_AUTO
    Common DensWidIds

    ;allows code to access widget info on ECHRES panel
    info = echres_uval 

	; put the calculated values into the output table:
	widget_control, info.WidIDs.InputTable, set_value=ECHSys.InputTable
	widget_control, info.WidIDs.Eqb, set_value=PrgDat.shottime
	widget_control, info.WidIDs.CentralDensity, set_value=PrgDat.Profiles.CentralDensity
	widget_control, info.WidIDs.CentralTe, set_value=PrgDat.Profiles.CentralTe
	widget_control, info.WidIDs.Bt0, set_value=PrgDat.Profiles.Bt0
	PrgDat.Status.OutputsValid=1
	
	if PrgDat.show_toray_window then begin
		PrgDat.show_toray_window=0
		wshow, PrgDat.toray_window
	endif

END

;Function that replicates the 'get_polarizer_setttings' routine in echres.pro
PRO get_polarizer_settings_dcalc

    Common ECHRES_INPUTS
    Common ECHCOM
    Common REFLECTION_CHECK
    COMMON DENS_LIM_AUTO
    Common DensWidIds
    
    ;allows code to access widget info on ECHRES panel
    info = echres_uval 

	if PrgDat.Prefs.nlinputfile ne '' then return	; don't know the gamma matrix

	widget_control, /hourglass

        kk=where(PrgDat.Status.PlotRays EQ 1)
	for num_kk=0, n_elements(kk)-1 do begin	
	    jj=kk[num_kk]
		if (PrgDat.Status.PlotRays[jj] and (ECHSys.InputTable[jj].PolarAng gt 10.)) then begin
		
			; if a gyro has no data for this shot, then clear the table:
			if ECHSys.Tank[jj].TankNo le 0 or ECHSys.Tank[jj].AntNum le 0 then begin
				ECHSys.InputTable[jj].xfrac=0.0
				ECHSys.InputTable[jj].polarang=0.0
				ECHSys.InputTable[jj].aziang=0.0
				ECHSys.InputTable[jj].polcts=0
				ECHSys.InputTable[jj].torcts=0
				ECHSys.InputTable[jj].pol1=0.0
				ECHSys.InputTable[jj].pol2=0.0
				ECHSys.InputTable[jj].F_abs=0.0
				ECHSys.InputTable[jj].rho_peak=0.0
				ECHSys.InputTable[jj].eccd=0.0
				goto, next_line 
			endif
		
			if ECHSys.Tank[jj].xfrac lt 0.5 then begin
				omo=1
				alpha_0=ECHSys.Antenna[jj].alpha_x+90.
				beta_0=-ECHSys.Antenna[jj].beta_x
				if alpha_0 LT -90. then alpha_0=alpha_0+180. else $ ; get alpha bet 0 and pi
					if alpha_0 GT 90. then alpha_0=alpha_0-180. 
			endif else begin
				omo=0
				alpha_0=ECHSys.Antenna[jj].alpha_x
				beta_0=ECHSys.Antenna[jj].beta_x
			endelse

			GetPolSettings, $ 
				ECHSys.XLine[jj].alpha0*!dtor, ECHSys.XLine[jj].beta0*!dtor, $
				ECHSys.XLine[jj].gm1, ECHSys.XLine[jj].gm2, $
				ECHSys.XLine[jj].mir_type, ECHSys.Xline[jj].gammasd*!dtor, $
				ECHSys.XLine[jj].delay, alpha_0*!dtor, beta_0*!dtor, $
				ECHSys.Antenna[jj].Tor_Ang*!dtor, ECHSys.Antenna[jj].Pol_Ang*!dtor, $
				ECHSys.Antenna[jj].pol_id*!dtor, polvalues

			ind=where((polvalues[3,*] gt 0.01) and (polvalues[2,*] gt 99.9) )
			if ind[0] lt 0 then $
				ind=where((polvalues[3,*] gt 0.01) and (polvalues[2,*] gt 99.75) )
			if ind[0] lt 0 then $
				ind=where((polvalues[3,*] gt 0.01) and (polvalues[2,*] gt 99.5) )
			if ind[0] lt 0 then $
				ind=where((polvalues[3,*] gt 0.01) and (polvalues[2,*] gt 99.0) )
			if ind[0] lt 0 then $
				ind=where((polvalues[3,*] gt 0.01) and (polvalues[2,*] gt 98.5) )
			if ind[0] lt 0 then $
				ind=where((polvalues[3,*] gt 0.01) and (polvalues[2,*] gt 98.0) )
			ind=where(min(polvalues[3,ind]) eq polvalues[3,ind])
			if ind[0] ge 0 then begin
				ECHSys.InputTable[jj].Pol1=polvalues[0,ind[0]]
				ECHSys.InputTable[jj].Pol2=polvalues[1,ind[0]]
				if omo then ECHSys.InputTable[jj].xfrac=100.-polvalues[2,ind[0]] else $
					ECHSys.InputTable[jj].xfrac=polvalues[2,ind[0]]
				ECHSys.Antenna[jj].alpha_a=polvalues[4,ind[0]]
				ECHSys.Antenna[jj].beta_a=polvalues[5,ind[0]]
			endif else message, 'Could not find polarizer settings for system '+strtrim(jj+1,2)
			
		endif
		next_line:
	endfor

	PrgDat.Status.OutputsValid=0
	widget_control, info.WidIDs.ConfigTable, set_value=ECHSys.Tank
	widget_control, info.WidIDs.InputTable, set_value=ECHSys.InputTable
	PrgDat.Status.ArchivedData=0	; user changed polarizers
	widget_control, info.WidIDs.Archive, set_value=PrgDat.Status.ArchivedData
		
	do_the_plot_dcalc
END

;Function that replicates hitting the "GET MDS ECH SETUP" button on the ECHRES panel 
PRO read_mds_dcalc

    Common ECHRES_INPUTS
    Common ECHCOM
    Common REFLECTION_CHECK
    COMMON DENS_LIM_AUTO
    Common DensWidIds

    ;allows code to access widget info on ECHRES panel
    info = echres_uval 
	
	widget_control, /hourglass
	
	strgshot=strtrim(gstruct.shot,2)
	gshot=long(strgshot.substring(0,5))
	PrgDat.config_shot=gshot

	Clear_Com	; clear tables for new input
	PrgDat.Prefs.nlinputfile=''
	read_MDS_setup, PrgDat.Config_Shot, success=success
	
	if success then begin	; get polarization purity here and put in inputtable

		; set ArchivedData button to reflect the success of the read
		PrgDat.Status.ArchivedData=1
		widget_control, info.WidIDs.Archive, set_value=PrgDat.Status.ArchivedData
		
		; update the tables
		widget_control, info.WidIDs.ConfigTable, set_value=ECHSys.Tank
		widget_control, info.WidIDs.InputTable, set_value=ECHSys.InputTable
			
		; designate the outputs table as invalid
		PrgDat.Status.OutputsValid=0
		widget_control, info.WidIDs.Eqb, set_value=PrgDat.shottime
			
		; set the likely antenna numbers for ray tracing
		widget_control, info.WidIds.RayTrace, set_value=PrgDat.Status.PlotRays
		if PrgDat.config_shot ge PrgDat.first_shot_newest_config then $
			widget_control, info.WidIDs.UsePresent, set_value=1 else $
			widget_control, info.WidIDs.UsePresent, set_value=0

		uniq_freq=ECHSys.Tank[uniq(ECHSys.Tank.freq, sort(ECHSys.Tank.freq))].freq
		ifrq=where(uniq_freq gt 0., count)
		if count gt 0 then uniq_freq=uniq_freq[ifrq]
		if n_elements(uniq_freq) gt 3 then begin
			print, '  !!! Maximum of 3 unique frequencies allowed.'
			return
		endif
		PrgDat.n_freq=n_elements(uniq_freq)
		PrgDat.unique_freq[*]=0.
		PrgDat.unique_freq[0:PrgDat.n_freq-1]=uniq_freq
		PrgDat.status.recalc_rays[*]=1
		widget_control, info.WidIDs.inputtable, $
			use_table_select=[4,0,5,n_elements(ECHSys.Tank)-1], $
			foreground_color=[0,0,0]
		if PrgDat.counts_valid_window ge 0 then begin
			wdelete, PrgDat.counts_valid_window
			PrgDat.counts_valid_window = -1			
		endif
		
		; reset nharm for DIII-D
		ECHSys.Tank[0:ECHSys.NumTanks-1].nharm=2

		do_the_plot_dcalc

	endif else begin ; MDSplus data not available

		Config_Source, info ; pick alternative if desired

	endelse

END

;Function that replicates hitting the 'READ SETUP FILE' button on the ECHRES panel
PRO read_setup_file_dcalc

    Common ECHRES_INPUTS
    Common ECHCOM
    Common REFLECTION_CHECK
    COMMON DENS_LIM_AUTO
    Common DensWidIds

    ;allows code to access widget info on ECHRES panel
    info = echres_uval 

	res=dialog_message(['Read in ECH Config setup file?'], /question)
	if res eq 'Yes' then setupdata=read_setup_file(setup_dir='/fusion/projects/d3dops/ech/configs/') else $
	setupdata=read_setup_file(setup_dir=PrgDat.Setup_Dir)
	if not setupdata.success then begin
		message, 'Did not get setup file', /info
		return
	endif
	
	PrgDat.Setup_Dir=setupdata.setup_dir
	
	usetable=0
	
	readfile:
	
	if setupdata.shot ge PrgDat.First_Shot_Newest_Config then begin
		PrgDat.config_shot=PrgDat.First_Shot_Newest_Config+1
		widget_control, info.WidIDs.UsePresent, set_value=1
	endif else begin
		PrgDat.config_shot=setupdata.shot
		widget_control, info.WidIDs.UsePresent, set_value=0
	endelse
	
	PrgDat.Prefs.nlinputfile=''
	PrgDat.Status.ArchivedData=0
	widget_control, info.WidIDs.Archive, set_value=PrgDat.Status.ArchivedData
	
	if not usetable then begin	; enter all data from present.setup file
	
		clear_com	; clear tables for all new input	
		configdata=resolve_setup(setupdata)
		if configdata.success then fill_com, configdata, success=success else $
			message, 'resolve_setup failed'
		if not success then message, 'fill_com failed'
		
	endif else begin	; keep pol and azi angles that are present in table already
		
		; first, look for gyrotrons in the old AND present lists
		takenold=intarr(ECHSys.NumTanks)
		takenpresent=intarr(setupdata.num_sys)

		ECHSys.Tank[*].Power_MW=0

		for i=0, setupdata.num_sys-1 do begin
			k=where(strmid(strupcase(ECHSys.Tank.gyroname), 0, 3) eq $
				strmid(strupcase(setupdata.gyroname[i]),0,3), nmatch)
				
			IF nmatch EQ 1 THEN BEGIN
				takenold[k]=1
				takenpresent[i]=1		
				enter_ECH_data, k
			ENDIF				
		ENDFOR
		
		kold=where(takenold eq 0, nold)
		kpres=where(takenpresent eq 0, npres)

		;;;replacing the unavailable old gyro with the present gyro		
		IF (npres GT 0) AND (nold GT 0) THEN BEGIN
			for i=0, nold-1 do begin
				j=where(takenpresent eq 0, nj)
				IF nj gt 0 THEN BEGIN
				   ECHSys.Tank[kold[i]].gyroname=setupdata.gyroname[j[0]]
				   enter_ECH_data, kold[i]
				   takenold[kold[i]]=1
				   takenpresent[j[0]]=1
				ENDIF
			endfor
			kold=where(takenold eq 0, nold)
			kpres=where(takenpresent eq 0, npres)
		END
				
		;;;appending the left 'new' present gyro to the end of the ECHSys	
		IF (npres GT 0) AND (nold EQ 0) THEN BEGIN
			FOR i=0, npres-1 DO BEGIN
				ECHSys.Tank[n_elements(takenold)+i].gyroname=setupdata.gyroname[kpres[i]]
				ECHSys.InputTable[n_elements(takenold)+i].gyroname=setupdata.gyroname[kpres[i]]
				ECHSys.InputTable[n_elements(takenold)+i].PolarAng=90.-2*i ;;;;hardcoded an initial value for new gyro
				ECHSys.InputTable[n_elements(takenold)+i].AziAng=180. ;;;;hardcoded an initial value for new gyro
				ECHSys.XLine[n_elements(takenold)+i].alpha0=90. ;;;;hardcoded an initial value for new gyro
				PrgDat.Status.PlotRays[n_elements(takenold)+i] = 1
				PrgDat.Status.recalc_rays[n_elements(takenold)+i] = 1
				enter_ECH_data, n_elements(takenold)+i
			ENDFOR 			
		ENDIF
	
		;;;removing the gyrotrons that are not available now		
		IF (npres EQ 0) AND (nold GT 0) THEN BEGIN 
			for i=0, nold-1 do begin
				ECHSys.Tank[kold[i]].gyroname=''
				ECHSys.Tank[kold[i]].freq=0.
				ECHSys.Tank[kold[i]].nharm=0.
				ECHSys.InputTable[kold[i]].gyroname=''
				ECHSys.InputTable[kold[i]].pol1=0
				ECHSys.InputTable[kold[i]].pol2=0
				enter_ECH_data, kold[i], /empty
				PrgDat.Status.Plotrays[kold[i]]=0
			endfor	
		ENDIF
		
		ECHSys.NUMTANKS=setupdata.NUM_SYS
	
	endelse
	
	; put data into the inputs table
	widget_control, info.WidIDs.ConfigTable, set_value=ECHSys.Tank

	; designate the outputs table as invalid
	PrgDat.Status.OutputsValid=0

	; set the likely antenna numbers for ray tracing
	widget_control, info.WidIds.RayTrace, set_value=PrgDat.Status.PlotRays
	PrgDat.Status.recalc_rays[*]=1
	
	; reset nharm for DIII-D
	ECHSys.Tank[0:ECHSys.NumTanks-1].nharm=2

	widget_control, info.WidIDs.inputtable, $
		use_table_select=[4,0,5,n_elements(ECHSys.Tank)-1], $
		foreground_color=[0,0,0] 
		
	;;check whether it's within the port limits
    for i=0, setupdata.num_sys-1 do begin
        IF (strpos(setupdata.gyroname[i], 'T') eq 0) THEN BEGIN 
            in=1 
        ENDIF ELSE BEGIN
        in=test_counts_valid(PrgDat.config_shot, $ ; returns true if counts valid
            ECHSys.InputTable[i].PolCts, $
            ECHSys.InputTable[i].TorCts, $
            ECHSys.InputTable[i].gyroname, -1, i+1, /noplot)
        ENDELSE
        if in then begin
            widget_control, info.WidIDs.inputtable, $
                use_table_select=[4,i,5,i], $
                foreground_color=[0,0,0]
        endif else widget_control, info.WidIDs.inputtable, $
            use_table_select=[4,i,5,i], $			
            foreground_color=[255,0,0]
    endfor
	
	if usetable then begin
        get_polarizer_settings_dcalc
    endif else begin 
        do_the_plot_dcalc
    endelse
	
END

;Function that replciates hitting the 'ZIPFITS' button on the ECHRES panel
PRO get_zipfits_dcalc

    Common ECHRES_INPUTS
    Common ECHCOM
    Common REFLECTION_CHECK
    COMMON DENS_LIM_AUTO
    Common DensWidIds

    ;allows code to access widget info on ECHRES panel
    info = echres_uval 

    dtr=!PI/180.
    rho_kin=get_range(0.,1.,rad_res)

    ;imitate case structure like original function
    test = 1
    case test of

        1: begin

            print, '  *** Getting zipfits...'
                
            ; first enter Te knots from MDS+ zip fits
            PrgDat.Profiles.Source='zipfits'
            
            te_str=get_fitted_profile(PrgDat.shot, PrgDat.time, 'etemp', ierr, /single_slice)
            if (ierr ne 0) then begin
                message, 'T_e zipfit not available.', /info
                enein=fltarr(rad_res)
                tein=enein
                zeffin=enein
                widget_control, info.WidIDs.ProfSource, set_value=0
                break
            endif

            if te_str.redchisq gt 10. then $
                print, '  !!! T_e profile has redchisq ' + $
                    strtrim(string(te_str.redchisq, format='(f10.1)' ),2)

            ilto=where(te_str.rho le 1.0)
            rho=te_str.rho[ilto]
            rtein=rho_kin
            tein=spline(te_str.rho, te_str.data[ilto], rho_kin)
            njte=rad_res

            ; fix Te profile if necessary 
            teinstr=fixte(rtein, tein,/echres)
            tein=teinstr.te

            ; now enter density values
            ierr=0
            ne_str=get_fitted_profile(PrgDat.shot, PrgDat.time, 'edensn', ierr, /single_slice)
            IF (ierr ne 0) THEN BEGIN 
                ne_str=get_fitted_profile(PrgDat.shot, PrgDat.time, 'edens', ierr, /single_slice)
                print, '  !!! No CO2 normalization'
            ENDIF
            if (ierr ne 0) then begin
                message, 'n_e zipfit not available', /info
                enein=fltarr(rad_res)
                tein=enein
                zeffin=enein
                widget_control, info.WidIDs.ProfSource, set_value=0
                break
            endif

            if ne_str.redchisq gt 12.5 then $
                print, '  !!! n_e profile has redchisq ' + $
                    strtrim(string(ne_str.redchisq, format='(f10.1)' ),2)	
            renein=rho_kin
            enein=spline(ne_str.rho, ne_str.data, rho_kin)*1.e13
            njene=rad_res

            ; now enter zeff knots
            ; too many problems with zeff from get_fitted profs
            rzeffin=rho_kin
            zeffin=fltarr(rad_res)
            zeffin[*]=1.75
            njzef=rad_res
        end
    
    endcase

    ; check that the outer part of enein is monotonically decreasing, 
	; so that toray doesn't make ray go out instead of in
	for ik=rad_res-4, rad_res-2 do $
		if enein[ik+1] ge enein[ik] then enein[ik+1]=0.9*enein[ik]	
	
	; enter the data into com
	PrgDat.Profiles.njte=rad_res
	PrgDat.Profiles.njene=rad_res
	PrgDat.Profiles.njzef=rad_res
	PrgDat.Profiles.renein[*]=rho_kin
	PrgDat.Profiles.rtein[*]=rho_kin
	PrgDat.Profiles.rzeffin[*]=rho_kin
	PrgDat.Profiles.enein[*]=0.
	PrgDat.Profiles.tein[*]=0.
	PrgDat.Profiles.zeffin[*]=0.
	PrgDat.Profiles.enein[0:rad_res-1]=enein
	PrgDat.Profiles.tein[0:rad_res-1]=tein
	PrgDat.Profiles.zeffin[0:rad_res-1]=zeffin
	PrgDat.Profiles.CentralDensity=PrgDat.Profiles.enein[0]
	PrgDat.Profiles.InputDensity=PrgDat.Profiles.CentralDensity
	PrgDat.Profiles.CentralTe=PrgDat.Profiles.tein[0]
	PrgDat.Profiles.InputTe=PrgDat.Profiles.CentralTe
	PrgDat.Profiles.ShotCentralDensity=enein[0]
	PrgDat.Profiles.ShotCentralTe=tein[0]
	PrgDat.Status.OutputsValid=0	; check if cutoffs should be calculated
	
	skip_prof_entry:
	widget_control, info.WidIDs.CentralDensity, set_value=PrgDat.Profiles.CentralDensity
	widget_control, info.WidIDs.CentralTe, set_value=PrgDat.Profiles.CentralTe
	
	WIDGET_CONTROL, zipfits_id, set_value=PrgDat.Profiles.CentralDensity
	
	do_the_plot_dcalc

END

;Function that replicates hitting the 'AUTO REFL DENS' button on the ECHRES panel
PRO auto_density_dcalc

    Common ECHRES_INPUTS
    Common ECHCOM
    Common REFLECTION_CHECK
    COMMON DENS_LIM_AUTO
    Common DensWidIds
	Common AutoWidIDs

	widget_control, /hourglass
	
	if PrgDat.gfile eq '' then begin
		res=dialog_message('Pick equilibrium first')
		return
	endif
	if ECHSys.NumTanks eq 0 then begin
		res=dialog_message('Get ECH setup first')
		return
	endif
	if PrgDat.Profiles.enein[0] lt 1. then begin
		res=dialog_message('Pick kinetic profiles first')
		return
	endif

	;close the 'TORAY Plot Window' launched from previous run
	pltorayid=LookupManagedWidget('plot_toray')
	IF pltorayid NE 0L THEN widget_control,pltorayid,/destroy
	
	; kill cql3d window and clear data, if open
	if xregistered('plot_cql_echres') then begin
		widget_control, PrgDat.cql_windowID, /destroy
		PrgDat.cql_pwr[*]=0.
		PrgDat.cql_cur[*]=0.
		PrgDat.cql_rya[*]=0.
		PrgDat.cql_surf=10
		PrgDat.cql_totpwr=0.
		PrgDat.cql_totcur=0.
		PrgDat.cql_mode=0
		PrgDat.cql_window=-1
		PrgDat.cql_b_fudge_factor=1.0
		PrgDat.cql_plotresonance=1
		PrgDat.cql_quadrant=0
		PrgDat.cql_done=0
	endif

	;close the opened 'Unabsorbed EC Power on the Wall' window when'run_Toray' is clicked
	raywallid=LookupManagedWidget('plot_rayonwall')
	IF raywallid NE 0L THEN widget_control,raywallid,/destroy

	;close auto dens gui if it is already open
	if ISA(auto_stash) eq 1 then begin
		if WIDGET_INFO(auto_stash.AutoBase, /VALID_ID) eq 1 then begin
			WIDGET_CONTROL, auto_stash.AutoBase, /Destroy
		endif
	endif

	;run auto_density_limit function
    auto_density_limit

	;automatically hit "RUN" button
	WIDGET_CONTROL, auto_stash.Run, SEND_EVENT={id:0L, top:0L, handler:0l}

END

;Main function that calculates the ECH Density Limit based on a given shot and time slice
PRO run_ech_res_timeslice, shot, time, use_mds, find_dens
    Common ECHRES_INPUTS
    Common ECHCOM
    Common REFLECTION_CHECK
    COMMON DENS_LIM_AUTO
    Common DensWidIds
   
    MDSconnect, 'atlas.gat.com'
    mdsopen, 'EFIT01', shot

    ;Pull adata and gdata for given shot
    adata = reada(shot, time, RUNID='EFIT01', EXACT_TIME='true', mode='mdsplus')
    gdata = readg(shot, time, RUNID='EFIT01', EXACT_TIME='true', mode='mdsplus')

    ;Open ECHRes
    v=echres(adata, gdata, bid=bid_ECHres, color=color_ECHres, verbose=verbose_ECHres, callback=callback_ECHres, parameter=parameter_ECHres, cancel_event=cancel_event_ECHres)

	;Only calculate density if MDS or Setup File buttons are hit (not load ECHRES)
	if find_dens eq 1B then begin

		;Run 'read_mds_dcalc' if MDS button is hit or run 'read_setup_file_dcalc' if Setup File button is hit
		if use_mds eq 1B then begin
			read_mds_dcalc
		endif else begin
			read_setup_file_dcalc
		endelse

		;Get zipfits data
		get_zipfits_dcalc

		;Run automated reflection density function
		auto_density_dcalc

	endif

end