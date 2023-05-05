; proc to read preferences files and enter data in the structure
; for use with echres, ichres, or nbiplot

pro enter_prefs, nlfile, str, overlay_name
; nlfile is a namelist file containing preferences and/or file paths
; str is the relevant structure
; overlay_name=['ECHRES' | 'NBIPLOT' |....] of calling efitviewer overlay

print, '  *** Reading file '+nlfile
if not file_test(nlfile) then begin
	print, '  !!! Could not find file '+nlfile
	return
endif

prefs=read_nl2(nlfile)
ptags=tag_names(prefs)
overlay=ptags[1]
tags=tag_names(prefs.(1))
if n_elements(overlay_name) gt 0 then overlay_name=strupcase(overlay_name) $
	else overlay_name='ECHRES'

;**************************************************************
; First, enter the general info

in=where(tags eq 'TOKAMAK')
if in[0] gt -1 then begin
	if overlay_name eq 'ECHRES' then begin
		str.Prefs.tokamak=strupcase(strap(prefs.(1).tokamak))
		print, '  *** Setting tokamak to ' +prefs.(1).tokamak+' set by '+nlfile
	endif
endif

in=where(tags eq 'SCRATCH_PATH')	; eg, '/cluster-scratch'
if in[0] gt -1 then begin
	fh=file_test(strap(prefs.(1).scratch_path))
	if not fh then begin
		print, '  !!! Could not find scratch path '+prefs.(1).scratch_path
		print, '      Using local directory instead'
		prefs.(1).scratch_path='.'
	endif
	scratch_path=strap(prefs.(1).scratch_path)
	if strpos(scratch_path, '/', /reverse_search) ne $
		strlen(scratch_path) then scratch_path += '/'
	if overlay_name eq 'ECHRES' then str.Prefs.scratch_path=scratch_path
	print, '  *** Setting scratch path to ' + str.Prefs.scratch_path + ' set by '+nlfile
endif

in=where(tags eq 'TORAY_PATH')
if in[0] gt -1 then begin
	if overlay_name eq 'ECHRES' then $
		str.toray_path=strap(prefs.(1).toray_path) else $
	if overlay_name eq 'NBIPLOT' then $
		str.prefs.toray_path=strap(prefs.(1).toray_path)
	print, '  *** Setting toray_path to '+strap(prefs.(1).toray_path)+' by '+nlfile
	if not file_test(strap(prefs.(1).toray_path)) then $
		print, '  !!! toray not found: '+strap(prefs.(1).toray_path)
endif

in=where(tags eq 'CQL3D_PATH')
if in[0] gt -1 then begin
	if overlay_name eq 'ECHRES' then $
		str.cql3d_path=strap(prefs.(1).cql3d_path) else $
	if overlay_name eq 'NBIPLOT' then $
		str.prefs.cql3d_path=strap(prefs.(1).cql3d_path)
	print, '  *** Setting CQL3D path to '+strap(prefs.(1).cql3d_path)+' by '+nlfile
	if not file_test(strap(prefs.(1).cql3d_path)) then print, $
		'  !!! cql3d not found: ', strap(prefs.(1).cql3d_path)
endif

in=where(tags eq 'PRINTER_NAME')
if in[0] gt -1 then begin
	if overlay_name eq 'ECHRES' then str.printer_name=strap(prefs.(1).printer_name) else $
	if overlay_name eq 'NBIPLOT' then str.text_printer_name=strap(prefs.(1).printer_name)
endif

; obsolete (used for non-DIII-D installations)
;in=where(tags eq 'PATHS_TO_FILES')
;if in[0] gt -1 then begin
;	print, '  *** Setting paths_to_files to '+prefs.(1).paths_to_files+' by '+nlfile
;	if not file_test(strap(prefs.(1).paths_to_files)) then $
;		message, 'Can not find required file '+prefs.(1).paths_to_files, /info
;	if overlay_name eq 'ECHRES' then str.paths_to_files=strap(prefs.(1).paths_to_files)
;endif

in=where(tags eq 'PROGRAM_HELP_PATH')
if in[0] gt -1 then begin
	if overlay_name eq 'ECHRES' then $
		str.prefs.program_help_path=strap(prefs.(1).program_help_path)
	if overlay_name eq 'NBIPLOT' then $
		str.prefs.instructions_path=strap(prefs.(1).program_help_path)
endif

in=where(tags eq 'INONE_TEMPLATE')
if in[0] gt -1 then begin
	if overlay_name eq 'ECHRES' then str.inone_template=strap(prefs.(1).inone_template)
endif

in=where(tags eq 'TORAYIN_TEMPLATE')
if in[0] gt -1 then begin
	if overlay_name eq 'ECHRES' then str.torayin_template=strap(prefs.(1).torayin_template)
	if not file_test(strap(str.torayin_template)) then print, $
		'  !!! toray.in_template not found'
endif

in=where(tags eq 'CQLINPUT_TEMPLATE')
if in[0] gt -1 then begin
	if overlay_name eq 'ECHRES' then str.cqlinput_template=strap(prefs.(1).cqlinput_template)
	if not file_test(strap(str.cqlinput_template)) then print, $
		'  !!! cqlinput_template file not found'
endif

in=where(tags eq 'ONETWO201_PATH')
if in[0] gt -1 then begin
	if overlay_name eq 'NBIPLOT' then $
		str.prefs.onetwo_path=strap(prefs.(1).onetwo201_path)
	print, '  *** Setting ONETWO path to '+prefs.(1).onetwo201_path+' by '+nlfile
	if not file_test(strap(prefs.(1).onetwo201_path)) then print, $
		'  !!! ONETWO executable not found'
endif

in=where(tags eq 'ONETWO51_PATH')
if in[0] gt -1 then begin

endif

;***********************************************************
; Then, ECHRES stuff
if overlay_name eq 'ECHRES' then begin

	in=where(tags eq 'IDAMP')
	if in[0] gt -1 then begin
		idamp=fix(prefs.(1).idamp[0])
		if idamp ne 1 and idamp ne 2 and idamp ne 8 then idamp=8
		str.Prefs.idamp=idamp
		print, '  *** Setting idamp to '+strtrim(idamp,2)+' set by '+nlfile
	endif

	in=where(tags eq 'DS')
	if in[0] gt -1 then begin
		ds=prefs.(1).ds[0]
		if ds lt 0.1 then ds=0.1
		if ds gt 5.0 then ds=5.0
		str.Prefs.ds=ds
		print, '  *** Setting ds to '+strtrim(ds,2)+' (cm) set by '+nlfile
	endif 

	in=where(tags eq 'DSMIN')
	if in[0] gt -1 then begin
		minds=prefs.(1).dsmin[0]
		if minds lt 0.05 then minds=0.05
		if minds gt ds then minds=ds
		str.Prefs.dsmin=minds
		print, '  *** Setting dsmin to '+strtrim(minds,2)+' (cm) set by '+nlfile
	endif

	in=where(tags eq 'SMAX')
	if in[0] gt -1 then begin
		maxs=prefs.(1).smax[0]
		if maxs lt 10.0 then maxs=10.0
		if maxs gt 1000.0 then maxs=1000.0
		str.Prefs.smax=maxs
		print, '  *** Setting smax to '+strtrim(maxs,2)+' (cm) set by '+nlfile
	endif
		
	in=where(tags eq 'POW_AVG_HALF_PERIOD') ; for ECH
	if in[0] gt -1 then begin
		if prefs.(1).pow_avg_half_period lt 0.1 then prefs.(1).pow_avg_half_period=0.1
		if prefs.(1).pow_avg_half_period gt 100. then prefs.(1).pow_avg_half_period=100.
		str.Prefs.pow_avg_half_period=prefs.(1).pow_avg_half_period
		print, '  *** Setting power averging time (half period) to ' + $
			strtrim(prefs.(1).pow_avg_half_period,2) + ' (msec) set by '+nlfile
	endif

	in=where(tags eq 'ECHRES_PRESENT_SETUP')
	if in[0] gt -1 then begin
		str.Prefs.present_setup_path=strap(prefs.(1).echres_present_setup)
		if not file_test(strap(prefs.(1).echres_present_setup)) then print, $
			'  !!! present setup file not found: '+ prefs.(1).echres_present_setup
	endif
	
	in=where(tags eq 'PRESENT_SETUP')	; path to present setup file
	if in[0] gt -1 then begin
		str.Prefs.present_setup_path=strap(prefs.(1).present_setup)
		if not file_test(strap(prefs.(1).present_setup)) then print, $
			'  !!! present setup file not found: '+ prefs.(1).present_setup
	endif
	
	in=where(tags eq 'FIRST_SHOT_ALLOWED')
	if in[0] gt -1 then str.Prefs.first_shot_allowed=prefs.(1).first_shot_allowed

	in=where(tags eq 'USE_ALPHABETA')
	if in[0] gt -1 then begin
		uab=strap(prefs.(1).use_alphabeta)
		if uab eq '.T.' or uab eq '.TRUE.' or uab eq 'T' or uab eq 'TRUE' then $
			PrgDat.Prefs.use_alphabeta=1B else PrgDat.Prefs.use_alphabeta=0B
	endif

	in=where(tags eq 'ECHRES_INSTRUCTIONS')
	if in[0] gt -1 then begin
		str.Prefs.program_help_path=strap(prefs.(1).echres_instructions)
		if not file_test(strap(prefs.(1).echres_instructions)) then print, $
			'  !!! echres help file not found: '+ prefs.(1).echres_instructions
    endif
	
	in=where(tags eq 'ECHRES_INONE_TEMPLATE')
	if in[0] gt -1 then begin
		str.inone_template=strap(prefs.(1).echres_inone_template)
		if not file_test(strap(prefs.(1).echres_inone_template)) then print, $
			'  !!! echres inone template file not found: '+ prefs.(1).echres_inone_template
	endif
	
	in=where(tags eq 'ECHRES_TORAYIN_TEMPLATE')
	if in[0] gt -1 then begin
		str.torayin_template=strap(prefs.(1).echres_torayin_template)
		if not file_test(strap(prefs.(1).echres_torayin_template)) then print, $
			'  !!! echres toray.in template file not found: '+ prefs.(1).echres_torayin_template
	endif
	
	in=where(tags eq 'ECHRES_CQLINPUT_TEMPLATE')
	if in[0] gt -1 then begin
		str.cqlinput_template=strap(prefs.(1).echres_cqlinput_template)
		if not file_test(strap(prefs.(1).echres_cqlinput_template)) then print, $
			'  !!! echres cqlinput template file not found: '+ prefs.(1).echres_cqlinput_template
	endif
	
endif ; end ECHRES


;*****************************************************************
; NBIPLOT

if overlay_name eq 'NBIPLOT' then begin

	in=where(tags eq 'NUBEAM_DRIVER_PATH')
	if in[0] gt -1 then begin
		str.prefs.nubeam_driver_path=strap(prefs.(1).nubeam_driver_path)
		if not file_test(strap(prefs.(1).nubeam_driver_path)) then print, $
			'  !!! nubeam driver file not found: '+prefs.(1).nubeam_driver_path
	endif
	
	in=where(tags eq 'INONE_NBIPLOT_TEMPLATE')
	if in[0] gt -1 then begin
		str.prefs.inone_nbiplot_template=strap(prefs.(1).inone_nbiplot_template)
		if not file_test(strap(prefs.(1).inone_nbiplot_template)) then print, $
			'  !!! file inone_nbiplot_template not found: '+prefs.(1).inone_nbiplot_template
	endif
	
	in=where(tags eq 'NBIPLOT_INSTRUCTIONS')
	if in[0] gt -1 then begin
		str.prefs.instructions_path=strap(prefs.(1).nbiplot_instructions)
		if not file_test(strap(prefs.(1).nbiplot_instructions)) then print, $
			'  !!! file nbiplot_instructions not found: '+prefs.(1).nbiplot_instructions
	endif
	
	in=where(tags eq 'RPC_CONFIG_TEMPLATE')
	if in[0] gt -1 then begin
		str.prefs.rpc_config_template=strap(prefs.(1).rpc_config_template)
		if not file_test(strap(prefs.(1).rpc_config_template)) then print, $
			'  !!! file rpc_config_template not found: '+prefs.(1).rpc_config_template
	endif
	
endif ; end NBIPLOT

;******************************************************************
; ICHRES

if overlay_name eq 'ICHRES' then begin

	in=where(tags eq 'GENRAY_PATH')
	if in[0] gt -1 then begin
		str.genray_com=strap(prefs.(1).genray_path)+' > logg'
		if not file_test(strap(prefs.(1).genray_path)) then print, $
			'  !!! genray executable not found: '+prefs.(1).genray_path
	endif
	
	in=where(tags eq 'ONETWO201_PATH')
	if in[0] gt -1 then begin
		str.onetwo_com=strap(prefs.(1).onetwo201_path)+' > log12'
		if not file_test(strap(prefs.(1).onetwo201_path)) then print, $
			'  !!! onetwo executable not found: '+prefs.(1).onetwo201_path
	endif
	
	in=where(tags eq 'ICHRES_GENRAYDAT1')
	if in[0] gt -1 then begin
		
	endif

	in=where(tags eq 'ICHRES_GENRAYDAT2')
	if in[0] gt -1 then begin
		
	endif

	in=where(tags eq 'ICHRES_INONE_TEMPLATE')
	if in[0] gt -1 then begin
		
	endif

endif ; end ICHRES

return

end
