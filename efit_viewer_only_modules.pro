;; The routne get_fitted_profile uses serveral include files from $VERSION4D/FITTING
!path = !path+':'+getenv('VERSION4D')+'/FITTING/'    

;; Fitting Routines
.compile $VERSION4D/EFITLIB/polyfit.pro
.compile $VERSION4D/FITTING/makeline.pro
.compile $VERSION4D/FITTING/PLASMA_ROTATION/bfield.pro
.compile $VERSION4D/EFITLIB/kupfer/idl_math/qsimpv.pro

;; Gadat, last resort
.compile $IDLSOURCE/data/mdsfindsig.pro
.compile $IDLSOURCE/data/gadat_get_data.pro
.compile $IDLSOURCE/data/gadat_find_library.pro
.compile $IDLSOURCE/data/gadat_check_type.pro
.compile $IDLSOURCE/data/gadat_set_libidl.pro
.compile $IDLSOURCE/data/gadat_make_executable.pro
.compile $IDLSOURCE/data/gadat.pro
.compile $IDLSOURCE/data/gadat2.pro
.compile $IDLSOURCE/data/getallpcdata.pro
.compile $IDLSOURCE/general/find_library.pro
.compile $VERSION4D/UTILITIES/set_libidl.pro
.compile $VERSION4D/MDSPLUS/mds4d_points.pro
.compile $VERSION4D/FITTING/get_cer_beam_order.pro

;; Edit_entry used by animation
.compile $IDLSOURCE/general/edit_entry.pro

;; Data routines for overlay
.compile $IDLSOURCE/data/get_cergeom.pro
.compile $IDLSOURCE/general/get_ece_freq.pro

;; Configure diagnostic overlays
.compile xregistered.pro
.compile mean.pro
.compile gamma
.compile $VERSION4D/UTILITIES/f_debug.pro
.compile $VERSION4D/UTILITIES/f_write_cache.pro
.compile $IDLSOURCE/zipfit/get_fitted_profile.pro
.compile $IDLSOURCE/general/interpp.pro
.compile $IDLSOURCE/general/get_ece.pro
.compile $IDLSOURCE/general/setdifference.pro
.compile $IDLSOURCE/general/listmatch.pro
.compile $IDLSOURCE/general/nearest_index.pro
.compile $EFITVIEW/diagnoses_config.pro
diagnoses_config

;; Compile diagnostics overlays
@$EFITVIEW/diagnoses_modules.pro

;; Efit_viewer routines
.compile $EFITVIEW/sync_color.pro
.compile $EFITVIEW/eq_plot.pro            
.compile $EFITVIEW/eq_animate.pro
.compile $EFITVIEW/getafiles.pro
.compile $EFITVIEW/bfield_pol.pro           
.compile $EFITVIEW/bfield_f.pro           
.compile $EFITVIEW/calculate_bfieldsg.pro
.compile $EFITVIEW/get_ecei_loc.pro
.compile $EFITVIEW/get_ecei_loc_mds.pro
.compile $EFITVIEW/get_mseports.pro          
.compile $EFITVIEW/mse_plot.pro           
.compile $EFITVIEW/rho_map.pro
.compile $EFITVIEW/cw_pointname.pro       
.compile $EFITVIEW/overlay_plot.pro       
.compile $EFITVIEW/all_plots.pro
.compile $EFITVIEW/cw_runefit.pro
.compile $EFITVIEW/diagnoses_table.pro
.compile $EFITVIEW/efit_viewer.pro        



