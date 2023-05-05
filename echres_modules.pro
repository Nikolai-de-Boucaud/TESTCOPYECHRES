.compile reg

;;; precompile some rsi routines
.compile spline
.compile deriv
.compile interpol
.compile xregistered
.compile gaussfit
.compile curvefit
.compile moment
.compile crossp
.compile read_ascii
.compile query_ascii
.compile array_indices
.compile oploterr
.compile loadct
.compile cw_fslider
.compile velovect

;;; compile Kupfer codes (order matters!!)
.compile $VERSION4D/EFITLIB/kupfer/idl_math/bispline
.compile $VERSION4D/EFITLIB/kupfer/idl_math/bispline_grid
.compile $VERSION4D/EFITLIB/kupfer/idl_math/qgauss
.compile $VERSION4D/EFITLIB/kupfer/idl_geqdsk/metric
.compile $VERSION4D/EFITLIB/kupfer/idl_math/linspace
.compile $VERSION4D/EFITLIB/kupfer/idl_math/qsimpv
.compile $VERSION4D/EFITLIB/kupfer/idl_geqdsk/contour_psi
.compile $VERSION4D/EFITLIB/kupfer/idl_geqdsk/fraction_circ
.compile $VERSION4D/EFITLIB/kupfer/idl_geqdsk/fluxmap

;;; some orginal /link/idl' now 'fusion/usc/link/idl' routines
;.compile gadat_check_type
;.compile mdsfindsig
;.compile get_ts
;.compile rho_rz
;.compile writeg

;;; some other public routines
.compile $IDLSOURCE/zipfit/get_fitted_profile
.compile $IDLSOURCE/zipfit/get_ts_99
.compile $IDLSOURCE/efitview/bfield_f


;;; some general purpose but local routines
.compile clear_struct
.compile stg
.compile get_flind
.compile readnc
.compile write_nl2
.compile read_nl2
.compile nbi12_8sources
.compile setintersection
.compile windowavailable
.compile interp

;;; ec-specific codes
.compile clear_com
.compile get_peak_abs
.compile npar_spread
.compile read_psiin
.compile write_psiin_echres
.compile write_echin_echres
.compile write_echin
.compile read_echin
.compile scale_equilibrium
.compile fluxfun_ech
.compile window_text_gaplot
.compile xmfrac
.compile config_source
.compile enter_prefs
.compile index
.compile snell

;;; some orginal /link/idl' now 'fusion/usc/link/idl' routines
;;; keep this compile line since I have the most up-to-date get_ech.pro, -xi 01/12/2016
.compile get_ech

;;; procedures unique to echres
.compile echcal
.compile strap
.compile antenna_angles
.compile find_pol
.compile nearest_index
.compile get_range
.compile ang
.compile write_setup_file
.compile read_MDS_setup
.compile read_setup_file
.compile update_echsys
.compile resolve_setup
.compile fill_com
.compile makeline_lite
.compile write_torayin
.compile run_toray_echres
.compile plot_toray
.compile plot_profiles
.compile minmaxb
.compile fixte
.compile x_frac
.compile rq
.compile test_counts_valid
.compile write_comfile
.compile write_netcdf
.compile write_cqlinput
.compile get_rya_toraync
.compile run_cql_echres
.compile combine_toraync
.compile auto_aimer
.compile survey_steering_angles
.compile plot_survey
.compile polazi_to_alphabeta
.compile alphabeta_to_polazi
.compile enter_ECH_data
.compile plot_density_limit
.compile makeline_2pts
.compile pnpoly_d3d_wall
.compile run_rayonwall
.compile plot_rayonwall
.compile plot_d3d_wall
.compile dens_limit_calculator

.compile echres
.compile auto_density_limit
.compile run_ech_res_timeslice

.compile plot_cql_echres
.compile plot_flux_echres
.compile tr_pros
.compile res_ellipse
.compile add_gyroname

;;; for timcon only:
.compile ech_launch_angles

comptime=systime()
save,description=comptime,/routine,filename='echres.compile',/xdr
print, '  *** Wrote file echres.compile '+comptime


exit
