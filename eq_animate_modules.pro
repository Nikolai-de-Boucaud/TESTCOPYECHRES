; GENERAL IDL ROUTINES 

print,'Compiling eq_animate modules'
.compile str_sep 
.compile colormap_applicable
.compile cw_animate
.compile cw_bgroup
.compile cw_field
.compile xpdmenu
.compile xmanager
.compile mpeg_put
.compile mpeg_save
.compile mpeg_close

; MDS INIT
@$IDLSOURCE/data/mdsplus_modules.pro

.compile $IDLSOURCE/data/set_experiment.pro
.compile $IDLSOURCE/general/color_setup.pro
.compile $IDLSOURCE/general/edit_entry.pro
.compile $IDLSOURCE/general/struct_hastag.pro
.compile $IDLSOURCE/general/getenv_.pro
.compile $VERSION4D/EFITLIB/kupfer/idl_math/bispline.pro
.compile $IDLSOURCE/ga_plot_new/position.pro
.compile $IDLSOURCE/data/mdsevent__define.pro
.compile $IDLSOURCE/data/mdsevent_handler__define.pro
.compile $IDLSOURCE/efitview/efit_mdsevent_handler__define.pro
.compile $IDLSOURCE/efitview/eq_plot.pro
.compile $IDLSOURCE/efitview/eq_animate.pro

