if (!VERSION.OS_FAMILY eq 'unix') then begin                                          & $
  ;; efitviewer path				                                      & $
  IF getenv('EFITVIEW') EQ '' THEN setenv,'EFITVIEW='+getenv('IDLSOURCE')+'/efitview' & $
  ;; iefit path				                                              & $
  IF getenv('IEFIT') EQ '' THEN setenv,'IEFIT='+getenv('IDLSOURCE')+'/iefit'          & $
endif
                   
;; MDS INIT 
@$IDLSOURCE/data/mdsplus_modules.pro
	
;; EFIT PICKER ROUTINES
@$IDLSOURCE/efit/cw_efitpick_modules.pro	

;; EFITENV MODULES (shared)
@$IDLSOURCE/efitenv/efitenv_core_modules.pro
			
;; EFITVIEWER MODULES (only)	
@$IDLSOURCE/efitview/efit_viewer_only_modules.pro

