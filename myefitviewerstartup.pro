; An EFITViewer startup file for testing changes in a very small number of
; modules.  It restores the compiled efitviewer in the public area.  The 
; author of a specific module may then re-compile his/her own version 
; of the module.  This scheme works only when there is no conflicts of
; variables, structures, objects, etc.

; Get the path for startup files
  _EFITVIEW_PATH_ = getenv('EFITVIEW')
  if _EFITVIEW_PATH_ eq '' then _EFITVIEW_PATH_ = '.'

; restore compiled efit_viewer
  print,'restoring compiled efitviewer: '+_EFITVIEW_PATH_+'/efit_viewer.compile'
  restore,_EFITVIEW_PATH_+'/../efit/cw_efitpick.compile'
  restore,_EFITVIEW_PATH_+'/efit_viewer.compile'

; setup mdsplus
  status = mdsplus_setup()

; Either the next two lines:
;  print, 'compiling echres...'
;  .compile echres.pro
  
; OR the next two lines
restore, './echres.compile', description=comptime
print, 'restoring echres compiled on ' + comptime+' ...'

  
; start the application
  wid=efit_viewer()

exit
