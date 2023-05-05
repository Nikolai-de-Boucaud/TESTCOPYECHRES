pro efitviewer_vm

  ;setenv,'DEBUG=1'
  if (command_line_args())[0] ne '' then cd, (command_line_args())[0]

  ; Get the path for startup files
  _EFITVIEW_PATH_ = getenv('EFITVIEW')
  if _EFITVIEW_PATH_ eq '' then _EFITVIEW_PATH_ = '.'

  ; restore compiled echres
  _ECHRES_PATH_ = getenv('ECHRES_PATH')
  file = file_search(_ECHRES_PATH_+'/echres.compile')
  if (file[0] ne '') then begin
      print,'restoring '+file[0]
      restore,file[0],description=desc
      print,'echres description: ',desc 
  endif

  ; restore compiled nbiplot
  _NBIPLOT_PATH_ = getenv('NBIPLOT_PATH')
  file = file_search(_NBIPLOT_PATH_+'/nbiplot.compile')
  if (file[0] ne '') then begin
      print,'restoring '+file[0]
      restore,file[0]
  endif

  ; restore compiled ichres
  _ICHRES_PATH_ = getenv('ICHRES_PATH')
  file = file_search(_ICHRES_PATH_+'/ichres.compile')
  if (file[0] ne '') then begin
      print,'restoring '+file[0]
      restore,file[0]
  endif

  ; Restore this after the overlays, as those may have a bad version of ga_printer
  if (file_search(_EFITVIEW_PATH_+'/../general/ga_printer.compile'))[0] ne '' then $
  restore,_EFITVIEW_PATH_+'/../general/ga_printer.compile'

  ; start the application
  wid=efit_viewer()

end
