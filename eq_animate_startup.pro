;; Get $IDLSOURCE
_IDLSOURCE_ = getenv('IDLSOURCE')

;; Log the usage
logfile = _IDLSOURCE_+'/efitview/efitanimate.log'
spawn,['uname','-n'],host,/noshell  
openw, unit, logfile, /get_lun, /append
printf, unit, format='(a8," ",a6,"  ",$)', $
strupcase(getenv('LOGNAME')),strupcase(host[0])
printf, unit, systime(),' ',getenv('PWD')
free_lun, unit

restore,_IDLSOURCE_+'/efitview/eq_animate.compile'

mdsconnect,'atlas.gat.com'
setenv,'CHARSIZE=2'
eq_animate,/AutoUpdate,/TearOff

