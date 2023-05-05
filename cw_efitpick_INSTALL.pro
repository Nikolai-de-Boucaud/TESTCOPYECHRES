@cw_efitpick_modules
print,'saving .compile file...'
save,/routines,file='cw_efitpick.compile'
print,'setting permissions to g+w...'
spawn,'chmod g+w cw_efitpick.compile'
print,'done.'
exit
