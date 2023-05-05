path = getenv('WRITEGPATH')
if ((path ne '') and strmid(path,strlen(path)-1,1) ne '/') then path=path+'/'

args = getenv('WRITEGARGS')
if (args eq '') then begin & $
  shot = 0l & $
  tree = '' & $
  time = 0.0 & $
  read,'SHOT: ',shot & $
  read,'TREE: ',tree & $
  read,'TIME: ',time & $
  args = strtrim(shot,2)+':'+tree+':'+strtrim(time,2) & $
endif 

args = strcompress(args)
pieces = str_sep(args,' ')

for i=0,n_elements(pieces)-1 do begin & $
  items = str_sep(pieces[i],':') & $
  file = path+'g'+string(items[0],format='(i6.6)')+'.'+string(long(items[2]),format='(i5.5)') & $
  writeg,long(items[0]),float(items[2]),runid=items[1],file=file,status=status & $
  if (status) then print,'Wrote: '+file else print,'Could not write: '+file & $
endfor


exit

