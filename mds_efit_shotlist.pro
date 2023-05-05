function mds_efit_shotlist,count=count,reverse=reverse
  forward_function reverse  ; this is necessary because otherwise IDL will subscript the VARIABLE reverse!!!

  filespec = 'efit01_path:efit*_*.tree'
  files = strupcase(mdsvalue('findfile($)',filespec,/quiet,status=status))
  help,status
  if (n_elements(files) gt 0 and status) then begin
    p=strpos(files,'MODEL')
    ix=where(p eq -1,n)
    if (n gt 0) then begin
      files=files(ix)
      px=strpos(files(0),'[')
      p0=strpos(files,'_',px+1)
      p1=strpos(files,'.TREE')
      for i=0,n_elements(files)-1 do files(i)=strmid(files(i),p0(i)+1,p1(i)-p0(i)-1)
      shots=long(files)
      isort=sort(shots)  ; get unique shots now (if >1 EFIT for a shot, will return same shot multiple times otherwise)
      iuniq=uniq(shots(isort))
      shots=shots(isort(iuniq))
      count=n_elements(shots)
      if (keyword_set(reverse)) then shots=reverse(shots)
    endif else begin
      shots=[0l]
      count=0
    endelse
  endif else begin
    shots=[0l]
    count=0
  endelse
  return,shots
end
