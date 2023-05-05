;=== Created by Jeff Schachter 1998.10.27

;=== NOTE!  THIS PROCEDURE IS A TEMPORARY SOLUTION.
;=== IT MAY CHANGE AT ANY TIME WITHOUT WARNING.
;=== READING ARRAYS OF G STRUCTURES IS A VERY INEFFICIENT 
;=== METHOD FOR PASSING GEQDSK DATA AROUND.  WHEN A BETTER
;=== SOLUTION IS FOUND IT WILL BE USED HERE INSTEAD.

; 20111103 SMF
;   This routine is a 13 year old "temporary" routine only used 
;   by efitloader?  Cleaning it up to be efitloader specific:
;   - This routine only handles files.  Removed arg2.
;   - Renamed arg1, as it takes a string array of file names.
;   - Added atime keyword and filter gfiles based on atime
;     which will already be either filtered/unfiltered before
;     it's passed in by efitloader.
;   - Check atime keywoed and set filter off when no specified.
;

function readgarray,files,atime=atime,mode=bogusmode,_EXTRA = e

  ;; Mode to readg here should ALWAYS be file, so if user passes in
  ;; mode, it will be ignored as bogusmode
  if (n_elements(bogusmode) gt 0) then print,'IGNORING MODE='+bogusmode+'.  READING ONLY FILES!'

  ;; If atime array was give, filter out any gfiles that don't have
  ;; a matching afile.  Otherwise, read all gfiles.  
  if keyword_set(atime) then filter = 1 else filter = 0

  ; Create the g pointer array
  ntot = n_elements(files) 
  gptr = ptrarr(ntot)
  nactual = 0

  ; Read gfiles and keep track of number of slices successfuly read
  for i=0,n_elements(files)-1 do begin
    gtemp = readg(files[i],_EXTRA=e,mode='FILE')
    if ( gtemp.error eq 0 ) then begin
      if ( (filter && (where(atime eq gtemp.time) ne -1)) || (filter eq 0) ) then begin
        nactual = nactual + 1
        gptr[nactual-1] = ptr_new(gtemp,/no_copy)
      endif
    endif
  endfor

  ; Find the max size of r from all the gfiles read?
  nmax = 0
  imax = -1l
  for i=0,nactual-1 do begin
    n = n_elements((*gptr[i]).r)
    if (n gt nmax) then begin
      nmax = n
      imax = i
    endif
  endfor

  ; Create garray based on the structure size of slice w/ max r? 
  index = -1l
  if (imax ge 0l) then begin
    garray = replicate(*gptr[imax],nactual)
    ntags = n_tags(garray[0])
    for i=0,nactual-1 do begin
      for j=0,ntags-1 do garray[i].(j) = (*gptr[i]).(j)
    endfor
  endif else garray=[{ierr:1}]

  ; Return garray data
  ptr_free,gptr
  return,garray

end

    
