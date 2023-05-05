function efit_shotsubs,path

  pushd,path
    subs = subdirs()
  popd

  ishots = where( (strpos(strupcase(subs),'SHOT') eq 0) and $
	            (strmid(subs,5,1) ge '0')             and $
		    (strmid(subs,5,1) le '9') ,nshots)

  if (nshots gt 0) then begin
    subs = temporary(subs[ishots])
    shots = long(strmid(subs,4,max(strlen(subs))))
    isort = sort(shots)
    shots = temporary(shots(isort))
    subs  = temporary(subs(isort))
  endif else begin
    shots = [0l]
    subs  = ''
  endelse

  return,{nshots:nshots, shots:shots, dirs:subs}

end

    
    