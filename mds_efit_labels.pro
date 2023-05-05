function mds_efit_labels,type,shot,run
  if (not(keyword_set(type))) then type = 'a'

  mdsopen,run,shot,/quiet,status=stat

  if (stat) then begin
    headerPath = mdsvalue('GETNCI("\\EFIT_AEQDSK","FULLPATH")',/quiet,status=statheader)
    if (not(statheader)) then headerPath = '\TOP.RESULTS.AEQDSK'

    nids = mdsvalue('GETNCI($,"NID_NUMBER")',headerPath+':*:READA_NAME',/quiet,status=statx)

    if (statx) then begin

      parentnids = mdsvalue('GETNCI(GETNCI($,"PARENT"),"NID_NUMBER")',nids,/quiet,status=stat)
      if (stat) then begin
        signals = strtrim(mdsvalue('GETNCI($,"NODE_NAME")',parentnids,/quiet),2)
        reada_names = strtrim(mdsvalue('GETNCI($,"RECORD")',nids),2)
        labels = strtrim(mdsvalue('GETNCI($,"RECORD")',headerPath+':'+signals+':LABEL'),2)
        units = strtrim(mdsvalue('GETNCI($,"RECORD")',headerPath+':'+signals+':UNITS'),2)
      endif 

    endif else begin

      ; C-Mod tree
      nids = mdsvalue('GETNCI($,"NID_NUMBER")',headerPath+':*',/quiet,status=stat)
      if (stat) then begin
        signals = strtrim(mdsvalue('GETNCI($,"NODE_NAME")',nids,/quiet),2)
        labels = strtrim(mdsvalue('GETNCI($,"RECORD")',signals+':COMMENT',/quiet,status=statlabel),2)
        if not statlabel then labels = strarr(n_elements(signals))
        units = strarr(n_elements(signals))
        reada_names = signals
      endif

    endelse
  endif


  if (stat) then begin
    return,{signals:signals, labels:labels, units:units, reada:reada_names, status:1}
  endif else return,{status:0}

end
