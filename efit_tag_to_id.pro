function efit_tag_to_id, arg1, arg2, experiment=experiment, $
                         debug=debug, status=status

  ;;
  ;; STEP 0: Definition of our run data structure
  ;;

  run = { id:0L, tag:'', tree:'' }

  status=1
  catch, error
  if error ne 0 then begin
    catch, /cancel
    status=0
    message, !ERROR_STATE.MSG, /info
    return, run
  endif

  ;;
  ;; STEP 1: Process arguments
  ;;

  dbg = keyword_set(debug)

  exp = (keyword_set(experiment)) ? experiment : 'D3D'

  if n_elements(arg1) ne 1 then message,'Required: shot number (scalar long)'

  if n_elements(arg2) ne 1 then message,'Required: user tag (scalar string)'

  shot = 0
  tag = ''
  type1 = size(arg1,/type)
  if type1 eq 2 or type1 eq 3 then shot = arg1
  if type1 eq 7 then tag = arg1
  type2 = size(arg2,/type)
  if type2 eq 2 or type2 eq 3 then shot = arg2
  if type2 eq 7 then tag = arg2
  
  if shot eq 0 then message,'Usage: id = efit_tag_to_id(shot,tag)'
  if tag eq '' then message,'Usage: id = efit_tag_to_id(shot,tag)'

  
  ;;
  ;; STEP 2: Query the code run database to find any run IDs
  ;; corresponding to an EFIT code run with the specified tag for the
  ;; shot supplied by the user.
  ;;

  set_database, 'code_rundb', status=status
  if not status then message,'Could not connect to Code Run Database'

  sql = $
      'select run_id, tree ' + $
      '  from code_rundb.dbo.plasmas ' + $
      ' where experiment = ? ' + $
      '   and shot = ? ' + $
      '   and code_name = ''EFIT'' ' + $
      '   and deleted = 0 ' + $
      '   and run_type = ?'
  
  n = dsql(sql, exp, shot, tag, ids, trees)

  if n lt 0 then message,'Error querying Code Run Database'
  if n eq 0 then message,'Search returned 0 items'

  runs = replicate(run, n)

  for i=0, n-1 do begin
    runs[i].id = ids[i]
    runs[i].tag = tag
    runs[i].tree = trees[i]
  endfor

  return, runs

end
  
