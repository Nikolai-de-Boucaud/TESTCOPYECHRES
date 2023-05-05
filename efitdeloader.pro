;; efitdeloader.pro
;; This is a quick little GUI app for loading EFITDE.  The procedure
;; is:
;;  STEP 1: check inputs
;;  STEP 2: read data from file
;;  STEP 3: check data
;;  STEP 4: reserve a run id in the code run db
;;  STEP 5: create a new EFITDE tree
;;  STEP 6: load data into EFITDE tree
;;  STEP 7: write other metadata to code run db

pro efitdeloader, filename, setup=setup, debug=debug, status=status

  if not keyword_set(debug) then begin
    catch, error
    if error ne 0 then begin
      message,'unexpected error: '+!ERROR_STATE.MSG,/info
      return
    endif
  endif
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; STEP 1: check inputs
  ;;

  status = 0
  if n_elements(filename) eq 0 then begin
    message,'filename of EFIT file required',/info
    return
  endif
  dbg = keyword_set(debug)
  setup = keyword_set(setup)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; STEP 2: read data from file
  ;;

  basename = file_basename(filename)

  filetype = 'unknown'
  if strpos(basename,'g') eq 0 then filetype='g'
  if strpos(basename,'a') eq 0 then filetype='a'
  
  if filetype eq 'unknown' then begin
    message,'unknown EFIT file type',/info
    return
  endif
  
  case filetype of
    'g' : efit = readg(filename)
    'a' : efit = reada(filename)
    else: message,'no routine for reading that file type'
  endcase


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; STEP 3: check data
  ;;

  ;; Check return value from read routine to make sure it looks like
  ;; it is supposed to look, e.g. a structure with a error tag.
  if n_elements(efit) eq 0 then message,'unknown error trying to read file'
  if n_elements(efit) gt 1 then $
      message,'read routine returned too many elements'
  if size(efit,/type) ne 8 then message,'read routine did not return a struct'
  tags = tag_names(efit)
  test = where(tags eq 'ERROR',count)
  if count eq 0 then message,'read routine returned struct without ERROR tag'
  if not is_numeric(efit.error) then $
      message,'read routine returned an ERROR tag with a non-numeric type'
  type = size(efit.error,/type)
  if not (type eq 1 or type eq 2 or type eq 3) then $
      message,'read routine returned ERROR tag not of type byte, int, or long'

  ;; Check error tag.  In this context true indicates an error.
  if efit.error then begin
    message,'read routine did not read EFIT successfully',/info
    return
  endif

  ;; Make sure we have shot and time tags
  test = where(tags eq 'SHOT',count)
  if count eq 0 then message,'read routine returned struct without SHOT tag'
  if not is_numeric(efit.shot) then $
      message,'read routine returned a SHOT tag with a non-numeric type'
  type = size(efit.shot,/type)
  if not (type eq 2 or type eq 3) then $
      message,'read routine returned SHOT tag not of type int or long'
  shot = efit.shot
  test = where(tags eq 'TIME',count)
  if count eq 0 then message,'read routine returned struct without TIME tag'
  if not is_numeric(efit.time) then $
      message,'read routine returned a non-numeric TIME tag'

  ;; We need to be connected to the MDSplus server
  MdsConnect,'atlas.gat.com',status=status
  if not status then begin
    message,'could not connect to atlas.gat.com',/info
    return
  endif
  
  ;; We need to be connected to the code run database
  set_database,'code_rundb',status=status
  if not status then begin
    message,'unable to connect to code run database',/info
    return
  endif

  ;; If we're running this in setup mode, then open the model tree for 
  ;; edit and make sure we have the right nodes present in the model
  ;; tree before proceeding.
  if setup then begin
    
    MdsTCL,'edit efitde',status=status
    if not status then begin
      message,'could not open efitde model for edit',/info
      return
    endif
    
    ;; Make sure our filetype branch exists
    expr = 'getnci(".'+filetype+'","NID_NUMBER")'
    nid  = MdsValue(expr,/quiet)
    if nid[0] eq 0 then begin
      MdsTCL,'add node .'+filetype,status=status
      if not status then begin
        message,'could not add node .'+filetype,/info
        MdsClose
        return
      endif
      MdsTCL,'write'
    endif

    ;; Add nodes (if necessary)
    MdsTCL,'set def .'+filetype
    written = efitde_write(efit,/setup,debug=debug)
    message,strtrim(written,2)+' node(s) added',/info
    if written gt 0 then MdsTCL,'write'
    MdsTCL,'close'

endif
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; STEP 4: reserve a run id
  ;;

  ;; At this point we know that we read the EFIT successfully and will 
  ;; be loading something into MDSplus.  So we add an entry to the
  ;; code run database to reserve a run id.  The first step is to find 
  ;; the last entry (the max run id) and increment by 1...
  sql = "select max(run_id),count(run_id) from plasmas " + $
      "where tree='EFITDE' " + $
      "and experiment='D3D'"
  runid=0L
  count=0L
  n = dsql(sql,runid,count)
  runid=runid[0]  ;; dsql makes these into arrays
  count=count[0]
  if n eq -1 then begin
    message,'this query returned an error:',/info
    message,sql,/info
    return
  endif
  if count eq 0 then run=1 else run=runid+1
  if runid lt 1 then runid=1

  ;; Next, we reserve our run id.
  message,'warning: using run_type DN for now...',/info
  if dbg then message,'using shot/run '+strtrim(shot,2)+'/'+strtrim(run,2),/$
      info
  sql = "exec code_rundb.dbo.spInsertEFITDE ?,?,?,?"
  run_type = 'DN' ; hard-code for now
  run_comment = 'Inserted by efitdeloader.pro' ; also hard-code for now
  id = 0L
  n = dsql(sql, run, run_type, run_comment, shot, id)
  run = run[0]
  shot = shot[0]
  id = long(id[0]) ; sometimes this comes back as a float not an int
  if n ne 1 then begin
    message,'error when trying to reserve run id',/info
    message,'this was the query:',/info
    message,sql,/info
    message,'here are the inputs:',/info
    help, run, shot
    return
  endif
  if dbg then message,'identity insert result = '+strtrim(strjoin(id),2),/info

  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; STEP 5: create new EFITDE tree

  MdsOpen,'EFITDE',-1,status=status
  if not status then begin
    message,'could not open EFITDE model tree',/info
    return
  endif
  MdsTCL,'create pulse '+strtrim(run),status=status
  if not status then begin
    message,'there was a problem executing the create pulse command',/info
    MdsClose,'EFITDE',-1
    return
  endif
  MdsClose,'EFITDE',-1
  MdsOpen, 'EFITDE', run, status=status
  if not status then begin
    message,'could not open new EFITDE tree',/info
    message,'the create pulse command may have failed',/info
    return
  endif

  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; STEP 6: Load inputs into EFITDE tree
  ;;
  MdsTCL,'set def .'+filetype
  lines = efitde_write(efit,debug=debug)
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; STEP 7: write metadata to code run database
  ;;
  sql = "insert into code_rundb.dbo.efit_details (idx) values (?)"
  n = dsql(sql, id)
  if n eq -1 then begin
    message,'error when trying to insert into efit_details',/info
    message,'query = '+sql,/info
    return
  endif

  status=1
  return
    
end
