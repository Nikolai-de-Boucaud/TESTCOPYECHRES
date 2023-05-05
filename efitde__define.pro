;; efitde__define.pro
;; This class is for working with Design EFITs, a.k.a. EFITDEs.
;;
;; USAGE EXAMPLE:
;;  o = obj_new('efitde','/my/a/file','/my/g/file')
;;  o->setComment, 'testing efitde'
;;  o->load, /setup
;;

function efitde::init, file1, file2, debug=debug
  
  self.debug = keyword_set(debug)

  if n_elements(file2) eq 1 then self->setFile,file2
  if n_elements(file1) eq 1 then self->setFile,file1

  self.tree = 'EFITDE'
  self.experiment = 'D3D'
  self.comment = 'Design EFIT'
  self.runid = 0L
  self.id = 0L
  self.run_type = 'Unknown'
  
  ;; We need to be connected to the MDSplus server
  MdsConnect,'atlas.gat.com',status=status
  if not status then begin
    message,'could not connect to atlas.gat.com',/info
    return,0
  endif
  
  ;; We need to be connected to the code run database
  set_database,'code_rundb',status=status
  if not status then begin
    message,'unable to connect to code run database',/info
    return,0
  endif

  return,1
end

pro efitde::cleanup
  if ptr_valid(self.pA) then ptr_free, self.pA
  if ptr_valid(self.pG) then ptr_free, self.pG
end

pro efitde::load, setup=setup

  setup = keyword_set(setup)

  if not self->aFileOK() and not self->gFileOK() then begin
    message,'either an A-File or G-File is required before loading',/info
    message,'use the setFile method to set either/both A and G files',/info
    return
  endif

  filetypes = ['a','g']
  if self->gFileOK() and not self->aFileOK() then begin
    message,'warning: loading without A-File',/info
    filetypes = ['g']
    shot = (*(self.pG)).shot
  endif
  if self->aFileOK() and not self->gFileOK() then begin
    message,'warning: loading without G-File',/info
    filetypes = ['a']
    shot = (*(self.pA)).shot
  endif
  if self->aFileOK() and self->gFileOK() then begin
    message,'loading A-File and G-File',/info
    filetypes = ['a','g']
    shot = (*(self.pA)).shot
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
 
    for i=0, n_elements(filetypes)-1 do begin

      filetype = filetypes[i]
   
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
      MdsTCL,'set def .-'
      message,strtrim(written,2)+' node(s) added',/info
     
    endfor

    ;; Save (if necessary)
    if written gt 0 then MdsTCL,'write'
    MdsTCL,'close'
    
  endif
    
  ;; Reserve a run ID in the code run database
  dbkey = self->_reserveRunID(shot,runid=runid)

  if dbkey eq 0 then begin
    message,'we could not reserve a run id',/info
    message,'EFIT was NOT loaded',/info
    return
  endif else message,'run id is '+strtrim(strjoin(runid),2),/info

  ;; Create new tree
  MdsOpen,'EFITDE',-1,status=status
  if not status then begin
    message,'could not open EFITDE model tree',/info
    return
  endif
  MdsTCL,'create pulse '+strtrim(runid),status=status
  if not status then begin
    message,'there was a problem executing the create pulse command',/info
    MdsClose,'EFITDE',-1
    return
  endif
  MdsClose,'EFITDE',-1
  MdsOpen, 'EFITDE', runid, status=status
  if not status then begin
    message,'could not open new EFITDE tree',/info
    message,'the create pulse command may have failed',/info
    return
  endif

  ;; Load data
  for i=0, n_elements(filetypes)-1 do self->_loadData, filetypes[i]

  ;; Load metadata
  self->_loadMetaData, dbkey
  

end

pro efitde::setFile, filename

  ;; Error handler
  catch, error
  if error ne 0 then begin
    message,!ERROR_STATE.MSG,/info
    return
  endif

  ;; Check arguments
  if n_elements(filename) ne 1 then message,'filename required'
  if size(filename,/type) ne 7 then message,'filename must be a string'

  ;; Check file
  file = file_info(filename)  
  if not file.exists then message,'file does not exist'
  if not file.read then message,'file is not readable'
  if not file.regular then message,'file is not a regular file'


  ;; Process file (needs to be A-File or G-File)
  basename = file_basename(filename)
  filetype = 'unknown'
  if strpos(basename,'g') eq 0 then filetype='g'
  if strpos(basename,'a') eq 0 then filetype='a'
  if filetype eq 'unknown' then message,'unknown EFIT file type'
  
  ;; Read data from file
  case filetype of
    'g' : efit = readg(filename)
    'a' : efit = reada(filename)
    else: message,'no routine for reading that file type'
  endcase

  ;; Load EFIT from file into memory
  self->setEFIT, efit, filetype, status=status

  ;; If EFIT was loaded, then save the filename
  if status then begin
    case filetype of
      'a' : self.afile = filename
      'g' : self.gfile = filename
    endcase
  endif
  
end

pro efitde::setEFIT, efit, filetype, status=status

  status=0

  ;; Error handler
  catch, error
  if error ne 0 then begin
    message,!ERROR_STATE.MSG,/info
    return
  endif
  
  ;; Check arguments
  if n_elements(efit) ne 1 then message,'efit structure required'
  if n_elements(filetype) ne 1 then message,'efit type required'
  if size(efit,/type) ne 8 then message,'efit must be a structure'
  if size(filetype,/type) ne 7 then message,'efit type must be a string'
  tags = tag_names(efit)
  test = where(tags eq 'ERROR',count)
  if count eq 0 then message,'efit structure needs ERROR tag'
  if not is_numeric(efit.error) then $
      message,'efit struct ERROR tag has non-numeric type'
  type = size(efit.error,/type)
  if not (type eq 1 or type eq 2 or type eq 3) then $
      message,'efit struct ERROR tag not of type byte, int, or long'
  if efit.error then message,'efit struct ERROR tag indiciates error'
  test = where(tags eq 'SHOT',count)
  if count eq 0 then message,'efit struct needs SHOT tag'
  if not is_numeric(efit.shot) then $
      message,'efit struct SHOT tag has non-numeric type'
  type = size(efit.shot,/type)
  if not (type eq 2 or type eq 3) then $
      message,'efit structure SHOT tag not of type int or long'
  shot = efit.shot
  test = where(tags eq 'TIME',count)
  if count eq 0 then message,'efit structure needs TIME tag'
  if not is_numeric(efit.time) then $
      message,'efit structure has non-numeric TIME tag'

  ;; Save EFIT (A-EFIT or G-EFIT only)
  case filetype of
    'a' : begin
      if ptr_valid(self.pA) then ptr_free, self.pA
      self.pA = ptr_new(efit)
      self.run_type = efit.limloc
      status=1
      return
    end
    'g' : begin
      if ptr_valid(self.pG) then ptr_free, self.pG
      self.pG = ptr_new(efit)
      status=1
      return
    end
    else : message,'unknown file type'
  endcase

  message,'efit file type must be "a" or "g"'

end

pro efitde::setComment, str
  
  catch, error
  if error ne 0 then begin
    message,!ERROR_STATE.MSG,/info
    return
  endif

  if n_elements(str) ne 1 then message,'comment required'
  if size(str,/type) ne 7 then message,'comment must be a string'

  self.comment = str

end

function efitde::aFileOK
  return, n_elements(self.afile) eq 1
end

function efitde::gFileOK
  return, n_elements(self.gfile) eq 1
end


;;
;; "PRIVATE" METHODS
;;

function efitde::_reserveRunID, shot, runid=runid

  ;; Find the max run id and reserve the next number after this one
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
    return,0
  endif
  if count eq 0 then run=1 else run=runid+1
  if runid lt 1 then runid=1

  ;; Next, we reserve our run id.
  sql = "exec code_rundb.dbo.spInsertEFITDE ?,?,?,?"
  run_type = self.run_type
  run_comment = self.comment
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
    return,0
  endif
 
  runid=run
  return,id

end

pro efitde::_loadData, filetype

  case filetype of
    'a' : efit = *(self.pA)
    'g' : efit = *(self.pG)
    else : return
  endcase
  
  MdsTCL,'set def .'+filetype
  lines = efitde_write(efit,debug=self.debug)
  MdsTCL,'set def .-'

end

pro efitde::_loadMetaData, dbkey

  if not ptr_valid(self.pA) then begin
    message,'A-File data not available, so skipping metadata load',/info
    return
  endif

  start_time = (*(self.pA)).time

  sql = "insert into code_rundb.dbo.efit_details (idx) values (?)"
  n = dsql(sql, dbkey)
  if n eq -1 then begin
    message,'error when trying to insert into efit_details',/info
    message,'query = '+sql,/info
    return
  endif

  sql = "update code_rundb.dbo.plasmas set start_time = ? where idx = ?"
  n = dsql(sql, dbkey, time)
  if n eq -1 then begin
    message,'error when trying to update efit metadata in plasmas table',/info
    message,'query = '+sql,/info
    return
  endif

end

pro efitde__define
  s = { EFITDE, $
        debug:0,$
        tree:'EFITDE', $
        runid:0L, $
        id:0L,$
        experiment:'D3D',$
        afile:'',$
        gfile:'',$
        pA:ptr_new(),$
        pG:ptr_new(),$
        run_type:'',$
        comment:'' $
      }
end
