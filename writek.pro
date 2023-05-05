;+ 
; NAME: 
;       WRITEK
; PURPOSE: 
;       Write a K file from MDSplus to a file
; CALLING SEQUENCE: 
;       WRITEK, tree, shot, time [,file=file]
;                                [,dir=dir]
;                                [,all=all]
;                                [,debug=debug]
;                                [,status=status]
; INPUT PARAMETERS: 
;       tree:  string containing EFIT tree from which to retrieve K
;              file data ("EFIT01", "EFIT02", etc.)
; 
;       shot:  shot number (long)
;
;       time:  time slice to extract (ignored if keyword ALL is set)
;
;           ====>> NOTE:  WRITEK will write out a file for the 
;                         NEAREST time to the one specified
;
; OPTIONAL INPUT PARAMETERS: 
;       none
;
; KEYWORDS: 
;       all are optional
;
;       FILE: filename to write to (overrides default, only if ALL not 
;             set)
;
;       DIR:  directory in which to write file(s)
;
;       ALL:  if set, writes a K file for each time slice stored in
;             MDSplus (and time parameter in call is ignored)
;
;       DEBUG:  set for debugging purposes
;
;       STATUS:  (returned) - odd if successfully wrote K file, even
;                             if failure
;
; OUTPUTS: 
;       no return value.  Writes a file.
;
; COMMON BLOCKS: 
;      "WRITEK_CACHE": stores last K file read (see details below)
;
; SIDE EFFECTS: 
;      Writes a file
;
; RESTRICTIONS:
;      Can only be used with MDSplus
;
; PROCEDURE: 
;
;      K file data in MDSplus is stored as time dependent signals.
;      This procedure reads the time dependent signals, and writes a K
;      file for the time requested, or for all the times stored if the 
;      keyword ALL is set.
;
;      ==>> NOTE:  WRITEK will write out a file for the NEAREST time
;                  to the one specified
;
;      Since the data is read back as a function of time, this routine 
;      caches the last read in case the user makes multiple calls for
;      the same set of data - for example if the user wants to write
;      some but not all of the time slices available.
;
;      By default, the K file is named kSSSSSS.TTTTT where
;
;               SSSSSS is the shot number (minimum 6 digits, so 097979 
;               for example)
;
;               TTTTT is the time in milliseconds (5 digits)
;
;      The file is written in the current directory by default.
;
;      If the keyword DIR is set to a string containing a directory,
;      WRITEK will write the file(s) in that directory instead.  In Unix, 
;      DIR must have a trailing /.  In VMS, DIR should be the VMS
;      directory specification DISK:[DIRECTORY].
;
;      If the keyword FILE is set to a string containing a filename,
;      and the keyword ALL is not set (ie. writing one timeslice only),
;      WRITEK will use that filename instead of its default.  DIR
;      is not used so FILE should contain the fully qualified path.
;
; CODE TYPE: modeling, analysis, control  
;
; CODE SUBJECT:  operation, handling, equilibrium
;
; EASE OF USE: can be used with existing documentation
;
; OPERATING SYSTEMS:  VMS, All flavors of Unix
;
; EXTERNAL CALLS:  MDSplus
;
; RESPONSIBLE PERSON: Jeff Schachter
;	
; DATE OF LAST MODIFICATION:  9/28/00
;
; MODIFICATION HISTORY:
;
;      1999.11.02 - Jeff Schachter: created
;      2000.09.28 - J.S.: Finally got it working on current method of
;                   storing K files - as 2D string array.
;-	

function writek_filename,shot,time
  if (shot gt 999999) then shotstr = strtrim(shot,2) else shotstr = string(shot,format='(i6.6)')
  return,'k'+shotstr+'.'+string(time,format='(i5.5)')
end

function writek_mdssubone,timeIndex,structIndex
  common writek_cache,tree_common,shot_common,pointer_common

  tags = tag_names((*pointer_common).(structIndex))

  case ((size( (*pointer_common).(structIndex).(0)))[0]) of
    0 : 
    1 : data=create_struct(tags[0],((*pointer_common).(structIndex)).(0)[timeIndex])
    2 : data=create_struct(tags[0],reform(((*pointer_common).(structIndex)).(0)[timeIndex,*]))
    else : 
  endcase

  for i=1,n_elements(tags)-1 do begin
    case ((size( (*pointer_common).(structIndex).(i)))[0]) of
      0 : 
      1 : data=create_struct(temporary(data),tags[i],((*pointer_common).(structIndex)).(i)[timeIndex])
      2 : data=create_struct(temporary(data),tags[i],reform(((*pointer_common).(structIndex)).(i)[timeIndex,*]))
      else : 
    endcase
  endfor
  return,data
end
  
function writek_mdssub,time

  common writek_cache,tree_common,shot_common,pointer_common
  
  tags=tag_names(*pointer_common)

  dummy = min(abs((*pointer_common).in1.itime - time), index)
  timeUse = (*pointer_common).in1.itime[index]


  k = create_struct(tags[0],writek_mdssubone(index,0))
  for i=1,n_elements(tags)-1 do k = create_struct(temporary(k),tags[i],writek_mdssubone(index,i))
  
  return,k
end


function writek_mdsreadall,default

  forward_function efit_read_error, efit_read_mds_tags, mdsvalue
  s = ''
  quiet = 1 - (keyword_set(debug))
  mdssetdefault,default,quiet=quiet,status=status
  if (status) then begin
    nodes = strtrim(mdsvalue('GETNCI($,"NODE_NAME")',default+':*',quiet=quiet,status=status),2)
    if (status) then begin
      i = 0
      repeat begin
        data = mdsvalue(nodes[i],/quiet,status=status)
        i=i+1
      endrep until (i eq n_elements(nodes) or status)

      if (status) then begin
        s = create_struct(nodes[i-1],data)
        for i=i,n_elements(nodes)-1 do begin
          data=mdsvalue(nodes[i],quiet=quiet,status=statread)
          if (statread) then s=create_struct(temporary(s),nodes[i],data)
        endfor
      endif
    endif
  endif 
  return,s
end


pro writek_USINGNL,tree,shot,time,file=file,dir=dir,debug=debug,status=status,all=all

  common writek_cache,tree_common,shot_common,pointer_common

  quiet = 1-keyword_set(debug)
  
  if (n_elements(tree_common) eq 0) then begin
    tree_common = ''
    shot_common = 0l
    pointer_common = ptr_new()
  endif

  if ((tree ne tree_common) or (shot ne shot_common)) then begin

    ptr_free,pointer_common
    nmls = ['IN1','INER','INS','INWANT']
    defaults = '\TOP.NAMELISTS.'+nmls
    mdsopen,tree,shot,quiet=quiet,status=status
    if (status) then begin
      ptr_free,pointer_common
      pointer_common = ptr_new(create_struct(nmls,$
                                             writek_mdsreadall(defaults[0]), $
                                             writek_mdsreadall(defaults[1]), $
                                             writek_mdsreadall(defaults[2]), $
                                             writek_mdsreadall(defaults[3])),/no_copy)
    endif
    
  endif

  if (ptr_valid(pointer_common)) then begin
    if (n_elements(dir) gt 0) then filename = dir else filename = ''
    if (keyword_set(all)) then begin
      for i=0,n_elements((*pointer_common).in1.itime)-1 do begin
        k = writek_mdssub((*pointer_common).in1.itime[i])
        fileone = filename + writekfile(k.in1.ishot,k.in1.itime)
        write_nl,k,fileone
        print,'Wrote: '+fileone
      endfor
    endif else begin
      k = writek_mdssub(time)
      if (n_elements(file) gt 0) then filename = file else filename = filename + writekfile(k.in1.ishot, k.in1.itime)
      write_nl,k,filename
      print,'Wrote: '+filename
    endelse
  endif

end

pro writek_dowrite,string,filename,debug=debug
  if (keyword_set(debug)) then error = 0 else catch,error
  if (error ne 0) then begin
    catch,/cancel
    if (n_elements(lun) eq 1) then free_lun,lun
    message,'Could not write K file: '+!ERR_STRING,/info
    return
  endif
  
  openw,lun,filename,/get_lun

  printf,lun,string
  
  free_lun,lun

end

pro writek,tree,shot,time,file=file,dir=dir,debug=debug,status=status,all=all

  common writek_cache,tree_common,shot_common,pointer_common

  quiet = 1-keyword_set(debug)
  
  if (n_elements(tree_common) eq 0) then begin
    tree_common = ''
    shot_common = 0l
    pointer_common = ptr_new()
  endif

  if ((tree ne tree_common) or (shot ne shot_common)) then begin

    ptr_free,pointer_common
    mdsopen,tree,shot,quiet=quiet,status=status
    if (status) then begin
      ptr_free,pointer_common
      kdata = mdsvalue('_k = \TOP.NAMELISTS:KEQDSKS',quiet=quiet,status=status)
      if (status) then begin
        time = mdsvalue('DIM_OF(_k,0)',quiet=quiet, status=status)
        if (status) then pointer_common = ptr_new({time:time, data:kdata},/no_copy)
      endif
    endif
    
  endif

  if (ptr_valid(pointer_common)) then begin
    if (n_elements(dir) gt 0) then filename = dir else filename = ''
    if (keyword_set(all)) then begin
      for i=0,n_elements((*pointer_common).time)-1 do begin
        fileone = filename + writek_filename(shot,(*pointer_common).time[i])
        writek_dowrite,(*pointer_common).data[i,*],fileone
        print,'Wrote: '+fileone
      endfor
    endif else begin
      dummy = min(abs((*pointer_common).time - time), index)
      timeUse = (*pointer_common).time[index]
      if (n_elements(dir) gt 0) then filename = dir else filename = ''
      if (n_elements(file) gt 0) then filename = file else filename = filename + writek_filename(shot, timeUse)
      writek_dowrite,(*pointer_common).data[index,*],filename
      print,'Wrote: '+filename
    endelse
  endif

end
