;+ 
; NAME: 
;	CW_EFITPICK
;
; PURPOSE: 
;
;	Compound widget allowing selection of an EFIT run for other tools
;
; CATEGORY: 
;
;	DIII-D development 
;
; CALLING SEQUENCE: 
;	WidgetId = CW_EFITPICK(parent, [,path=path] [,mode=mode] [,types=types]
;                                      [,force_types=force_types]
;                                      [,multiple=multiple]
;				       [,help_event=help_event]  )
;
; INPUT PARAMETERS: 
;
;	parent: ID of parent widget 
;
; OPTIONAL INPUT PARAMETERS: 
;
;	none 
;
; KEYWORDS: 
;
;	(all are optional)
;
;	PATH:  Initial path for file browsing.  Defaults to current directory
;
;	MODE:  Initial mode for selecting EFIT (either "FILE" or "MDSPLUS")
;	       (user can change this mode; this just sets which is displayed
;		when widget is first created).  Defaults to "FILE".
;
;	TYPES:  The types of EFIT files required of a shot and time in the
;	        selection.  These will be any combination of "a","g", and "m" 
;               in a string array.  If no types are specified, any file type 
;               is allowed. The user may alter the required types unless the 
;               keyword FORCE_TYPES is set.  (See below for discussion of file 
;               types returned.)
;
;	FORCE_TYPES:  If set, prevents user from changing list of required 
;                      file types (see TYPES keyword).
;
;       MULTIPLE:  If set, multiple time selections are allowed. Events 
;                  returned in this case will still contain only the
;                  last selected time, but the value of the
;                  CW_EFITPICK widget will contain the full list of
;                  selected times (and files if in FILE mode)
;
;	HELP_EVENT:  CW_EFITPICK will set this to be the name of a 
;                    procedure to call for help on the EFIT picker.
;                    It is intended to be used by calling applications
;                    that have a HELP option in their menu bar.  The 
;                    calling application must set the label of the
;                    HELP item corresponding to the help provided by
;                    the procedure HELP_EVENT
; 
;	Keywords accepted by base widgets are also accepted by CW_EFITPICK
;
; OUTPUTS: 
;
;	ID of newly created CW_EFITPICK compound widget (and
;	optionally the HELP_EVENT procedure).
;
; COMMON BLOCKS: 
;
;	None.  
;
; SIDE EFFECTS: 
;
;	This widget does NOT alter the current path of the IDL process.
;	
;	Calls are made to the operating system (spawn) and to MDSplus
;	(mds$open, mds$value etc.)
;
; RESTRICTIONS:
;
;	parent MUST be specified 
;	
;
;	An IDL MDSplus shared library must be present and a connection to the
;	MDSplus data server initialized.
;
; PROCEDURE: 
;
;	This widget is for selecting an EFIT run from either the file system
;	or MDSplus.  EFITs in the file system are identified by the type of
;	EFIT output (either an a, g, or m file currently), the shot number,
;	and the time slice.  EFITs in MDSplus are identified by the shot
;	number, time, and run ID of the EFIT run.
;	
;	EFITs from the file system are identified by filenames of the 
;       following form:
;	
;		FNNNNNN.TTTTT*
;	
;	Where F is the file type (either a, g, or m), nnnnnn is a *six* digit
;	shot number (padded by 0) and TTTTT is the time in ms of the shot
;	(padded by 0's).  The * allows users to select different
;	versions of EFIT files for the same time, and also allows
;	sub-millisecond EFIT's to be selected.
;
;	Sub-millisecond EFIT's are of the form:
;	
;		FNNNNNN.TTTTT_TTT
;	
;	CW_EFITPICK will return as many of the file types for a given shot and
;	time as are available, regardless of the setting of the TYPES keyword.
;	If the calling program requires the data from file type F, and cannot
;	accept the EFIT run without it, then list this type in the TYPES
;	keyword and set the FORCE_TYPES keyword as well.  The user will then
;	be able to select only from the EFIT shots/times that have a file of
;	type F available.
;	
;	By pressing the ANY FILE option, the user may select any files in the
;	file system to be used as the a, g, and m files.  No checking is
;	performed to determine if the specified files are of the required type
;	(if there is a required type), nor are the filenames parsed to attempt
;	to determine their shot and time.  This option gives the user extra
;	flexibility in selecting EFIT data, but the calling program must be
;	careful to ensure that the files specified are valid for the
;	application.
;	
;	In MDSplus mode, after a specific EFIT run is selected (ie. shot and
;	run ID), the user may optionally display the namelist used for the run
;	and the additional contextual information stored by pressing the "More
;	Info" button.
;	
;	To update the menu of shots/directories to pick up new shots, press
;	the "Update list of shots/directories" button.  This will update both
;	the file listings and the MDSplus shot list.
;	
;	The list of recognized EFIT file types is hardcoded in CW_EFITPICK *in
;	one location only* - the array fileTypes in CW_EFITPICK_BUILD, variable fileTypes).
;	
;	FILE MODE, "Any File" selection:
;	
;	   {CW_EFITPICK_ANYFILE, id:0l, top:0l, handler:0l, shot:0l, 
;                                time:0.0d0, path:'',   
;                                types:strarr(NTYPES), files:strarr(NTYPES)}
;	
;	   In this case the shot, time, and path will be null, as they
;	   are not determined from the filenames selected.
;	
;	MDSPLUS MODE, shot selected:
;	
;	   {CW_EFITPICK_MDSSHOT, id:0l, top:0l, handler:0l, shot:0l} 
;	
;	MDSPLUS MODE, EFIT run selected:
;	
;	   {CW_EFITPICK_MDSRUN, id:0l, top:0l, handler:0l, shot:0l, run:''}
;	
;	MDSPLUS MODE, time selected:
;	
;	   {CW_EFITPICK_MDSTIME, id:0l, top:0l, handler:0l, shot:0l,
;	                         run:'', time:0.0}
;	
;	
;	Getting/Setting the value
;	-------------------------
;	
;	A call to WIDGET_CONTROL,/GET_VALUE=val returns a structure of
;	the following form (depending on the current selection)
;
;	 {mode:'MDSSHOT',  shot:0l}
;	 {mode:'MDSRUN',   shot:0l,  run:''} 
;	 {mode:'MDSTIME',  shot:0l,  run:'',  time:fltarr(NSELECTED)}
;	
;	   		- OR -
;	
;	 {mode:'NMDSSHOT', shot:0l,  path:''}
;	 {mode:'NMDSTIME', shot:0l,  time:dlbarr(NSELECTED), path:'', 
;                          types:strarr(NTYPES), 
;                          FILES:strarr(NTYPES,NSELECTED)}
;
;       where NSELECTED is the number of times selected (= 1 if keyword 
;       MULTIPLE not specified) and NTYPES is the number of file types.
;	
;	By testing val.mode it is possible to determine which
;	structure  members are present.
;	
;	Setting the value of a CW_EFITPICK_WIDGET does not set the current
;	EFIT selection.  Instead, it is used as a means of changing the
;	current mode, path, or required types.  The value passed to
;	WIDGET_CONTROL,SET_VALUE=val should be a structure containing
;	any of the following members:
;	
;		path:''			; change the current path
;		mode:'' 		; set the mode to MDSPLUS or FILE
;		types:intarr(NTYPES)	; set the required types. 
;                                       ; (types[i]=1 sets the type, 
;		                        ; types[i]=0 clears the type)
;	
;
; CODE TYPE: utility
;
; CODE SUBJECT:  utility
;
; EASE OF USE: Can be used with existing documentation, widget
;              programming knowledge required.
;
; OPERATING SYSTEMS:  HP-UX, OSF/Unix, OpenVMS
;
; EXTERNAL CALLS:  MDSplus and operating system file access commands
;
; RESPONSIBLE PERSON:  Jeff Schachter
;	
; DATE OF LAST MODIFICATION:  1/07/00
;
; MODIFICATION HISTORY:
;	Beta development began 98.02.07
;	Version 1.0: Released by Jeff Schachter 98.02.27
;	Version 2.0: Released by Jeff Schachter 98.04.25
;	             changes: - events returned for any selection
;                               (shot, time, run)
;			      - searches only current directory for
;                               EFIT files, not subdirectories (only
;                               displays shots from subdirs).
;				Information is cached in internally 
;                               maintained pointers.
;	Version 2.1: using cw_label for "Any File" widget
;		     changed width of path widget to 30 from 25 characters
;	Version 2.2: bug in handling of time-independent files in 
;                    cw_efitpick_typeFilter
;   Version 2.3: CMG removed Forward_function mds$open - mds$open is a
;                pro not a function
;
;   Version 3.0:  Jeff Schachter 1998.08.28 - added keyword MULTIPLE
;                 and allow CW_EFITPICK to hold multiple times in
;                 it value.
;   Version 3.1:  Jeff Schachter 1998.09.11 - do not get list of
;                 MDSplus EFITs on initialization unless starting in
;                 MDSplus mode.
;   Version 3.2:  Jeff Schachter 1998.09.16 - Update list of shots
;                 only updates list of files in FILE mode or MDSplus
;                 shots in MDSplus mode. 
;
;   Version 3.3:  Jeff Schachter 1998.10.06
;                 - modify calls to MDSplus functions so that this
;                   procedure works with both client/server and
;                   native access
;   Version 3.4:  Jeff Schachter 1998.10.21
;                 - cw_test was crashing when quit after bringing up 
;                   ANY FILE dialog.  Because uvalue was never checked back in.
;   Version 3.4.1: Jeff Schachter 1998.11.06
;                 - remove spurious line in MDSinitialize
;   Version 4.0:  Jeff Schachter 1999.02.10
;                 - MDSplus shot entry no longer from list unless
;                   requested (to save time).  Simply type in shot
;                   number for EFIT selection (do receive feedback if
;                   no EFITs are stored for the requested shot)
;   Version 4.0.1: Jeff Schachter 1999.02.10
;                  - sensitize MDSplus widgets to focus attention
;   Version 5.0:  Jeff Schachter 2000.01.07
;                 - allow user to select files of form
;                   FNNNNNN.TTTTT*, and convert only those of form
;                   FNNNNNN.TTTTT_TTT to sub-millisecond form.
;   2002.07.05 Q.Peng - allow user to select a server from a dropdown list. 
;                   Info is gathered and selection made by set_experiment.
;   2002.08.27 Q.Peng - if only one site is returned, no server dropdown list.
;   2003.04.30 Q.Peng - disabled 'Update list of shots...' in MDSplus mode to
;		    prevent long wait if the button is hit accidentally. The
;		    same/better function is provided by Query Database.   
;   2003.11.04 Q.Peng IDL6.0 is more strict about the difference between a
;		    scalar and an array of size one.  Fix it in routine
;		    cw_efitpick_show_selected (string->string[0]).
;   2004.07.30 Q.Peng accomodate special cases of scratch (EFIT) and design
;                   efit (EFITDE).
;   2006.01.20 Q.Peng added efit file type 't' for geqdsk files from TRANSP,
;                   updated efit_viewer.pro accordingly. Requested by Lazarus.
;   20090310 SMF - Increased EFIT run width to 11 characters
;   20090506 SMF - Enabled the Machine drop down for File Mode
;-	


;=====================================================================

pro cw_efitany_file_event,ev
  widget_control,ev.id,get_uvalue=label
  widget_control,label,set_value=ev.value
end

pro cw_efitany_event,ev
  widget_control,ev.id,get_value=button
  case strupcase(strtrim(button,2)) of 
    "OK" : begin
      widget_control,ev.top,get_uvalue=u
      ntypes=n_elements(u.types)
      files=strarr(ntypes)
      for i=0,ntypes-1 do begin
	widget_control,u.f.cwFiles(i),get_value=file
	files(i)=file
	widget_control,u.f.labels(i),set_value=file
      endfor
      event={CW_EFITPICK_ANYFILE, $
	     id:u.base, $
	     top:u.tlb, $
	     handler:0l,$
	     shot:0l, $
	     time:0.0d0, $
	     path:'', $
	     types:u.types, $
	     machine:'',  $
	     files:files}
      widget_control,u.tlb,send_event=event
    end
    "CANCEL" : 
  endcase
  widget_control,ev.top,map=0
end
  
pro cw_efitany_dl_event,ev
  widget_control,ev.top,get_uvalue=u
  id=u.f.bases(ev.value)
  widget_control,id,map=ev.select
end

function cw_efitany,tlb,types,startpath=startpath
  base=widget_base(/column, group_leader=tlb, event_pro='CW_EFITANY_EVENT', $
	title="Select EFIT files")

  ntypes=n_elements(types)
  cwFiles=lonarr(ntypes)
  bases=lonarr(ntypes)
  labels=lonarr(ntypes)
  length=40

  row1=widget_base(base, /row, event_pro="cw_efitany_dl_event")
  x=widget_label(row1, value="Select file for file type: ")
  bgp=cw_bgroup(row1, types, /row, /exclusive, uvalue=types)
  for i=0,ntypes-1 do begin
    r=widget_base(base, /row, /base_align_left)
    x=widget_label(r, value="File type "+types(i)+": ")
    labels(i)=cw_label(r, width=length,/align_left)
  endfor

;  row=widget_base(base, /row, event_pro="cw_efitany_dl_event")
;  bgp=cw_bgroup(row, types, /column, /exclusive, uvalue=types)
;  col=widget_base(row, /col)
;  for i=0,ntypes-1 do begin
;    labels(i)=widget_label(col, value=string(bytarr(length)+32b))
;  endfor

;  for i=0,ntypes-1 do begin
;    row=widget_base(base, /row)
;    x=widget_label(row, value="File type: ")
;    rx=widget_base(row, /exclusive, /row, event_pro='cw_efitany_dl_event', uvalue=i)
;    bg=widget_button(rx, value=types(i))
;    labels(i)=widget_label(row, value=string(bytarr(length)+32b))
;  endfor
    


  baseBboard=widget_base(base, event_pro='CW_EFITANY_FILE_EVENT')
  for i=0,ntypes-1 do begin
    bases(i)=widget_base(baseBboard,/column,map=0)
    cwFiles(i)=cw_file(bases(i), xsize=30, frame=0, startpath=startpath, uvalue=labels(i), filter=types(i)+"*")
  endfor

  widget_control,bases(0),/map
  widget_control,bgp,set_value=0  
 
  row2=widget_base(base, /row)
  ok=widget_button(row2, value="     OK     ", /align_left)
  x=widget_label(row2, value="            ")
  cancel=widget_button(row2, value="     Cancel     ", /align_right)

  flds={cwFiles:cwFiles, bases:bases, bboard:baseBboard, labels:labels}
  widget_control,base,set_uvalue={base:base,tlb:tlb,f:flds,types:types},/realize
  return,base
end

;=====================================================================

pro cw_efitpick_help_event,event
  title='HELP on EFIT picker'
  filename = '$IDLSOURCE/efit/cw_efitpick.help'
  xdisplayfile,filename,title=title,group=event.id
end

function cw_efitpick_state_id,id,noparent=noparent
  if (not(keyword_set(noparent))) then begin
    idParent=widget_info(id,/parent)  
  endif else begin
    idParent=id 
  endelse
  rowParent=widget_info(idParent,/parent)  ; will be row1nMDS, row2nMDS, or rowMDS
  modeParent=widget_info(rowParent,/parent) ; will be baseMDS or basenMDS
  bBoard=widget_info(modeParent,/parent) ; will be baseBboard
  widget_control,bBoard,get_uvalue=row1
  return,row1
end
  
function cw_efitpick_state_get,id,noparent=noparent
  widget_control,cw_efitpick_state_id(id,noparent=noparent),get_uvalue=u,/no_copy
  return,u
end

pro cw_efitpick_state_set,id,u,noparent=noparent
  widget_control,cw_efitpick_state_id(id,noparent=noparent),set_uvalue=u,/no_copy
end

 
function cw_efitpick_get,base
  widget_control,widget_info(base,/child),get_uvalue=u,/no_copy
  widget_control,u.f.cwMode,get_value=mode

  case (mode) of
    'FILE' : id=u.f.basenMDS
    'MDSPLUS' : id=u.f.baseMDS
  endcase
  widget_control,id,get_uvalue=value
  widget_control,widget_info(base,/child),set_uvalue=u,/no_copy
  return,value
end

pro cw_efitpick_set,base,value
  widget_control,widget_info(base,/child),get_uvalue=u,/no_copy

  sz=size(value)
  if (sz(n_elements(sz)-2) ne 8) then message,'value must be structure'

  tags=tag_names(value)

  i=where(tags eq 'MODE',n)
  if (n gt 0) then widget_control,u.f.cwMode,set_value=value.(i)

  i=where(tags eq 'PATH')
  if (n gt 0) then widget_control,u.f.nMDSpath,set_value=value.(i)

  i=where(tags eq 'TYPES') 
  if (n gt 0) then widget_control,u.f.nMDStype,set_value=value.(i)

  widget_control,widget_info(base,/child),set_uvalue=u,/no_copy

end

pro cw_efitpick_MDSinitialize,u
  forward_function mds_efit_shotlist

  msg = "Retrieving list of MDSplus EFITs"
  id = cw_process(msg, group_leader=u.master,tlb_frame_attr=31)
  widget_control,u.tlb,/hourglass
  shots=mds_efit_shotlist(count=count,/reverse)
  widget_control,u.f.MDSshots,set_value=shots,set_uvalue=[' ']
  cw_efitpick_MDSclear,u
  cw_efitpick_clear_selected,[u.f.basenMDS,u.f.baseMDS],u
  widget_control,id,set_value="DONE "+msg
end


pro cw_efitpick_show_selected,u,shot=shot,run=run,time=time,clear=clear,file=file
  if (keyword_set(clear)) then begin
    string=''
  endif else begin
    string=strtrim(shot,2)
    if (keyword_set(run)) then  string = string + "  " + run
    if (keyword_set(time)) then string = string + "  t =" + string(time,format='(f9.3)')
    if (keyword_set(file)) then string = string + " from file"
  endelse
  widget_control,u.f.labSelected,set_value=strmid(string[0]+string(bytarr(u.f.WidthSelect)+32b),0,u.f.WidthSelect)
;  widget_control,u.f.labSelected,set_value=strmid(string+'1234567890123456789012345678901234567890',0,u.f.WidthSelect)
end

pro cw_efitpick_clear_selected,uids,u
  for i=0,n_elements(uids)-1 do widget_control,uids[i],set_uvalue=0l
  cw_efitpick_show_selected,u,/clear
end


function cw_efitpick_shotinfo,path,types

  shotSubs = efit_shotsubs(path)
  shotCurrent = efit_shotfiles(path,types=types)

  ncurrent = shotCurrent.nshots
  nsubs = shotSubs.nshots

  ; remove shots in subdirectories that are also in current dir.
  if (nsubs gt 0 and ncurrent gt 0) then begin
    imatch = findin(shotCurrent.shots,shotSubs.shots)
    isubs  = where(imatch eq -1,nsubs)
  endif 

  nshots = ncurrent + nsubs


  if (nshots gt 0) then begin
    shots = lonarr(nshots)
    paths = strarr(nshots)
    ptrs  = ptrarr(nshots,/allocate_heap)
  
    if (ncurrent gt 0) then begin
      shots[0:ncurrent-1l] = temporary(shotCurrent.shots)
      ptrs[0:ncurrent-1l]  = temporary(shotCurrent.ptrs) 
      ; don't free shotCurrent.ptrs cuz still being pointed to by ptrs!!!
      ;paths remains as '' - blank to indicate current dir
    endif 
    if (nsubs gt 0) then begin
      ; if did not have any shots in current directory, then didn't try
      ; to find macthing shots in subdirs.
      if (not(keyword_set(isubs))) then isubs = lindgen(nsubs)
      shots[ncurrent:nshots-1l] = temporary(shotSubs.shots[isubs])
      paths[ncurrent:nshots-1l] = temporary(shotSubs.dirs[isubs])
      ;ptrs remains undefined
    endif
    isort = reverse(sort(shots))
    return,{nshots:temporary(nshots), $
	    isort:temporary(isort),   $
	    shots:temporary(shots),   $
	    paths:temporary(paths),   $
	    ptrs:temporary(ptrs)}
  endif else return,{nshots:0}
end

pro cw_efitpick_build,tlb,base,flds,path=path,mode=mode,multiple=multiple,limited_gui=limited_gui,shot=shot,_EXTRA=e

  WidthSelect=32
  WidthType=11
  base=widget_base(tlb, /column, frame=1, event_func="CW_EFITPICK_EVENT",$
					  pro_set_value="CW_EFITPICK_SET",$
					  func_get_value="CW_EFITPICK_GET",$
					  space=0)

  row1=widget_base(base, /row, space=0, event_pro="cw_efitpick_mode_event", $
      		                        kill_notify="CW_EFITPICK_KILL")
        ; This is first child;  will hold widget state info.  Part of state is a pointer,
        ; so have kill_notify event allowing it to be freed

  if (limited_gui) then begin 
     widget_control,row1,kill_notify=''  ;; limited gui does not play nice with the kill routine (kludge - SF)  
     temp = widget_base()
     cwMode = cw_mode(temp, modes=['File', 'MDSplus'], /row, default=mode)
  endif else begin
     x=widget_label(row1, value="Select EFIT from: ")
     cwMode = cw_mode(row1, modes=['File', 'MDSplus'], /row, default=mode) 
     widget_control,cwMode,get_value=defaultMode
  endelse

  sites = set_experiment(/names)
  if (!VERSION.OS ne 'darwin') then expt = set_experiment(sites[0],/connect)
  temp=widget_base(row1, /row, space=0)
  butMDSserver=widget_droplist(temp,value=sites,uvalue=tlb,$
	event_pro='cw_efitpick_server_event')
  if n_elements(sites) le 1 then widget_control,temp,map=0

  row2=widget_base(base,/row,space=0,/base_align_left)
  x=widget_label(row2, value="Selected: ")
;  fontbold='-adobe-helvetica-bold-r-normal--14-100-100-100-p-82-iso8859-1'
  fontbold='-adobe-courier-bold-r-normal--12-120-75-75-m-70-iso8859-1'
  labSelected=widget_label(row2, value=string(bytarr(WidthSelect)+32b), font=fontbold, /align_left);, /dynamic_resize)

  baseBboard = widget_base(base, frame=1, space=0, uvalue=row1)

  if (limited_gui) then begin
     baseMDS = widget_base(baseBboard, /column, uvalue=0, space=0)
  endif else begin
     basenMDS = widget_base(baseBboard, /column, map=(defaultMode ne 'MDSPLUS'), uvalue=0, space=0)
     baseMDS = widget_base(baseBboard, /column, map=(defaultMode eq 'MDSPLUS'), uvalue=0, space=0)
  endelse

  height=10 

  ;-----  Non MDSplus widgets

  if (not(limited_gui)) then begin
     row1nMDS=widget_base(basenMDS,/row, event_pro='cw_efitpick_nMDSpath_event', space=0)
     rownMDStype=widget_base(basenMDS, /row, space=0, event_pro='cw_efitpick_nMDStype_event')
     row2nMDS=widget_base(basenMDS, /row, space=0)
     bx=widget_base(row2nMDS, /column, space=0)
     nMDSpath=cw_path(row1nMDS, base2=bx,xsize=30, listwidth=16, listheight=height+2, startpath=path, /frame,  _EXTRA=e)


     fileTypes=['a','g','t','m']	;<<<<========== LIST OF RECOGNIZED EFIT FILE TYPES
					;		As new types are added or removed,
					; 		the program will adjust automatically
					;		to accomodate them.  The only place
					; 		to change when changing the EFIT file types
					;		is right here.

     ; VMS is case-insensitive
     if (!VERSION.OS_FAMILY eq 'vms') then fileTypes = strupcase(temporary(fileTypes))

     ; nMDStype=cw_bgroup(rownMDStype, fileTypes, uvalue=fileTypes, /nonexclusive, /row, label_left='File type: ')
     nMDStype=cw_bgroup(rownMDStype, fileTypes, uvalue=fileTypes, /nonexclusive, /row, label_left='Select times with:')
     x=widget_label(rownMDStype, value="OR ")
     bnMDSany=widget_button(rownMDStype, value="Any File", event_pro='cw_efitpick_nMDSany_event', uvalue=0l)


     bx=widget_base(row2nMDS, space=0, event_func='cw_efitpick_nMDSshot_event')
     nMDSshots=cw_listfield(bx, xsize=10, ysize=height, /long, format='(i6)', title='Shot', _EXTRA=e)
     ;nMDSshots will hold the shotinfo structure as its uvalue - this is the list of 
     ;shots,files,times, etc. in the current dir, and the list of subdirs with name "SHOTnnnnnn"
     widget_control,nMDSshots,set_uvalue={nshots:0}

     bx=widget_base(row2nMDS, space=0, event_func='cw_efitpick_nMDStime_event')
     ;@@@ change for any file extension @@@;  nMDStimes=cw_listfield(bx, xsize=9, ysize=height, /float, format='(f9.3)', title='Time (ms)', multiple=multiple, _EXTRA=e)
     nMDStimes=cw_listfield(bx, xsize=10, ysize=height, /string, title='Time (ms)', multiple=multiple, _EXTRA=e)

     ; bnMDSany=widget_button(basenMDS, value="or... select by browsing directories", event_pro='cw_efitpick_nMDSany_event', uvalue=0l)
  endif

  ;-----  MDSplus widgets

  rowMDS=widget_base(baseMDS, /row, space=0,/base_align_center) 

  MDSshots = cw_efitpick_mdsshots(rowMDS, height=height, event_func='cw_efitpick_mdsshot_event', shot=shot, _EXTRA=e)
  bx=widget_base(rowMDS, /column, event_func='cw_efitpick_MDSrun_event',space=0)
  ; SCRATCH  MDSruns  = cw_listfield(bx, xsize=11, ysize=height, /string, format='(a6)', title='Run',  _EXTRA=e)
  MDSruns  = cw_listfield(bx, xsize=10, ysize=height, /string, format='(a11)', title='Run', _EXTRA=e)
  bx=widget_base(rowMDS, /column, event_func='cw_efitpick_MDStime_event',space=0)
  MDStimes = cw_listfield(bx, xsize=10, ysize=height, /float,  format='(f9.3)',title='Time (ms)', multiple=multiple, _EXTRA=e)
  widget_control,MDSruns,sensitive=0
  widget_control,MDStimes,sensitive=0

  if (limited_gui) then rx = widget_base() else rx=widget_base(baseMDS, /row)
  x=widget_label(rx, value="Run By:")
  MDSluser=cw_label(rx, width=8)
  x=widget_label(rx, value="Run Tag:")
  MDSltag=cw_label(rx, width=8)
  x=widget_label(rx, value="Run Type:")
  MDSltype=cw_label(rx, width=9)
  if (limited_gui) then rx = widget_base() else rx=widget_base(baseMDS, /row)
  x=widget_label(rx, value="Exp:")
  MDSlexp=cw_label(rx, width=11)
  x=widget_label(rx, value="Date:")
  MDSldate=cw_label(rx, width=12)

  if (limited_gui) then begin
     MDScmt=widget_text(rx, xsize=20, ysize=4, /wrap, /scroll)
     butMDSinfo=widget_button(rx, value="Show Snapfile and More Info", event_pro='cw_efitpick_MDSinfo_event')
  endif else begin
     MDScmt=widget_text(baseMDS, xsize=20, ysize=4, /wrap, /scroll)
     butMDSinfo=widget_button(baseMDS, value="Show Snapfile and More Info", event_pro='cw_efitpick_MDSinfo_event')
  endelse
  ; widget_control,MDStype,sensitive=0
  widget_control,MDScmt,sensitive=0
  widget_control,butMDSinfo,sensitive=0


  ;-----  ETC

  if (limited_gui) then begin
     rescan=widget_button(rx, value="Update list of shots/directories", event_pro='cw_efitpick_rescan_event')
     flds={ baseMDS:baseMDS, labSelected:labSelected, MDSshots:MDSshots, MDSruns:MDSruns, MDStimes:MDStimes, $
            MDSluser:MDSluser, MDSltag:MDSltag, MDSltype:MDSltype, MDScmt:MDScmt, cwMode:cwMode, MDSinfo:butMDSinfo, $
            WidthType:WidthType, WidthSelect:WidthSelect, MDSserver:butMDSserver, Rescan:rescan, MDSlexp:MDSlexp,$
	    MDSldate:MDSldate } 
  endif else begin
     rescan=widget_button(base, value="Update list of shots/directories", event_pro='cw_efitpick_rescan_event')
     flds={basenMDS:basenMDS, baseMDS:baseMDS, cwMode:cwMode, labSelected:labSelected, $
	   nMDSpath:nMDSpath, nMDStype:nMDStype, nMDSshots:nMDSshots, nMDStimes:nMDStimes, rownMDStype:rownMDStype, $ 
	   MDSshots:MDSshots, MDSruns:MDSruns, MDStimes:MDStimes, $
           MDSluser:MDSluser, MDSltag:MDSltag, MDSltype:MDSltype, $ 
           MDScmt:MDScmt, MDSlexp:MDSlexp, MDSldate:MDSldate, $
	   MDSinfo:butMDSinfo, WidthType:WidthType, WidthSelect:WidthSelect,$
	   MDSserver:butMDSserver, Rescan:rescan}
  endelse
end

pro cw_efitpick_ptrfree,u
  ; this frees the pointers in shotinfo and erases the uvalue from u.f.nMDSshots
  widget_control,u.f.nMDSshots,get_uvalue=shotinfo,/no_copy
  if (shotinfo.nshots gt 0) then ptr_free,shotinfo.ptrs
end

pro cw_efitpick_kill,id
  widget_control,id,get_uvalue=u,/no_copy
  cw_efitpick_ptrfree,u
end

function cw_efitpick_typeFilter,types,ptr,nfilter=nfilter

  ; only show dependent times having file types of types selected
  if ((*ptr).dep.ntime gt 0) then begin
    itypes = 0 ; initialize
    ntypes = n_elements(types) ; initialize
    if ((*ptr).indep.ntime gt 0) then begin
       print,'CW_EFITPICK_INDEP_FILTER: HOW DO YOU GET TO THIS SECTION?!'
    ;  cmd = 'itypes = where((*ptr).indep.types ne types[0]'
    ;  for i=1,n_elements(types)-1 do begin
    ;    cmd = cmd + ' and (*ptr).indep.types ne types['+strtrim(i,2)+']'
    ;  endfor
    ;  cmd = cmd + ',ntypes)'
    ;  stat = execute(cmd)
    ;  if (ntypes gt 0) then types = types[itypes]
    endif
    if (ntypes gt 0) then begin
      ifilter = 0 ; initialize
      nfilter = 0 ; initialize
      temp = lonarr(n_elements((*ptr).dep.typeList))
      temp(*) = 1
      for j=0,n_elements((*ptr).dep.typeList)-1 do begin
          for i=0,ntypes-1 do begin
              if(strpos((*ptr).dep.typeList[j],types[i]) eq -1) then temp(j) = 0
          endfor
      endfor
      ifilter = where(temp eq 1,nfilter)
    endif else begin
      nfilter = n_elements((*ptr).dep.tuniq)
      ifilter = lindgen(nfilter)
    endelse
  endif else begin
    ; if no dependent times, then no times to display
    ifilter = -1
    nfilter = 0
  endelse
  return,ifilter
end

pro cw_efitpick_rescan_event,ev
  widget_control,widget_info(widget_info(ev.id,/parent),/child),get_uvalue=u,/no_copy
  widget_control,u.f.cwMode,get_value=mode
  case (mode) of
    'MDSPLUS' : begin
      cw_efitpick_MDSinitialize,u
      widget_control,widget_info(widget_info(ev.id,/parent),/child),set_uvalue=u,/no_copy
    end

    'FILE' : begin
      pathfld=u.f.nMDSpath
      widget_control,widget_info(widget_info(ev.id,/parent),/child),set_uvalue=u,/no_copy
      widget_control,pathfld,get_value=path
      widget_control,pathfld,set_value=path ; this causes a cw_path event
    end
  endcase
end
  
pro cw_efitpick_mode_event,ev
  widget_control,widget_info(ev.id,/parent),get_uvalue=u,/no_copy
  widget_control,u.f.basenMDS,map=(ev.mode ne 'MDSPLUS')
  widget_control,u.f.baseMDS,map=(ev.mode eq 'MDSPLUS')
  widget_control,u.f.rescan,sensitive=(ev.mode ne 'MDSPLUS')
  ;if widget_info(u.f.MDSserver,/valid) then $
  ;widget_control,u.f.MDSserver,sensitive=1 ;(ev.mode eq 'MDSPLUS')
  cw_efitpick_clear_selected,[u.f.basenMDS,u.f.baseMDS],u
  widget_control,widget_info(ev.id,/parent),set_uvalue=u,/no_copy
end

pro cw_efitpick_server_event,ev
  catch,error_status
  if error_status ne 0 then begin
     print,'Could not connect to SQL'
     catch,/cancel
     return
  endif
  widget_control,ev.id,get_value=v
  machine = v[ev.index]
  expt = set_experiment(machine,/connect)
  if not expt.connect then okay = dialog_message('Error connecting to '+server+'. Make sure you have been authenticated.',dialog_parent=ev.id)
  widget_control,ev.id,get_uvalue=u
  widget_control,u,get_uvalue=tlb
  event={CW_EFITPICK_SERVER,$
	id:ev.id,$
	top:ev.top,$
	handler:0L}
  widget_control,tlb,send_event=event
end

pro cw_efitpick_nMDSpath_event,ev
  forward_function efit_shotlist
  u=cw_efitpick_state_get(ev.id,/noparent)

  ; first free old pointers and clear old shotinfo uvalue
  cw_efitpick_ptrfree,u

  ; path was guaranteed to be valid when selected, can be invalidated between that time
  ; and here if directory is removed after initial selection

  widget_control,u.f.nMDStype,get_uvalue = types
  shotinfo = cw_efitpick_shotinfo(ev.value,types)

  if (shotinfo.nshots gt 0) then begin
    widget_control,u.f.nMDSshots,set_value=shotinfo.shots[shotinfo.isort],sensitive=1
    widget_control,u.f.nMDSshots,set_uvalue=shotinfo,/no_copy ; do last because erases shotinfo
  endif else begin
    widget_control,u.f.nMDSshots,sensitive=0,set_value=0l,set_uvalue={nshots:0}
  endelse
  widget_control,u.f.nMDStimes,sensitive=0,set_value=0l
  cw_efitpick_clear_selected,u.f.basenMDS,u

  cw_efitpick_state_set,ev.id,u,/noparent

end

pro cw_efitpick_nMDStype_event,ev

  forward_function cw_efitpick_nMDSshot_event

  u=cw_efitpick_state_get(ev.id,/noparent)

  widget_control,u.f.nMDSshots,get_uvalue=shotinfo,get_value=shot,/no_copy

  if (shotinfo.nshots gt 0) then begin
    ;Must pass in event structure the index of the item clicked in the 
    ;list widget.  The list widget displays the array shotinfo.shots[shotinfo.isort]
    ;so do the WHERE command on it to mimic the selection by the user.
    index=where(shotinfo.shots[shotinfo.isort] eq shot,n)
    
    base=u.base
    nMDSshots=u.f.nMDSshots
  endif else n=0 

  widget_control,u.f.nMDSshots,set_uvalue=shotinfo,/no_copy
  cw_efitpick_state_set,ev.id,u,/noparent

  if (n eq 1) then begin
    event={id:nMDSshots, top:base, handler:0l, index:index(0), value:shot}
    dummy = cw_efitpick_nMDSshot_event(event)
  endif

end

pro cw_efitpick_nMDSany_event,ev

  forward_function cw_efitany

  u=cw_efitpick_state_get(ev.id,/noparent)
;  id=widget_info(widget_info(ev.id,/parent),/parent)
;  widget_control,id,get_uvalue=uid
;  widget_control,uid,get_uvalue=u,/no_copy

  widget_control,ev.id,get_uvalue=baseAny
  if (widget_info(baseAny,/valid)) then begin
    widget_control,baseAny,/map,/show
    cw_efitpick_state_set,ev.id,u,/noparent
  endif else begin
    widget_control,u.f.nMDSpath,get_value=path
    widget_control,u.f.nMDStype,get_uvalue=types
    baseAny=cw_efitany(u.base, types, startpath=path)
    widget_control,ev.id,set_uvalue=baseAny
    cw_efitpick_state_set,ev.id,u,/noparent
    xmanager,"CW_EFITANY",baseAny
  endelse

;  widget_control,uid,set_uvalue=u,/no_copy

end

function cw_efitpick_nMDSshot_event,ev
  u=cw_efitpick_state_get(ev.id)
  widget_control,u.f.nMDStype,get_uvalue=types,get_value=TypeSel
  widget_control,u.f.nMDSpath,get_value=path

  widget_control,ev.id,get_uvalue=shotinfo,/no_copy
  index=shotinfo.isort[ev.index]  ; display shots in reverse order!

  ;=== if have not yet visited this subdirectory, get the list of files from it first
  if (not(keyword_set(*shotinfo.ptrs[index]))) then begin
    widget_control,u.f.nMDSpath,get_value=path
    s = efit_shotfiles(path,subdir=shotinfo.paths[index],shot=ev.value,types=types)
    status = (s.nshots eq 1)
    if (status) then begin
      shotinfo.ptrs[index] = temporary(s.ptrs[0])
    endif
  endif else status=1

  ;=== if have list of files, then display unique times
  ;=== only display times that satisfy currently selected type filter

  if (status) then begin
    status = ((*shotinfo.ptrs[index]).dep.ntime gt 0)
    if (status) then begin
      times = (*shotinfo.ptrs[index]).dep.tuniq
      iTypeSel = where(TypeSel,nTypeSel)
      if (nTypeSel gt 0) then begin
        ifilter = cw_efitpick_typeFilter(types[iTypeSel],shotinfo.ptrs[index],nfilter=nfilter)
        if (nfilter gt 0) then begin
          times = temporary(times[ifilter])
        endif
        status = (nfilter gt 0)
      endif else status = 1
    endif 
  endif

  ;=== if have times for this shot, then display them

  if (status) then begin
    widget_control,u.f.nMDStimes,set_value=times,sensitive=1
  endif else begin
    widget_control,u.f.nMDStimes,set_value=0l,sensitive=0
  endelse

  widget_control,ev.id,set_uvalue=shotinfo,/no_copy

  widget_control,u.f.mdsserver,get_value=machines
  machine = machines[ widget_info(u.f.mdsserver,/droplist_select) ]

  ;=== compose event structure sent back to caller notifying that shot was changed

  event={CW_EFITPICK_NMDSSHOT, 				$
	 id:u.base, 					$
	 top:u.tlb, 					$
	 handler:0l, 					$
	 shot:ev.value,				        $
         machine:machine, 				$
	 path:path(0)}

  ;=== display and record selected shot

  cw_efitpick_show_selected,u,shot=ev.value
  widget_control,u.f.basenMDS,set_uvalue={mode:'NMDSSHOT', shot:ev.value, path:path(0)}, /no_copy

  ;=== reset state before returning event

  cw_efitpick_state_set,ev.id,u

  ;=== return event to caller
  return,event


end

function cw_efitpick_nMDStime_event,ev
  u=cw_efitpick_state_get(ev.id)

  widget_control,u.f.nMDSshots,get_value=shot,get_uvalue=shotinfo,/no_copy
  widget_control,u.f.nMDStimes,get_value=time
  widget_control,u.f.nMDSpath,get_value=path

  widget_control,u.f.nMDStype,get_uvalue=types
  ntypes=n_elements(types)

  ntimes = n_elements(time)

  files=strarr(ntypes,ntimes)

  ;===============

  index = (where(shotinfo.shots eq shot,nindex))[0]
  if (nindex eq 1) then begin ; weird if it doesn't


    for t=0,ntimes-1 do begin

      ;;=== try to find a file for every type (even if it isn't selected)

      for i=0,ntypes-1 do begin

        ;;=== do time independent files first (if there are any)

        if ((*shotinfo.ptrs[index]).indep.ntime gt 0) then begin
          itype = where((*shotinfo.ptrs[index]).indep.types eq types[i], ntype)
          if (ntype gt 0) then files[i,t] = (*shotinfo.ptrs[index]).indep.files[itype]
        endif

        ;;=== now check time-dependent files (override time independent files)

        if ((*shotinfo.ptrs[index]).dep.ntime gt 0) then begin
          itimes = where((*shotinfo.ptrs[index]).dep.times eq time[t],ntimes)
          if (ntimes gt 0) then begin
            itype = where((*shotinfo.ptrs[index]).dep.types[itimes] eq types[i], ntype)
            if (ntype gt 0) then files[i,t] = (*shotinfo.ptrs[index]).dep.files[itimes[itype]]
          endif
        endif

      endfor

    endfor

  endif else message,"ERROR in cw_efitpick_nMDStime_event - unmatched shot: "+strtrim(shot,2)

  widget_control,u.f.nMDSshots,set_uvalue=shotinfo,/no_copy

  iev = where(time eq ev.value,nev)
  if (nev eq 0) then message,'Weird - time array does not match ev.value'

  widget_control,u.f.mdsserver,get_value=machines
  machine = machines[ widget_info(u.f.mdsserver,/droplist_select) ]

  event={CW_EFITPICK_NMDSTIME, 				$
	 id:u.base, 					$
	 top:u.tlb, 					$
	 handler:0l, 					$
	 shot:shot,				        $
;@@@ change for any file extension @@@;  time:ev.value,   		                $
	 time:double(ev.value),   		        $ 
	 path:path(0),			        	$
	 types:types,					$
         machine:machine, 				$
	 files:files[*,iev]}

  widget_control,u.f.basenMDS,set_uvalue={mode:'NMDSTIME', shot:shot, time:time, path:path(0), $
		types:types, files:files}, /no_copy
  cw_efitpick_show_selected,u,shot=shot,time=ev.value

  cw_efitpick_state_set,ev.id,u

  return,event

end

pro cw_efitpick_MDSinfo_event,ev

  widget_control,widget_info(widget_info(ev.id,/parent),/parent),get_uvalue=row1
  widget_control,row1,get_uvalue=u

  widget_control,u.f.MDSshots,get_value=shot
  widget_control,u.f.MDSruns,get_value=run
  ; check for scratch run
  if not(strcmp(run,'EFIT',4)) then begin
     shot=long(run)
     run='EFIT' 
  endif

  if (keyword_set(shot) and keyword_set(run)) then begin
    Forward_Function mdsvalue
    mdsopen,run,shot,/quiet,status=stat
    if (stat) then begin

      run_type='' & comments = '' & uderid = '' & run_type=''
      set_database,'code_rundb'
      sql = "select runtag,run_by,run_comment,run_type from plasmas where code_name='efit' and tree='"+run+"' and run_id="+strtrim(shot)    
      status = dsql(sql,runtag,userid,comments,run_type)  
      run_type = run_type[0] & comments = comments[0] & userid = userid[0] & runtag = runtag[0]  
      
      namelist=mdsvalue('\TOP:NAMELIST',/quiet,status=stat_nml)
      if (not(stat_nml)) then namelist='*** NO NAMELIST STORED WITH RUN ***'
      sep=string(bytarr(80)+45b)
      text=['RUN TYPE: '+run_type, 'RUNTAG: '+runtag,'RUN BY: '+userid, sep,'COMMENTS: ', comments, sep,'NAMELIST: ',namelist]
      title='NAMELIST for '+run+' '+strtrim(shot,2)
      xdisplayfile,file,text=text,group=ev.id,title=title,height=30
    endif else x=widget_message('Could not open MDSplus tree: '+run+' '+strtrim(shot,2),/error)
    mdsopen,run,shot,/quiet,status=stat
  endif
      
  widget_control,row1,set_uvalue=u,/no_copy
end

pro cw_efitpick_MDSclear,u
  widget_control,u.f.MDSruns,set_value=0l,sensitive=0
  widget_control,u.f.MDStimes,set_value=0l,sensitive=0
  widget_control,u.f.MDSlexp,set_value=''  
  widget_control,u.f.MDSldate,set_value=''
  widget_control,u.f.MDSluser,set_value=''
  widget_control,u.f.MDSltag,set_value=''
  widget_control,u.f.MDSltype,set_value=''
  widget_control,u.f.MDScmt,set_value='',sensitive=0
  widget_control,u.f.MDSinfo,sensitive=0
end

function cw_efitpick_MDSshot_event,ev
  forward_function mds_efit_info

  evtype = tag_names(ev,/str)
  if (evtype eq 'CW_EFITPICK_MDSSHOT_GETLIST') then begin
    
    
    u=cw_efitpick_state_get(ev.id,/noparent)
    cw_efitpick_MDSinitialize,u
    cw_efitpick_state_set,ev.id,u,/noparent

  endif else begin

    ; Figure out current machine
    u=cw_efitpick_state_get(ev.id,/noparent)
    widget_control,u.f.mdsserver,get_value=machines
    machine = machines[ widget_info(u.f.mdsserver,/droplist_select) ]

    mdsinfo=mds_efit_info(ev.value,machine)

    cw_efitpick_MDSclear,u
    if (mdsinfo.stat) then widget_control,u.f.MDSruns,set_value=mdsinfo.runs,sensitive=1

    widget_control,u.f.mdsserver,get_value=machines

    ;=== compose event structure to send back to caller
    event={CW_EFITPICK_MDSSHOT,  				$
           id:u.base, 					$
           top:u.tlb, 					$
           handler:0l,					$
           machine:machine,				$
           shot:ev.value}
    
    ;=== set and display selected shot
    widget_control,u.f.baseMDS,set_uvalue={mode:'MDSSHOT', shot:ev.value}, /no_copy
    if (mdsinfo.stat) then begin
      cw_efitpick_show_selected,u,shot=ev.value
    endif else begin
      if (ev.value ne 0) then begin
        print,string(07b)
        cw_efitpick_show_selected,u,shot=ev.value,run='- No MDSplus EFITs'
      endif else cw_efitpick_show_selected,u,/clear
      event = ''
    endelse

    widget_control,ev.id,set_uvalue=mdsinfo,/no_copy
    
    ;=== reset state before sending event
    cw_efitpick_state_set,ev.id,u,/noparent

    ;=== return event to caller
    return,event
  endelse
end

function cw_efitpick_MDSrun_event,ev
  forward_function mds_efit_time

  u=cw_efitpick_state_get(ev.id)
  widget_control,u.f.MDSshots,get_value=shot,get_uvalue=mdsinfo,/no_copy

  ;SCRATCH  times=mds_efit_time(shot,ev.value,count=count)
  runid   = mdsinfo.runids[ev.index] ;SCRATCH  
  tree    = mdsinfo.trees[ev.index] ;SCRATCH  
  machine = mdsinfo.experiments[ev.index]
  times=mds_efit_time(runid, tree, count=count) ;SCRATCH  
  if (count gt 0) then begin
    widget_control,u.f.MDStimes,set_value=times,sensitive=1
    if (count eq 1) then cw_listfield_set_selected,u.f.MDStimes,times[0]
  endif else begin
    widget_control,u.f.MDStimes,set_value=[''],sensitive=0
  endelse
  widget_control,u.f.MDSlexp,set_value=mdsinfo.experiments[ev.index]
  widget_control,u.f.MDSldate,set_value=mdsinfo.dates[ev.index]
  widget_control,u.f.MDSluser,set_value=mdsinfo.userids[ev.index]
  widget_control,u.f.MDSltag,set_value=mdsinfo.runtags[ev.index]
  widget_control,u.f.MDSltype,set_value=mdsinfo.runtypes[ev.index]
  widget_control,u.f.MDScmt,set_value=mdsinfo.comments(ev.index),sensitive=1
  widget_control,u.f.MDSinfo,sensitive=1
  widget_control,u.f.MDSshots,set_uvalue=mdsinfo,/no_copy

  widget_control,u.f.mdsserver,get_value=machines

  ;=== compose event structure to send back to caller
  event={CW_EFITPICK_MDSRUN,  				$
	 id:u.base, 					$
	 top:u.tlb, 					$
	 handler:0l,					$
         shot:runid,                                    $  ;SCRATCH
         machine:machine,				$
	 run:tree}                                         ;SCRATCH

  ;=== set and display selected shot
  ;SCRATCH    widget_control,u.f.baseMDS,set_uvalue={mode:'MDSRUN', shot:shot, run:ev.value}, /no_copy
  widget_control,u.f.baseMDS,set_uvalue={mode:'MDSRUN', shot:runid, run:tree}, /no_copy
  ;SCRATCH    cw_efitpick_show_selected,u,shot=shot,run=ev.value
  cw_efitpick_show_selected,u,shot=runid,run=tree

  ;=== reset state before sending event
  cw_efitpick_state_set,ev.id,u

  ;=== return event to caller
  return,event

end

function cw_efitpick_MDStime_event,ev
  u=cw_efitpick_state_get(ev.id)
  widget_control,u.f.MDSshots,get_value=shot,get_uvalue=mdsinfo,/no_copy
  widget_control,u.f.MDSruns,get_value=run
  widget_control,u.f.mdsserver,get_value=machines
  isel = where(mdsinfo.runs eq run, nsel) ;SCRATCH  
  if (nsel gt 0) then begin  ; SCRATCH
    runid = mdsinfo.runids[isel]  ;SCRATCH  
    tree = mdsinfo.trees[isel]    ;SCRATCH  
    machine = mdsinfo.experiments[isel]

    event={CW_EFITPICK_MDSTIME,  			$
	   id:u.base, 					$
	   top:u.tlb, 					$
	   handler:0l,					$
	   shot:runid, 					$  ;SCRATCH    
	   run:tree, 					$  ;SCRATCH    
           machine:machine,                             $
	   time:ev.value}
  endif else begin
    event = ''
    runid = 0l
    tree=''
  endelse
  widget_control,u.f.MDSshots,set_uvalue=mdsinfo,/no_copy
  widget_control,u.f.MDStimes,get_value=times
  ;SCRATCH      widget_control,u.f.baseMDS,set_uvalue={mode:'MDSTIME', shot:shot, run:run, time:times},/no_copy
  widget_control,u.f.baseMDS,set_uvalue={mode:'MDSTIME', shot:runid, run:tree, time:times},/no_copy ;SCRATCH    
  ;SCRATCH      cw_efitpick_show_selected,u,shot=shot,time=ev.value,run=run
  cw_efitpick_show_selected,u,shot=runid,time=ev.value,run=tree  ;SCRATCH    
  cw_efitpick_state_set,ev.id,u
  return,event
end  


function cw_efitpick_event,ev
  widget_control,widget_info(ev.handler,/child),get_uvalue=u,/no_copy
  tag=tag_names(ev,/str)
  if (tag eq 'CW_EFITPICK_ANYFILE') then begin
    widget_control,u.f.basenMDS,set_uvalue={shot:ev.shot, time:ev.time, path:ev.path, types:ev.types, files:ev.files}
    widget_control,u.f.labSelected,set_value="[press ANY FILE button to view]"
  endif
  widget_control,widget_info(ev.handler,/child),set_uvalue=u,/no_copy
  return,ev
end

pro cw_efitpick_typeinit,flds,types=types,force_types=force_types
  if (keyword_set(types)) then begin
    widget_control,flds.nMDStype,get_uvalue=fileTypes
    butSet=intarr(n_elements(fileTypes))
    for i=0,n_elements(types)-1 do begin
      idx=where(strupcase(fileTypes) eq strupcase(types(i)),n)
      if (n gt 0) then butSet(idx)=1
    endfor
    widget_control,flds.nMDStype,set_value=butSet
    widget_control,flds.rownMDStype,sensitive=(1-keyword_set(force_types))
  endif
end



function cw_efitpick_toplevel,id
  newParent = id
  while (widget_info(newParent,/valid)) do begin
    parent = newParent
    newParent = widget_info(parent,/parent)
  endwhile
  return,parent
end

function cw_efitpick, tlb, path=path, types=types, force_types=force_types, mode=mode, $
			   help_event=help_event, limited_gui = limited_gui, $
			   uvalue=uvalue, value=value, shot=shot, debug=debug, _EXTRA=e

  if (not(keyword_set(limited_gui))) then limited_gui=0
  ;===== error handler with CW_EFITPICKER
  if (not(keyword_set(debug))) then begin
    catch,err 
  endif else err=0
  if (err eq 0) then begin

    ;do not allow specification of widget value in call to cw_efitpick
    if (keyword_set(value)) then message,'cannot set widget value on creation; use individual keywords to set defaults'

    cw_efitpick_build,tlb,base,flds,path=path,mode=mode,shot=shot,_EXTRA=e,limited_gui=limited_gui   

    cw_efitpick_typeinit,flds,types=types,force_types=force_types

    u={base:base, tlb:tlb, f:flds, master:cw_efitpick_toplevel(tlb)}

    widget_control,widget_info(base,/child),set_uvalue=u,/no_copy

    if (keyword_set(uvalue)) then widget_control,base,set_uvalue=uvalue

    help_event='CW_EFITPICK_HELP_EVENT'
    return,base
  endif else begin

    x = widget_message(["ERROR WITH EFIT PICKER!",!ERR_STRING],/error)
    return,0l

  endelse


end

