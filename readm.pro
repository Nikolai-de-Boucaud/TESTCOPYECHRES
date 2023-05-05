;+ 
; NAME: 
;	READM
;
; PURPOSE: 
;
;	Retrieves Measurements data from NetCDF "M" file or MDSplus
;
; CATEGORY: 
;
;	DIII-D development 
;
; CALLING SEQUENCE: 
;
;	a = READM(arg1 [,arg2] [,MODE=mode] [,RUNID=runid] [,INFO=info] 
;                              [,SOURCE=source] [,EXACT_TIME=exact_time]
;			       [,VERBOSE=verbose] 
;                              [,DEBUG=debug] [,STATUS=status] )
;
;
; INPUT PARAMETERS: 
;
;	arg1:  	Either a string specifying the filename to read, or a long 
;               integer specifying the shot number (if the latter, arg2 must 
;               be present) to read.
;
;	arg2:	Float or double specifying the EFIT time to read and return.  
;               If arg1 specifies the shot number, arg2 must be used.
;
; OPTIONAL INPUT PARAMETERS: 
;
;	none 
;
; KEYWORDS: 
;
;	(all are optional)
;	
;	MODE:  If "FILE", will restrict READM to retrieving EFIT data from
;	       files only, not from MDSplus.  If "MDSPLUS", will restrict 
;	       READM to retrieving EFIT data from MDSplus only, not files.  If
;	       not specified, READM will first attempt to retrieve the data 
;	       from a file, and then from MDSplus.
;	
;	RUNID:  EFIT "run ID" to use in MDSplus.  This defaults to "EFIT01" - 
;		the non-MSE automatic control room EFIT.
;	
;	INFO:  A structure with the following form:
;	
;		{mode:'', file:'', shot:0l, time:0.0d0, runid:''}
;	
;	       If specified as an input to READM, INFO will superceed the 
;	       arguments specified in arg1, arg2, and the keyword values of 
;              MODE and RUNID.
;	
;	       INFO is also returned from READM to indicate the values it used
;              to find the EFIT.
;	
;	SOURCE:  Either "FILE" or "MDSPLUS" - specifies the data source from
;	         where the EFIT data were retrieved.  Note that this information
;		 is also available in the returned structure as M.SOURCE.
;	
;	EXACT_TIME:  If set, forces READM to match the time specified, rather
;		     than using the default behavior of returning the nearest
;		     time.  
;
;	VERBOSE:  If set, READM will print out informational messages on its 
;                 progress.
;	
;	DEBUG:  If set, READM will print out additional debugging information,
;	        as well as turn off all error handling.  This will allow READM 
;	        to crash if there is an unexpected error.
;	
;	STATUS:  TRUE if READM was able to retrieve the data successfully, 
;                FALSE if not.  This information is also provided in the 
;		 output of the function (see below).
;
; OUTPUTS: 
;
;	Structure containing the Measurements data retrieved for the shot and
;	time specified.
;
;	Note that the structure contains the tag ERROR, which is 0 if the
;	data was read successfully, and 1 if not.
;
;	The returned structure also contains the tag SOURCE, a string that
;	describes the source from which the data was obtained (MDSplus or
;	File, which shot, EFIT run, and time).
;
; COMMON BLOCKS: 
;
;       COMMON EFIT_READM_CACHE,info_cache,data_cache
;
;	This common block caches Measurements data read from MDSplus.  READM
;	reads the data for the entire time history, and then subscripts it at
;	the time of interest. Subsequent references to data from the same shot
;	and EFIT run but different time will retrieve the data from the cache
;	rather than reading it again from MDSplus.
;
; SIDE EFFECTS: 
;
;	Calls function EFIT_READ to handle read logic - as this logic is the 
;	same as for READA.
;
; RESTRICTIONS:
;
;	None.
;
; PROCEDURE: 
;
;	READM retrieves Measurements data from an EFIT run for a particular shot 
;	and timeslice.  READM uses the following logic to locate the EFIT:
;	
;	- If arg1 specifies a file, READM attempts to determine the 6 digit
;	  shot number and time from the filename, assuming it has the format
;	  .../mSSSSSS.TTTTT_TTT.  _TTT is optional - used for specifying
;	  sub-millisecond timeslices.  If it cannot, it will still attempt to 
;	  read the file specified, but if the file attempt fails, the MDSplus
;	  attempt will also fail (see below).  NOTE THAT if arg1 specifies a
;	  file, READM will act as if the EXACT_TIME keyword is set - that is
;	  it will not attempt to find the nearest time if it cannot find the
;	  exact time.
;
;
;	  NOTE!  For Time-Dependent M files (that is, mSSSSSS.nc), a time
;	  *MUST* be specified as the second argument.
;	
;	- If arg1 specifies the shot and arg2 the time, the filename
;	  mSSSSSS.TTTTT_TTT is composed, where SSSSSS is the 6 digit shot 
;	  number and TTTTT_TTT is the time, optionally using the _TTT for 
;	  sub-ms timeslices.
;	
;	- If the filename contains a directory specification, READM will look 
;	  for the file specified in the place specified.  If it does not, 
;	  READM will search  the following locations (in order) for the file:
;	
;	  1) The current directory ./  (VMS: [])
;	  2) The subdirectory ./shotSSSSSS  (VMS: [.shotSSSSSS])
;	  3) The subdirectory ./shotSSSSS  (VMS: [.shotSSSSS]) 
;					(SSSSS = 5 digit shot number)
;	
;	- If the file is found in one of these places, an attempt is made to 
;	  read it.
;	
;	- If the file is not found in one of these three places, and the 
;	  keyword EXACT_TIME is *NOT* set, the same locations are searched 
;	  for a MEASUREMENTS file with a time *nearest* the time specified.  
;	
;	- If the read attempt fails, or if the file is not found, READM will 
;	  attempt to read the data from MDSplus, using the shot number and 
;	  time specified (or determined from the filename).  Data from the 
;	  time *nearest* the time specified will be returned if the MDSplus 
;	  read attempt is successful (unless the keyword EXACT_TIME is set).
;	
;	- If the value of the keyword MODE is "MDSPLUS", READM will not
;	  attempt to read the data from a file, instead proceeding directly to
;	  the MDSplus read attempt. 
;	
;	- If the value ofthe keyword MODE is "FILE", READM will not attempt to
;	  read the data from MDSplus if the file read attempt is unsuccessful.
;	
;	- Any other value of MODE will have no effect on the read logic.
;	
; EASE OF USE: Can be used with existing documentation
;
; OPERATING SYSTEMS:  HP-UX, OSF/Unix, OpenVMS, MacOS
;
; EXTERNAL CALLS:  MDSplus
;
; RESPONSIBLE PERSON: Jeff Schachter
;
; CODE TYPE: modeling, analysis, control  
;
; CODE SUBJECT:  handling, equilibrium
;
; DATE OF LAST MODIFICATION: 7/28/00
;
; MODIFICATION HISTORY:
;
;	Version 1.0: Released by Jeff Schachter 98.03.
;	Version 1.1: JMS - Check if no time specified for time-dependent file read
;	Version 2.0: Standardized behavior in finding nearest time, and added keyword
;		     EXACT_TIME (Jeff Schachter, 98.04.27)
;	Version 2.1: Jeff Schachter 98.05.16 - added M.SOURCE to indicate from where
;		     data was obtained
;       Version 2.2: Jeff Schachter 1998.10.06
;                    - modify calls to MDSplus functions so that this
;                      procedure works with both client/server and
;                      native access
;	01-19-99 Q.Peng updated vport per Rice.
;	06-23-99 Q.Peng used NEAREST_TIME keyword when calling read_nc.
;       2000.07.28 J. Schachter - handle single timeslice EFITs in readm_mdssub
;       07-30-2004 Q.P. add special case for design efit EFITDE
;-	


;-------------------------------------------------------------------------------------
; FILE SPECIFIC CODE
;-------------------------------------------------------------------------------------

function readm_file,info,verbose=verbose,debug=debug,status=status

  forward_function efit_read_error, efit_read_filesearch, read_nc

  ;====== ONE ERROR HANDLER FOR ALL I/O ERRORS
  if (not(keyword_set(debug))) then catch,err else err=0
  if (err ne 0) then begin
    catch,/cancel
    efit_read_message,1,'READM_FILE: Error reading mfile '+info.file+': '+!ERR_STRING
    if (keyword_set(lun)) then free_lun,lun
    status = 0
    return,efit_read_error()
  endif


  efit_read_message,verbose,'READM_FILE: reading file '+info.file

  ;====== check to see if file is a time-dependent NetCDF file, and if so, whether a time is specified.

  if (strupcase(strmid(info.file,strlen(info.file)-2,2)) eq 'NC' and info.time eq -1.) then begin
    msg = 'READM_FILE: For time dependent NetCDF files, a time argument must be specified'
    if (keyword_set(debug)) then begin
      print,msg
      stop
    endif else message,msg ; generate error for handler
  endif 

  ;====== open and read file 

  m = read_nc(info.file,time=info.time,/source,/nearest_time)  ;read single time slice
  status = (m.error eq 0)
  if (not(status)) then begin
    if (keyword_set(debug)) then begin
      print,m.msg
      stop
    endif else message,m.msg  ; generate error for handler
  endif

  return,m

end

;-------------------------------------------------------------------------------------
;  MDSplus SPECIFIC CODE
;-------------------------------------------------------------------------------------


function readm_mdsreadall,info,verbose=verbose,debug=debug,status=status

  forward_function efit_read_error, efit_read_mds_tags, mdsvalue

  quiet = 1 - (keyword_set(debug))

  s={shot:info.shot, time:mdsvalue('\MTIME',quiet=quiet,status=status), error:0}
  if (status) then begin
    siginfo = efit_read_mds_tags(info.type,debug=debug)
    status = siginfo.status
    if (status) then begin 
      indecies = where(siginfo.tags ne 'TIME',n)
      if (n gt 0) then begin
        for i=0,n-1 do begin
	  j = indecies[i]
          if (keyword_set(debug)) then print,siginfo.nodes[j]
          data=mdsvalue(siginfo.parents[j],quiet=quiet,status=statread)
          if (statread) then s=create_struct(temporary(s),siginfo.tags[j],data)
        endfor 
      endif else s.error=1
    endif else s.error=1
  endif else s.error=1
  return,s
  
end

function readm_mdssub,shot,itime,runid,status=status

  common efit_readm_cache,info_cache,data_cache

  source = 'MDSplus, shot = '+strtrim(shot,2)+', run = '+runid+', time = '+strtrim(data_cache.time[itime[0]],2)
  if (n_elements(data_cache.time) eq 1) then return,create_struct(data_cache,'tree',runid,'source',source)
  data={shot:shot, time:data_cache.time(itime), error:0, source:source}

  tags=tag_names(data_cache)
  ix=where(tags ne 'SHOT' and tags ne 'TIME' and tags ne 'ERROR',nx)

  if (nx gt 0) then begin
    for i=0,nx-1 do begin
      j=ix(i)
      case ((size(data_cache.(j)))(0)) of 
	0 : 
        1 : data=create_struct(data,tags(j),data_cache.(j)(itime))
        2 : data=create_struct(data,tags(j),data_cache.(j)(*,itime))
        3 : data=create_struct(data,tags(j),data_cache.(j)(*,*,itime))
      endcase
    endfor
    status  = 1
  endif else begin
    status = 0
    data.error = 1
  endelse
  return,data

end

function readm_mdsread,info,itime,verbose=verbose,debug=debug,status=status

  common efit_readm_cache,info_cache,data_cache
  forward_function efit_read_error

  ;====== initialize cache

  if (not(keyword_set(info_cache))) then begin
    efit_read_message,verbose,'READM_MDSREAD: Initializing info_cache'
    info_cache={shot:0l, runid:''}
  endif


  ;====== if data already cached

  if (keyword_set(data_cache) and (info.shot eq info_cache.shot) $
	                      and (info.runid eq info_cache.runid)) then begin

    data = readm_mdssub(info.shot,itime,info.runid,status=status)
    efit_read_message,verbose,'READM_MDSREAD: MDSplus data was cached already'

  endif else begin

    ;====== read data and cache it

    data_cache=readm_mdsreadall(info,verbose=verbose,debug=debug,status=status) 

    if (status) then begin

      info_cache=info
      data=readm_mdssub(info.shot,itime,info.runid,status=status)
      efit_read_message,verbose,'READM_MDSREAD: MDSplus read from '+info.runid+' '+strtrim(info.shot,2)+' successful.'

    endif else begin

      data_cache = '' ; unset data_cache variable
      data = efit_read_error()
      efit_read_message,verbose,'READM_MDSREAD: Error reading MDSplus Measurements data '+strtrim(info.shot,2)+' '+info.runid+' '+strtrim(info.time,2)
      if (keyword_set(debug)) then stop

    endelse

  endelse

  return,data
end

      
;-------------------------------------------------------------------------------------

;*************************************************************************
;
;  readm.pro
;
;  created: 
;    1-22-98    B. Rice - as getmse_m0
;
;  modified:
;    03-05-98	Q. Peng - modified from B.Rice''s getmse_m0 to include
;			  more info from the measurement file and 
;			  make it more general
;
;  purpose:
;    extracts mse data from measurements netcdf file
;
;  inputs: g0 filename, aeq structure from reada (for a.d.tavem)
;  keyword: _extra=e keywords of read_nc
;
;*************************************************************************


FUNCTION readm_convert,ms,mseonly=mseonly

  forward_function efit_read_error

  r_mp=2.4168              			;magnetic probe position
  z_mp=0.0

  gam_deg=atan(ms.tangam)*180/!pi
  err_gam_deg=atan(ms.siggam)*180/!pi
  nch = n_elements(ms.tangam)
  ch = indgen(nch)+1

  mse1 = {error:0, $
	 shot:ms.shot, time:ms.time, tgam:ms.tangam,$  				;dt:aeq.d.tavem,$
         err_tgam:ms.siggam, gam_deg:gam_deg, err_gam_deg:err_gam_deg,$
         r:ms.rrgam, z:ms.zzgam, a1:ms.a1gam, a2:ms.a2gam, a3:ms.a3gam, $
         a4:ms.a4gam, a5:ms.a5gam, a6:ms.a6gam, $ ;vport:vport, $
         ch:ch, bksub:ms.msebkp, ctgam:ms.cmgam, fwtgam:ms.fwtgam, $
         r_mp:r_mp, z_mp:z_mp, mp67:-ms.expmpi(14)}

  IF (Where(Tag_Names(ms) EQ 'VPORT'))[0] LT 0 THEN BEGIN
     vport=[replicate(315,11),replicate(45,15),replicate(15,10)]
     IF ms.shot LT 97400 THEN $
        vport=[replicate(315,16),replicate(45,9),replicate(15,10)]
     if ms.shot lt 91300 THEN vport=[replicate(315,8),replicate(45,8)]
     if ms.shot lt 80540 then vport=replicate(315,8)
  ENDIF ELSE vport = ms.vport
  mse1 = Create_Struct(mse1,'vport',vport)

;           i = w(0)
;           gam_deg=atan(ms.tangam(*,i))*180/!pi
;           err_gam_deg=atan(ms.siggam(*,i))*180/!pi
;           vport=[replicate(315,16),replicate(45,9),replicate(15,10)]
;           if ms.shot lt 91300 THEN vport=[replicate(315,8),replicate(45,8)]
;           if ms.shot lt 80540 then vport=replicate(315,8)
;           nch = n_elements(ms.tangam(*,i))
;           ch = indgen(nch)+1
;
;           mse1 = {shot:ms.shot, time:ms.time(i), $				;dt:aeq.d.tavem,$
;              tgam:ms.tangam(*,i), err_tgam:ms.siggam(*,i),  $
;              gam_deg:gam_deg, err_gam_deg:err_gam_deg,$
;              r:ms.rrgam(*,i), z:ms.zzgam(*,i), a1:ms.a1gam(*,i), $
;              a2:ms.a2gam(*,i), a3:ms.a3gam(*,i), a4:ms.a4gam(*,i), $
;              a5:ms.a5gam(*,i), a6:ms.a6gam(*,i), vport:vport, $
;              ch:ch, bksub:ms.msebkp(i), ctgam:ms.cmgam(*,i),  $
;              fwtgam:ms.fwtgam(*,i), r_mp:r_mp, z_mp:z_mp, $
;              mp67:-ms.expmpi(14,i), ierr:0}


; resize mse1 structure to eliminate channels that are turned off
  wg = where(mse1.r NE 0,nwg)

  if (nwg gt 0) then begin
    mse1 = {error:mse1.error,$
	  shot:mse1.shot, time:mse1.time, tgam:mse1.tgam(wg), $			;dt:mse1.dt, $
          err_tgam:mse1.err_tgam(wg), gam_deg:mse1.gam_deg(wg), $
          err_gam_deg:mse1.err_gam_deg(wg), r:mse1.r(wg), z:mse1.z(wg), $
          a1:mse1.a1(wg), a2:mse1.a2(wg), a3:mse1.a3(wg), a4:mse1.a4(wg), $
          a5:mse1.a5(wg), a6:mse1.a6(wg), vport:mse1.vport(wg), $
          ch:ch(wg), bksub:mse1.bksub, ctgam:mse1.ctgam(wg), $
          fwtgam:mse1.fwtgam(wg), r_mp:mse1.r_mp, z_mp:mse1.z_mp, $
          mp67:mse1.mp67}

  endif else begin 
    wg = mse1.r
    mse1.error = 2
  endelse

  ; add more info if not mseonly and available
  if not keyword_set(mseonly) then begin
    ; check if the additional info are available
    ; this set of info are added to weqdsk.for at the same time
    tagnames = tag_names(ms)
    index = where('CHIGAM' eq tagnames,count)
    if count ne 0 then begin
      mse1 = create_struct(mse1,'chigam',ms.chigam,$
		'chimpi',ms.saimpi,'chisil',ms.saisil,$
	   	'czmaxi',ms.czmaxi,'cchisq',ms.cchisq,'cerror',ms.cerror)
    endif
  endif

return, mse1 

END ; readm

;-------------------------------------------------------------------------------------

function readm, arg1, arg2, convert=convert, mode=mode, runid=runid, info=info,$ 
			    source=source, quiet=quiet, maxdt=maxdt, $
			    exact_time=exact_time, time_range=time_range, $
			    verbose=verbose, debug=debug, status=status 

  forward_function efit_read, efitde_read

  if keyword_set(quiet) then verbose=0

  if n_elements(runid) gt 0 then begin
    if strupcase(strtrim(strjoin(runid),2)) eq 'EFITDE' then begin
      mtemp = efitde_read(arg1,type='m',debug=debug,status=status,server=server)
      return,mtemp
    endif
  endif

  mtemp = efit_read(arg1, arg2, type='m', mode=mode, runid=runid, info=info, source=source, $
			        exact_time=exact_time, time_range=time_range, maxdt=maxdt, $
				verbose=verbose, debug=debug, status=status, quiet=quiet)
  if (keyword_set(convert) and (mtemp.error eq 0)) then begin
    return,readm_convert(mtemp)
  endif else return,mtemp

end

