;+ 
; NAME: 
;	READA
;
; PURPOSE:
;
;	Retrieves AEQDSK data from file or MDSplus
;
; CATEGORY: 
;
;	DIII-D development 
;
; CALLING SEQUENCE: 
;
;	a = READA(arg1 [,arg2] [,MODE=mode] [,RUNID=runid] [,INFO=info] 
;		               [,SOURCE=source] [,MKS=mks] 
;			       [,EXACT_TIME=exact_time]	
;			       [,VERBOSE=verbose] [,SERVER=server]
;			       [,DEBUG=debug] [,STATUS=status] )
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
;	MODE:  If "FILE", will restrict READA to retrieving EFIT data from
;	       files only, not from MDSplus.  If "MDSPLUS", will restrict 
;	       READA to retrieving EFIT data from MDSplus only, not files.  If
;	       not specified, READA will first attempt to retrieve the data 
;	       from a file, and then from MDSplus.
;	
;	RUNID:  EFIT "run ID" to use in MDSplus.  This defaults to "EFIT01" - 
;		the non-MSE automatic control room EFIT.
;	
;	INFO:  A structure with the following form:
;	
;		{mode:'', file:'', shot:0l, time:0.0d0, runid:''}
;	
;	       If specified as an input to READA, INFO will superceed the 
;	       arguments specified in arg1, arg2, and the keyword values of 
;              MODE and RUNID.
;	
;	       INFO is also returned from READA to indicate the values it used
;              to find the EFIT.
;	
;	SOURCE:  Either "FILE" or "MDSPLUS" - specifies the data source from
;	         where the EFIT data were retrieved.  Note that this information
;		 is also available in the returned structure as A.SOURCE.
;	
;	MKS:  If set, values are returned in MKS units, rather than cgs.
;
;	EXACT_TIME:  If set, forces READA to match the time specified, rather
;		     than using the default behavior of returning the nearest
;		     time.  
;
;	VERBOSE:  If set, READA will print out informational messages on its 
;                 progress.
;	
;       SERVER:  If set to a string containing a valid IP address for
;                an MDSplus data server, READA will read the EFITs
;                from the specified server instead of the default for DIII-D.
;	
;	DEBUG:  If set, READA will print out additional debugging information,
;	        as well as turn off all error handling.  This will allow READA 
;	        to crash if there is an unexpected error.
;	
;	STATUS:  TRUE if READA was able to retrieve the data successfully, 
;                FALSE if not.  This information is also provided in the 
;		 output of the function (see below).
;
; OUTPUTS: 
;
;	Structure containing the GEQDSK data retrieved for the shot and
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
;	None.  
;
; SIDE EFFECTS: 
;
;	Calls function EFIT_READ to handle read logic - as this logic is the 
;	same as for READG.
;
; RESTRICTIONS:
;
;	None.
;
; PROCEDURE: 
;
;	READA retrieves AEQDSK data from an EFIT run for a particular shot and
;	timeslice.  READA uses the following logic to locate the EFIT:
;	
;	- If arg1 specifies a file, READA attempts to determine the 6 digit
;	  shot number and time from the filename, assuming it has the format
;	  .../aSSSSSS.TTTTT_TTT.  _TTT is optional - used for specifying
;	  sub-millisecond timeslices.  If it cannot, it will still attempt to 
;	  read the file specified, but if the file attempt fails, the MDSplus
;	  attempt will also fail (see below).  NOTE THAT if arg1 specifies a
;	  file, READA will act as if the EXACT_TIME keyword is set - that is
;	  it will not attempt to find the nearest time if it cannot find the
;	  exact time.
;	
;	- If arg1 specifies the shot and arg2 the time, the filename
;	  aSSSSSS.TTTTT_TTT is composed, where SSSSSS is the 6 digit shot 
;	  number and TTTTT_TTT is the time, optionally using the _TTT for 
;	  sub-ms timeslices.
;	
;	- If the filename contains a directory specification, READA will look 
;	  for the file specified in the place specified.  If it does not, 
;	  READA will search the following locations (in order) for the file:
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
;	  for an AEQDSK file with a time *nearest* the time specified.  
;	
;	- If the read attempt fails, or if the file is not found, READA will 
;	  attempt to read the data from MDSplus, using the shot number and 
;	  time specified (or determined from the filename).  Data from the 
;	  time *nearest* the time specified will be returned if the MDSplus 
;	  read attempt is successful (unless the keyword EXACT_TIME is set).
;	
;	- If the value of the keyword MODE is "MDSPLUS", READA will not
;	  attempt to read the data from a file, instead proceeding directly to
;	  the MDSplus read attempt. 
;	
;	- If the value ofthe keyword MODE is "FILE", READA will not attempt to
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
; DATE OF LAST MODIFICATION: 6/04/01
;
; MODIFICATION HISTORY:
;
;	Version 1.0: Released by Jeff Schachter 98.03.19
;           3/25/98:  B. Davis - added a keyword (MKS) to return values in MKS units.
;          98.03.25:  J. Schachter - moved Bill''s convert_a_mks from a separate file 
;		               into reada.pro as the function reada_convert_mks
;          98.03.26:  bunch of bug fixes
;          98.04.21: debugged by Bill Davis
;	Version 2.0: Standardized behavior in finding nearest time, and added keyword
;		     EXACT_TIME (Jeff Schachter, 98.04.27)
;	Version 3.0: Read AEQDSK data directly from AEQDSK nodes rather than from
;		     "TIME_SLICE" signals. (Jeff Scachter, 98.04.30)
;	Version 4.0: Jeff Schachter 98.05.16
;		     - added "header" information to AEQDSK structure if from MDSplus
;		     - moved definition of mpi structure to mpi__define so can call
;		       for MDSplus read
;		     - added A.SOURCE - indicates from where data was retrieved
;	Version 4.1: Jeff Schachter 98.05.18
;		     - added FCOIL and PSF to structure returned by MDSplus
;		     - moved definition of FCOIL and PSF structures to __define 
;	               procedures
;		     - moved normalization of PSF and conversion of FCOIL to separate
;		       functions so can be called by both FILE and
;		       MDSPLUS reads.
;       Version 4.2: Jeff Schachter 98.10.05
;                    - removed log of user path if /link/idl not present
;       Version 4.3: Jeff Schachter 1998.10.06
;                    - modify calls to MDSplus functions so that this
;                      procedure works with both client/server and
;                      native access
;	12-17-98 Q.Peng - bypass errors when setting mag,coil,etc that are
;		 Tokamak specific so as a temporary solution, it can read a 
;		 file from JET; read the time from the time-only line instead
;		 of time+others line to avoid format problem.
;	01-28-99 Q.Peng - read the 4th line of A file in a string and separate
;		 and convert later to avoid format problem with 5-digit time.
;	02-05-99 Q.Peng - changed how the 4th line being converted to make it
;		 less format-dependent. It works for both DIIID and
;		 JET efits.
;       04-08-99 J.Schachter - take ABS() of plasma current in
;                              calculation of BETAN when reading from
;                              file (running in countercurrent mode
;                              for 1st time since 1994).
;       10-27-99 J.Schachter - switch time from LONG to FLOAT (for
;                              sub-millisecond EFITs)
; 	12-07-99 Q.P. Y2K compliant - Accommodate 4-digit year without 
;		breaking 2-digit year for version number
;       2000.01.13  J. Schachter - connect to server if necessary in reada_units
;	01-20-2000 Q.P. time is changed from float to double for full 7-digit
;		        sub-millisecond. This is also consistent with readg.
;	03-10-2000 Q.P. added 5 magnetic probles (mpi) and 16 more eqdata for
;			efit version 20000309. removed checking for agreement
;			between the number of elements in cmpr2 and mpi.
;	05-09-2000 Q.P. added condno to eqdata. It used to be a dummy var. 
;       2000.07.26: J. Schachter  handle single timeslice EFITs in MDSplus.
;       2001.06.04: J. Schachter  add SERVER keyword.
;	08-01-2002 Q.P. fixed a bug in reada_mdsmakestr that caused crash when
;			tree has 20*n+1 nodes. iextra=nmax*increment+1 lt ntags
;			instead of iextra=nmax*increment lt ntags
;	08-15-2002 Q.P. fixed a bug in reada_mdata that may cause crash or
;			wrong value for eccurt,mpi,psf,fcoil when there is
;			only one element in the array or only one time stored.
;	02-18-2003 Q.P. allow mco2r be either 1 or 2 (for new efits)
;	09-09-2003 Q.P. reads last 4 new vars psin32,psin21,rq32in,rq21top
;	02-13-2004 Q.P. added FILTER keyword to allow skipping bad efit based
;			on chisq and error. filter=[chisq_cutoff,error_cutoff].
;			If filter is a scalar, will use defaults.
;	01-07-2005 Q.P. bug fixed - typo cmprs -> cmpr2; 
;			check dim of psf in reada_psf_convert
;	05-10-2005 Q.P. check dimension of QMFLAG, stored as a scalar at CMOD
;			
;	05-08-2007 AWH  add 3 mag probes: MPI1L180:0.0,MPI2L180:0.0,MPI3L180:0.0
;			and three flux loops: PSI1L:0.0,PSI2L:0.0,PSI3L:0.0
; 	20120511 SMF Ignore check on mco2r/mco2v, this data tells us how to read
;                    lines 11-14i corretly.  Skip magprobe, fcoil, psi sections 
;                    if there are more than the max size of the struct.  
;       20150605 SMF Changed mpi__define to take a shot number.  Probe 1-29 have
;                    different names as of shot 162000.
;	20150610 SMF Changed mpi to be an anonymous struct to avoid tag conflicts
;		     because there are now two defintions. 
;                    Change AEQDSK to be an anonymous struct so that it can include
;                    include the mpi anonymous struct.
;
;-	


;-------------------------------------------------------------------------------------
; structure definitions
;-------------------------------------------------------------------------------------

function mpi__define,shot

  ; DIII-D MPI Probes #1-29 were renamed as of shot 162000  
  if shot ge 162000 then begin
     b = {	MPI11M067:0.0,	MPI1A139:0.0,	MPI2A067:0.0,	MPI2A139:0.0,	MPI3A139:0.0,	$
 		MPI4A139:0.0,	MPI5A139:0.0,	MPI66M247:0.0,	MPI79A147:0.0,	MPI7NA142:0.0,	$
 		MPI67A3:0.0,	MPI67A067:0.0,	MPI6FA142:0.0,	MPI6NA157:0.0,	MPI66M067:0.0,	$
 		MPI1B139:0.0,	MPI2B067:0.0,	MPI2B139:0.0,	MPI3B139:0.0,	MPI4B139:0.0,	$
 		MPI5B139:0.0,	MPI66M157:0.0,	MPI1B157:0.0,	MPI79B142:0.0,	MPI7NB142:0.0,	$
 		MPI67B3:0.0,	MPI67B067:0.0,	MPI6FB142:0.0,	MPI6NB157:0.0,  MPI8A322:0.0,   $
                MPI89A322:0.0,  MPI9A322:0.0,   MPI79FA322:0.0, MPI79NA322:0.0, MPI7FA322:0.0,  $
                MPI7NA322:0.0,  MPI67A322:0.0,  MPI6FA322:0.0,  MPI6NA322:0.0,  MPI66M322:0.0,  $
                MPI6NB322:0.0,  MPI6FB322:0.0,  MPI67B322:0.0,  MPI7NB322:0.0,  MPI7FB322:0.0,  $
                MPI79B322:0.0,  MPI9B322:0.0,   MPI89B322:0.0,  MPI8B322:0.0,   MPI5B322:0.0,   $
                MPI4B322:0.0,   MPI3B322:0.0,   MPI2B322:0.0,   MPI1B322:0.0,   MPI11M322:0.0,  $
                MPI1A322:0.0,   MPI2A322:0.0,   MPI3A322:0.0,   MPI4A322:0.0,   MPI5A322:0.0,   $
                MPI1U157:0.0,   MPI2U157:0.0,   MPI3U157:0.0,   MPI4U157:0.0,   DSL1U180:0.0,   $
                DSL2U180:0.0,   DSL3U180:0.0,   DSL4U157:0.0,   MPI5U157:0.0,   MPI6U157:0.0,   $
                MPI7U157:0.0,   DSL5U157:0.0,   DSL6U157:0.0,   MPI1L180:0.0,   MPI2L180:0.0,   $
                MPI3L180:0.0}
  endif else begin
     b = {	MPI11M067:0.0,	MPI1A067:0.0,	MPI2A067:0.0, 	MPI3A067:0.0,	MPI4A067:0.0,	$
           	MPI5A067:0.0,  	MPI8A067:0.0, 	MPI9A067:0.0, 	MPI79A067:0.0, 	MPI7FA067:0.0,	$
              	MPI7NA067:0.0, 	MPI67A067:0.0, 	MPI6FA067:0.0,	MPI6NA067:0.0,	MPI66M067:0.0,	$
              	MPI1B067:0.0,	MPI2B067:0.0,	MPI3B067:0.0,	MPI4B067:0.0,	MPI5B067:0.0,	$
              	MPI8B067:0.0,	MPI89B067:0.0,	MPI9B067:0.0,	MPI79B067:0.0,	MPI7FB067:0.0,	$
              	MPI7NB067:0.0,	MPI67B067:0.0,	MPI6FB067:0.0,	MPI6NB067:0.0,	MPI8A322:0.0,	$
		MPI89A322:0.0,	MPI9A322:0.0,	MPI79FA322:0.0,	MPI79NA322:0.0,	MPI7FA322:0.0,	$
		MPI7NA322:0.0,	MPI67A322:0.0,	MPI6FA322:0.0,	MPI6NA322:0.0,	MPI66M322:0.0,	$
		MPI6NB322:0.0,	MPI6FB322:0.0,	MPI67B322:0.0,	MPI7NB322:0.0,	MPI7FB322:0.0,	$
		MPI79B322:0.0,	MPI9B322:0.0,	MPI89B322:0.0,	MPI8B322:0.0,	MPI5B322:0.0,	$
		MPI4B322:0.0,	MPI3B322:0.0,	MPI2B322:0.0,	MPI1B322:0.0,	MPI11M322:0.0,	$
		MPI1A322:0.0,	MPI2A322:0.0,	MPI3A322:0.0,	MPI4A322:0.0,	MPI5A322:0.0,	$
              	MPI1U157:0.0,	MPI2U157:0.0,	MPI3U157:0.0,	MPI4U157:0.0,	DSL1U180:0.0,	$
		DSL2U180:0.0,	DSL3U180:0.0,	DSL4U157:0.0,	MPI5U157:0.0,	MPI6U157:0.0,	$
		MPI7U157:0.0,	DSL5U157:0.0,	DSL6U157:0.0,	MPI1L180:0.0,	MPI2L180:0.0,	$
		MPI3L180:0.0}
  endelse
  return,b

end


pro psf__define

  ps = {psf,PSF1A:0.0,PSF2A:0.0,PSF3A:0.0,PSF4A:0.0,PSF5A:0.0,PSF6NA:0.0,$
              PSF7NA:0.0,PSF8A:0.0,PSF9A:0.0,PSF1B:0.0,PSF2B:0.0,$
              PSF3B:0.0,PSF4B:0.0,PSF5B:0.0,PSF6NB:0.0,PSF7NB:0.0,$
              PSF8B:0.0,PSF9B:0.0,PSI11M:0.0,PSI12A:0.0,PSI23A:0.0,$
              PSI34A:0.0,PSI45A:0.0,PSI58A:0.0,PSI9A:0.0,PSF7FA:0.0,$
              PSI7A:0.0,PSF6FA:0.0,PSI6A:0.0,PSI12B:0.0,PSI23B:0.0,$
              PSI34B:0.0,PSI45B:0.0,PSI58B:0.0,PSI9B:0.0,PSF7FB:0.0,$
              PSI7B:0.0,PSF6FB:0.0,PSI6B:0.0,PSI89FB:0.0,PSI89NB:0.0,$
	      PSI1L:0.0,PSI2L:0.0,PSI3L:0.0}

end

pro fcoil__define
  fc = {fcoil,F1A:0.0,F2A:0.0,F3A:0.0,F4A:0.0,F5A:0.0,$
                F6A:0.0,F7A:0.0,F8A:0.0,F9A:0.0,$
                F1B:0.0,F2B:0.0,F3B:0.0,F4B:0.0,F5B:0.0,$
                F6B:0.0,F7B:0.0,F8B:0.0,F9B:0.0}
    
end

pro reada_fcoil_convert,fcoil

 ; this function converts calculated fcoil currents from amp-turns to amps/turn

;; old code - not necessary to do a loop
;  for n=1,numfcoil do begin
;    fturn = 58.0
;    if ( (n eq 6) or (n eq 7) or (n eq 9)) then fturn=55.0
;    if ( (n eq 15) or (n eq 16) or (n eq 18) ) then fturn = 55.0
;    fcoil(n-1)=fcoil(n-1)/fturn
;  endfor

  fturn = replicate(58.,n_elements(fcoil))
  icorrect = [6, 7, 9, 15, 16, 18] ; these indecies are "1-based", not "0-based"
  fturn[icorrect-1] = 55.0  ; so subtract one from them to properly subscript
  fcoil = fcoil / fturn
end


pro reada_psf_convert,psf

  ; this function references calculated psf fluxes to flux loop 1a

;;;old code - not necessary to do a loop
;  psiref = psf(0)
;  for n=2,numpsi do begin
;    psf(n-1) = psf(n-1) - psiref
;  endfor

  if n_elements(psf) eq 1 then return
  irange=indgen(n_elements(psf)-1)+1 ; from 1->n-1, don't normalize 0th element
  psf[irange] = psf[irange] - psf[0]

end


;-------------------------------------------------------------------------------------
; UNITS CONVERSION
;-------------------------------------------------------------------------------------

function reada_units,a,source,mks=mks,verbose=verbose,debug=debug,status=status

  forward_function mdsvalue

  if ( (strupcase(source) eq 'MDSPLUS' and keyword_set(MKS)) or $
       (strupcase(source) eq 'FILE' and not(keyword_set(MKS))) ) then begin

    if (keyword_set(verbose)) then print,'Data already in requested units'
    status = 1

  endif else begin

    if (strupcase(source) eq 'FILE') then status = mdsplus_setup()

    ; save current tree and shot
    shot_current = mdsvalue('$SHOT',/quiet,status=stat_current)
    if (stat_current) then tree_current = mdsvalue('$EXPT',/quiet)


    ; Get Units and Multipliers from EFIT01 model.
    ; Do not close tree afterwards to help efficiency.
    ; (Instead, reopen current tree if there was one coming in.)

    mdsopen,'EFIT01',-1,/quiet,status = status
    if (status) then begin
      tagsMDS  = strupcase(strtrim(mdsvalue('GETNCI("\\TOP.RESULTS.AEQDSK:*:READA_NAME","RECORD")',/quiet,status=status),2))
      if (status) then begin
        multsMDS = mdsvalue('GETNCI("\\TOP.RESULTS.AEQDSK:*:MULTIPLIER","RECORD")',/quiet,status=status)
	if (status) then begin

          atags = tag_names(a.d)
          ntags = n_elements(atags)
          mults = fltarr(ntags)+1.  ; defaults to 1.

          ; Rather than doing everything within one loop (which puts IF's and CASE's in loop), 
          ; use two separate loops.

          ; This loop matches the tag names of the a.d structure to the READA_NAME values in EFIT01,
          ; and gets the corresponding multiplier

          for i=0,ntags-1 do begin
            j = where(atags[i] eq tagsMDS,n)
	    if (n eq 1) then mults[i] = multsMDS[j[0]] else if (keyword_set(debug)) then print,'READ_CONVER: ',atags[i],' not found'
          endfor

          ; These loops multiply or divide by the appropriate factor:

          if (keyword_set(MKS)) then begin	

            ; only get here if source=FILE	
  	    if (keyword_set(verbose)) then print,'READA_UNITS: converting to MKS units'
            for i = 0,ntags-1 do a.d.(i) = a.d.(i) * mults[i] ; multiply by MULTS to get MKS

          endif else begin
	    ; only get here if source = MDSplus
  	    if (keyword_set(verbose)) then print,'READA_UNITS: converting to mixed units'
    	    for i = 0,ntags-1 do begin
	      if (keyword_set(debug)) then print,atags[i],' == ',mults[i]
	      a.d.(i) = a.d.(i) / mults[i] ; divide by MULTS to get Mixed units
	    endfor
          endelse
	endif else begin
          print,'READA_UNITS: Error getting Multipliers from MDSplus.'
          if (keyword_set(debug)) then stop
	endelse
      endif else begin
        print,'READA_UNITS: Error getting Node names from MDSplus'
        if (keyword_set(debug)) then stop
      endelse
    endif else begin
      print,'READA_UNITS: Error opening MDSplus model tree.'
      if (keyword_set(debug)) then stop
    endelse

    if (stat_current) then mdsopen,tree_current,shot_current,/quiet,status=s

    if (not(status)) then print,'READA_UNITS: *** CAUTION *** No units conversion performed.'


  endelse

  return,a

end
	
            

;-------------------------------------------------------------------------------------
; FILE SPECIFIC CODE
;-------------------------------------------------------------------------------------

function reada_file,info,verbose=verbose,debug=debug,status=status

  forward_function efit_read_error, efit_read_filesearch

  ;====== ONE ERROR HANDLER FOR ALL I/O ERRORS
  if (not(keyword_set(debug))) then catch,err else err=0
  if (err ne 0) then begin
    catch,/cancel
    efit_read_message,1,'READA_FILE: Error reading afile '+info.file+': '+!ERR_STRING
    if (keyword_set(lun)) then free_lun,lun
    status = 0
    return,efit_read_error()
  endif

  ;====== open and read file

  numpsi = 41
  numfcoil = 18
  numecoil = 6 		; the ecoil count for dimensioning arrays.

  ; The array of coil currents is always returned with 6 elements.
  ; The actual number of ecoil values in the file (numecoil_rd) is 
  ; determined later in the procedure from the efit version date. 

  eqd={eqdata,						          $
	CHISQ:float(0),		RCENRM:float(0), 	BCENTR:float(0),  $
	IPMEAS:float(0), 	IPMHD:float(0), 	RSURF:float(0),   $
	ZSURF:float(0), 	AMINOR:float(0), 	KAPPA:float(0),   $
	TRITOP:float(0),	TRIBOT:float(0), 	VOLUME:float(0),  $
	RCUR:float(0), 		ZCUR:float(0), 		QSTAR:float(0),   $
	BETAT:float(0), 	BETAP:float(0), 	LI:float(0),      $
	GAPIN :float(0), 	GAPOUT:float(0), 	GAPTOP:float(0),  $
	GAPBOT:float(0), 	Q95:float(0), 		NINDX:float(0),   $
	PATH1V:float(0), 	PATHV2:float(0), 	PATH3V:float(0),  $
	CO2D1:float(0), 	DENSV2:float(0), 	CO2D3:float(0),   $
	PATH1R:float(0), 	CO2DR:float(0), 	SHEAR:float(0),   $
	BPLOAV:float(0), 	S1:float(0), 		S2:float(0),      $
	S3:float(0), 		QL:float(0), 		SEPIN:float(0),   $
	SEPOUT:float(0), 	SEPTOP:float(0), 	PSIBDY:float(0),  $
	AREA:float(0), 		WMHD:float(0), 		EPS:float(0),     $
	ELONGM:float(0), 	QM:float(0), 		DIAMGC:float(0),  $
	ALPHA:float(0), 	RTTT:float(0), 		PSIREF:float(0),  $
	INDENT:float(0), 	RXPT1:float(0), 	ZXPT1:float(0),   $
	RXPT2:float(0), 	ZXPT2:float(0), 	SEPEXP:float(0),  $
	SEPBOT:float(0), 	BTM:float(0), 		BTVAC:float(0),   $
	RQ1:float(0), 		RQ2:float(0), 		RQ3:float(0),     $
	SEPLIM:float(0), 	RM:float(0), 		ZM:float(0),      $
	PSIM:float(0), 		TAUMHD:float(0), 	BETAPD:float(0),  $
	BETATD:float(0), 	WDIA:float(0), 		DIAMAG:float(0),  $
	VLOOPT:float(0), 	TAUDIA:float(0), 	QMERCI:float(0),  $
	TAVEM:float(0), 	PBINJ:float(0), 	RVSIN:float(0),   $
	ZVSIN:float(0), 	RVSOUT:float(0), 	ZVSOUT:float(0),  $
	VSURF:float(0), 	WPDOT:float(0), 	WBDOT:float(0),   $
	SLANTU:float(0), 	SLANTL:float(0), 	ZUPERTS:float(0), $
	CHIPRE:float(0), 	CJOR95:float(0), 	PP95:float(0),    $
	SSEP:float(0),		YYY2:float(0), 		XNNC:float(0),    $
	CPROF:float(0),		ORING:float(0),		J0N:float(0),     $
	FEXPAN:float(0),	QMIN:float(0),		CHIGAM:float(0),  $
	SSI01:float(0),		FEXPVS:float(0),	SEPNOSE:float(0), $
	SSI95:float(0),		RHOQMIN:float(0),	CJOR99:float(0),  $
	CJ1AVE:float(0),	RMIDIN:float(0),	RMIDOUT:float(0), $
	PSURFA:float(0),        PEAK:float(0),          DMINUX:float(0),  $
	DMINLX:float(0),	DOLUBAF:float(0),	DOLUBAFM:float(0),$
	DILUDOM:float(0),	DILUDOMM:float(0),	RATSOL:float(0),  $
	RVSIU:float(0),		ZVSIU:float(0),		RVSID:float(0),	  $
	ZVSID:float(0),		RVSOU:float(0),		ZVSOU:float(0),	  $
	RVSOD:float(0),		ZVSOD:float(0),		CONDNO:float(0),  $
	PSIN32:float(0),	PSIN21:float(0),	RQ32IN:float(0),  $
	RQ21TOP:float(0),       CHILIBT:float(0),	LI3:float(0),     $
	XBETAPR:float(0),       TFLUX:float(0),		TCHIM1S:float(0), $
        TWAGAP:float(0),	XDUM:float(0),    			  $
	IN:float(0),  		BETAN:float(0) }

  fcoil = {fcoil}
  psf = {psf}
  mpi = mpi__define(info.shot)

  a={ shot:long(0), time:0.d, error:long(0), source:info.file,  $
      uday:' ', mf1:' ', mf2:' ', ishot:long(0), ktime:long(0), $
      limloc:' ', mco2v:long(0), mco2r:long(0), qmflag:' ',     $
      d:{eqdata}, psf:{psf}, mpi:mpi, fcoil:{fcoil}, 		$
      eccurt:fltarr(numecoil)}

  ; Initialize the error flag to say that there was an error.  This will
  ; be changed later if this routine executes successfully.

  a.error = 1

  ;====== Create the first group of variables.

  uday='aaaaaaaaaa'
  mf1='aaaaa'
  mf2='aaaaa'
  ishot=long(0)
  ktime=long(0)

  ;==== Open the file for formatted reads.

  openr, lun, info.file, /get_lun

  ; Read the first group of variables assuming that the file is formatted.
  ; If there is an error jump ahead to try to read the file assuming that 
  ; it is unformatted.

  fileformatted = 0 ; initialize to "unformatted"

  on_ioerror,READA_UNFORMATTED

  readf,lun,uday,mf1,mf2,format='(1x,a10,2a5)'
  readf,lun,ishot,ktime, format='(1x,i6,11x,i5)'

  ;==== if formatted read attempt successful, will reach here

  fileformatted = 1 

READA_UNFORMATTED:

  on_ioerror,NULL  ;==== turn off special error handling.  Now will be handled by error handler at top of procedure.

  if (not(fileformatted)) then begin    

    efit_read_message,verbose,'READA_FILE: File is not formatted.  Attempting unformatted read.'

    ; Close the file then reopen it for unformatted reads.

    close,lun
    openr,lun,info.file,/f77_unformatted,/segmented

    ;reinitialize variables - to make sure they are ok before unformatted read
    
    uday='aaaaaaaaaa'
    mf1='aaaaa'
    mf2='aaaaa'
    ishot=long(0)
    ktime=long(0)

    ; Read the data.

    readu,lun,uday,mf1,mf2
    readu,lun,ishot,ktime

  endif else if (keyword_set(debug)) then print,'READA_FILE: File is formatted.'

  ;====  Now we have determined whether the file is formatted or unformatted.
  ;====  Procede to get the remainder of the data.


  ;====  Error check on data read so far

	
  ; ktime must be 1.

  if (ktime ne 1) then begin
    msg="KTIME ERROR (not 1) ishot,ktime="+strtrim(ishot,2)+' '+strtrim(ktime,2)
    if (keyword_set(debug)) then begin
      print,msg
      stop
    endif else message,msg ; generate error for handler
  endif


  ;==== Copy the data we have into the output structure.

  a.uday = uday
  a.mf1 = mf1
  a.mf2 = mf2
  a.ishot = ishot
  a.ktime = ktime

  a.shot = ishot ;;; JMS 98.04.27

  ;==== Handle multiple versions of AEQDSK file

  ; Determine how many magnetic probes there should be.  Efit versions
  ; before 6/27/88 had only 29 probes, otherwise there are 60 unless the shot
  ; number is greater than 91000 in which case there are 68 probes.
  ; 
  ; Y2K compliant - Accommodate 4-digit year without breaking 2-digit 
  ; year for version number. QP 12-7-99

  nvernum= long(strcompress(strmid(mf2,1,4),/remove_all)+$
  	strmid(mf1,0,2)+strmid(mf1,3,2))
  if(nvernum ge 880627) then begin
    if(ishot lt 91000) then magprobemax=60 else magprobemax=68
  endif else begin
    magprobemax=29
  endelse

  numecoil_rd = 2
  numeqdata = 96

  ; Determine how many ecoil segments there should be. Efit versions 
  ; after 7/11/95 increase the count to 6 from 2.  Also, efit versions
  ; after 7/11/95 increase the number of eqdata values to 100 from 96.

  if (nvernum gt 950711) then begin
    numecoil_rd = 6 
    numeqdata = 100
  endif

  ; The 9/6/95 version has rho_qmin added (rho = norm sqrt of volume)

  if (nvernum ge 950906) then begin
    numeqdata = 104
  endif

  ; The 7-24-96 version has rmidin and rmidout

  if (nvernum ge 960724) then begin
    numeqdata = 108
  endif

  if (nvernum ge 970904) then begin     ;added psurfa
    numeqdata = 112
  endif

  if (nvernum ge 20000309) then begin   ;add 4 lines, last being zvsod
    numeqdata = numeqdata + 16
  endif

  if (nvernum ge 20030206) then begin	;add 1 line, last being rq21top
    numeqdata = numeqdata + 4
  endif

  if (nvernum ge 20150604) then begin   ;add 1 line, last assigned field being twagap 
    numeqdata = numeqdata + 4
  endif 


  ;==== END handle multiple versions of AEQDSK file


  ;
  ; Create the second group of variables.
  ;

  tttt = float(0)
  jflag = long(0)
  lflag = long(0)
  limloc = 'aaaa'
  mco2v = long(0)
  mco2r = long(0)
  qmflag = 'aaa'
    
  ;
  ; Read the second group of variables.
  ;
    
  if (fileformatted) then begin
    readf,lun,ttt1,format='(1x,4e16.0)'
    string =''
    readf,lun,string	; convert later to avoid format problem. 1-28-99 QP
    string = strtrim(strmid(string,19),1)  ; strip out time and leading space
    string = str_sep(strcompress(strtrim(string,2)),' ')
    limloc = string[2]		; less format dependent. 2-5-99 QP
    mco2v = Long(string[3])
    mco2r = Long(string[4])
    qmflag = string[5]
  endif else begin
    readu,lun,ttt1
    readu,lun,tttt,jflag,lflag,limloc,mco2v,mco2r,qmflag
  endelse
    
  a.limloc = limloc
  a.mco2v = mco2v
  a.mco2r = mco2r
  a.qmflag = qmflag

  a.time = double(round(ttt1*1000))/1000.   ; change so that sub-ms times are read correctly to 3 decimal places 

  ;
  ; create the third group of variables.
  ;
    
  eqdata = fltarr(numeqdata)
    
  ;
  ; Initialize with dummy return values.
  ;
    
  eqdata(*) = 999.0
  jst = 1
    
  for jj=1,21 do begin
    if( (jj eq 7) or (jj eq 8) ) then begin
      jend = jst + mco2v - 1
    endif else if( (jj eq 9) or (jj eq 10) ) then begin
      jend = jst
    endif else begin
      jend = jst + 3
    endelse
    tsize = jend-jst+1
    temp=fltarr(tsize)
    if(fileformatted) then begin
      readf,lun,temp,format='(1x,4e16.0)'
    endif else begin
      readu,lun,temp
    endelse
    eqdata(jst-1:jend-1)=temp
    jst = jend + 1
  endfor
    
  ;
  ; Be sure that zxpt1 is always the position of the lower X point if
  ; there is one and that zxpt2 is always the position of the upper
  ; X point if there is one.
  ;
    
  zxpt1 = eqdata(54-1)
  rxpt1 = eqdata(53-1)
  zxpt2 = eqdata(56-1)
  rxpt2=  eqdata(55-1)
    
  ;
  ; if( (zxpt1 gt 0) or ( (zxpt2 lt 0) and (zxpt2 ne -999.0) ) ) then begin
  ;
    
  if( (zxpt2 ne -999.0) and (zxpt1 gt zxpt2) ) then begin
    eqdata(54-1) = zxpt2
    eqdata(53-1) = rxpt2
    eqdata(56-1) = zxpt1
    eqdata(55-1) = rxpt1
  endif
    
  ;
  ; Create the next set of variables and read them.
  ;
    
  if (nvernum ge 970524) then begin
    ;
    ; Read in the number of psi loops, magnetic probs, fcoils and ecoils.
    ; if version 5-24-97; convert INTEGER to LONG for the correct format for
    ; unformatted reading.
    ;
    numpsi = long(numpsi)
    magprobemax = long(magprobemax)
    numfcoil = long(numfcoil)
    numecoil_rd = long(numecoil_rd)
    if (fileformatted) then begin
      readf,lun,numpsi,magprobemax,numfcoil,numecoil_rd,format='(1x,4i5)'
    endif else begin
      readu,lun,numpsi,magprobemax,numfcoil,numecoil_rd 
    endelse

  endif
    
  psf = fltarr(numpsi)
  bmag = fltarr(magprobemax)
  fcoil = fltarr(numfcoil)
    
  ;
  ; Initialize with dummy return values.
  ;
    
  fcoil(*) = 999999.0
  psf(*) = 999999.0
  bmag(*) = 999999.0
  
  if(fileformatted) then begin
    readf,lun,psf,bmag,format='(1x,4f16.0)'
    readf,lun,fcoil,format='(1x,4f16.0)'
  endif else begin
    readu,lun,psf,bmag
    readu,lun,fcoil
  endelse

  ; By pass non-fatal errors - temporary solution for A eqdsk files from 
  ; other Tokamaks.  12-17-98 QP.
    
  IF (NOT(Keyword_Set(debug))) THEN catch,error_status ELSE error_status=0
  IF error_status NE 0 THEN BEGIN
     print,'Warning: ',!err_string,' Continue...'    
     error_status = 0
     catch,/cancel    
     goto,pass
  ENDIF

  ;
  ; Copy the bmag array into the structure of type mpi in the output
  ; structure a.
  ;

  if (magprobemax le n_tags(a.mpi)) then begin
  	for i=0,magprobemax-1 do a.mpi.(i) = bmag(i)
  endif else begin
	print,'Error: Magprobemax > n_tags(a.mpi)'
	ntags = n_tags(a.mpi)
	help,magprobemax,ntags
  endelse

  ;
  ;convert the f coil amp-turns to amps/turn.
  ;
    
  reada_fcoil_convert,fcoil

    
  ;
  ; Copy the fcoil array into the structure of type fcoil in the output
  ; structure a.
  ;
 
  if (numfcoil le n_tags(a.fcoil)) then begin  
  	for i=0,numfcoil-1 do a.fcoil.(i) = fcoil(i)
  endif else begin
	print,'Error: Numfcoil > n_tags(a.fcoil)'
	ntags = n_tags(a.fcoil)
	help,numfcoil,ntags
  endelse
    
  ;
  ; Reference the psi loops to 1a.
  ;
    
  reada_psf_convert,psf
    
  ;
  ; Copy the psf array into the structure of type psf in the output
  ; structure a.
  ;
 
  if (numpsi le n_tags(a.psf)) then begin  
  	for i=0,numpsi-1 do a.psf.(i) = psf(i)
  endif else begin
	print,'Error: Numpsi > n_tags(a.psf)'
	ntags = n_tags(a.psf)
	help,numpsi,ntags
  endelse  

pass:

  ;
  ; Next set of variables.
  ;
    
  eccurt = fltarr(numecoil_rd)
    
  if (fileformatted) then begin
    readf,lun,eccurt,format='(1x,4f16.0)'
  endif else begin
    readu,lun,eccurt
  endelse
    
  a.eccurt(0:numecoil_rd-1) = eccurt
    
  ;
  ; If numecoil_rd = 2 then copy the values from the first 2 elements of
  ; a.eccurt to the other 4 as is done by efit.
  ;
    
  if (numecoil_rd eq 2) then begin
    a.eccurt(3-1) = a.eccurt(1-1)
    a.eccurt(5-1) = a.eccurt(1-1)
    a.eccurt(4-1) = a.eccurt(2-1)
    a.eccurt(6-1) = a.eccurt(2-1)
  endif
    
  ;
  ; Read the rest of the a file.
  ;
  ; Read eqdata elements 77 through numeqdata. If these values do not exist
  ; in the file then the returned values will be left at -999.0 as
  ; initialized above.
  ;
    
  on_ioerror,NO92
  jst = 77 
  jjn=fix((numeqdata-jst+1)/4)
  
  for jj=1,jjn do begin
    jend = jst + 3
    temp = fltarr(jend-jst+1)
    if(fileformatted) then begin
      readf,lun,temp,format='(1x,4e16.0)'
    endif else begin
      readu,lun,temp
    endelse
    eqdata(jst-1:jend-1) = temp
    jst = jend + 1
  endfor    

NO92:

  on_ioerror,NULL

  ;
  ; Copy the eqdata array into the structure of type eqdata in the output
  ; structure a.
  ;
    
  for i=0,numeqdata-1 do a.d.(i) = eqdata(i)
    
  ;
  ; Compute the normalized beta and current.
  ;
    
  bsurf=abs(a.d.bcentr)*a.d.rcenrm/a.d.rsurf		;B at geometric center
  a.d.in = ABS(a.d.ipmhd)*1.0e-6/(a.d.aminor*0.01)/bsurf
  a.d.betan = a.d.betat/a.d.in
  
  ;
  ; convert meters to centimeters for rmid in and out
  ;
    
  a.d.rmidin = a.d.rmidin * 100.
  a.d.rmidout = a.d.rmidout * 100.
    
  ;
  ; All done.
  ; At this point, there were no errors.
  ;
    
  a.error = 0
  status = 1

  ;
  ; close the file and free the lun
  ;
  free_lun,lun
    
  return,a


end
      
;-------------------------------------------------------------------------------------
; MDSplus SPECIFIC CODE
;-------------------------------------------------------------------------------------

function reada_mdsstruct,tags,data,istart,iend
  s=0
  istr=strtrim(istart,2)
  s = create_struct(tags(istr),data(istr))
  for i=istart+1,iend do begin
    istr=strtrim(i,2)
    s = create_struct(s,tags(istr),data(istr))
  endfor
  return,s
end


function reada_mdsmakestr,data,tags
  increment=20
  ntags=n_elements(tags)
  nmax=fix(ntags/increment)
  iextra=((nmax*increment+1 lt ntags) and (nmax gt 0))

  if (nmax gt 0) then begin
    s=reada_mdsstruct(tags,data,0,increment) 
  endif else begin
    s=reada_mdsstruct(tags,data,0,ntags-1)
  endelse

  for i=1,nmax-1 do s=create_struct(temporary(s),reada_mdsstruct(tags,data,i*increment+1,(i+1)*increment))

  if (iextra) then s=create_struct(temporary(s),reada_mdsstruct(tags,data,nmax*increment+1,ntags-1))

  return,s

end

function reada_mdsheader,itime,verbose=verbose,debug=debug,status=status

  quiet = (1- keyword_set(debug))

  code_version = mdsvalue('\TOP.RESULTS:CODE_VERSION',quiet=quiet,status=status)
  if (keyword_set(debug)) then print,'CODE_VERSION: ',code_version
  if (status) then begin
    pieces = str_sep(code_version,'/')
    if n_elements(pieces) ge 3 then begin
    mf1 = pieces[0] + '/' + pieces[1]
    mf2 = '/' + pieces[2]
    endif else begin
    mf1 = pieces[0]
    mf2 = '?'
    end
  endif else begin
    mf1 = '?'
    mf2 = '?'
  endelse


  date_run = mdsvalue('\TOP.RESULTS:DATE_RUN',quiet=quiet,status=stat)
  if (not(status)) then date_run='?'
  if (keyword_set(debug)) then print,'DATE_RUN: ',date_run
  status = status and stat

  test = mdsvalue('GETNCI("\\TOP.RESULTS.AHEADER","FULLPATH")',/quiet,status=stattest)
  if (stattest) then headerpath = '\TOP.RESULTS.AHEADER' else headerpath = '\EFIT_AEQDSK'
  limloc = mdsvalue(headerpath+':LIMLOC',quiet=quiet,status=stat)
  if (stat) then begin
    ; Some EFIT trees store LIMLOC as a giant string, so check the data type
    ; and fix that before trying to index the data.
    if size(limloc,/dimensions) eq 0 then begin
       temp = strmid(limloc,0,4)
       for i=4,strlen(limloc),4 do temp = [temp,strmid(limloc,i,4)]
       limloc = temp
    endif
    limloc = limloc[itime]
  endif else limloc='?'
  if (keyword_set(debug)) then print,'LIMLOC: ',limloc
  status = status and stat

  qmflag = mdsvalue(headerpath+':QMFLAG',quiet=quiet,status=stat)
  if (stat) then begin
    if itime lt n_elements(qmflag) $
    then qmflag = qmflag[itime] $
    else qmflag = qmflag[0]     ; use the 1st value, a scalar in CMOD efits
  endif else qmflag='?'
  if (keyword_set(debug)) then print,'QMFLAG: ',qmflag
  status = status and stat

  mco2v = n_elements(mdsvalue('GETNCI("\\TOP.RESULTS.AEQDSK:PATHV*","NID_NUMBER")',quiet=quiet,status=stat1))
  mco2r = n_elements(mdsvalue('GETNCI("\\TOP.RESULTS.AEQDSK:PATHR*","NID_NUMBER")',quiet=quiet,status=stat2))
  status = status and stat1 and stat2
  return,{uday:date_run, mf1:mf1, mf2:mf2, limloc:limloc, qmflag:qmflag, mco2v:mco2v, mco2r:mco2r}
end


function reada_mdata,shot0,t0,verbose=verbose,debug=debug,status=status
  ; Jeff Schachter 98.05.16 - this function is not complete
  ; Its purpose is to return the data from the MEASUREMENTS area
  ; that is found in the "header" of the AEQDSK structure.
  ; Still to be added: A.PSF and A.FCOIL

  quiet = (1-keyword_set(debug))
  mtime = mdsvalue('\MTIME',quiet=quiet,status=status)
  if (status) then begin
    im = where(mtime eq t0[0],nm)
    status = (nm eq 1)
    if (status) then begin

      cecurr = mdsvalue('\TOP.MEASUREMENTS:CECURR',quiet=quiet,status=statce)
      if (statce) then begin
	if (size(cecurr))[0] eq 1 then cecurr = cecurr[im] $
				  else cecurr = cecurr[*,im]
      endif
      status = status and statce
      if (not(status)) then efit_read_message,verbose,'Could not read CECURR'

      cmpr2 = mdsvalue('\TOP.MEASUREMENTS:CMPR2',quiet=quiet,status=statcm)
      if (statcm) then begin
	if (size(cmpr2))[0] eq 1 then cmpr2 = cmpr2[im] $
				 else cmpr2 = cmpr2[*,im]
	b = mpi__define(shot0)   
	n = min([n_elements(cmpr2),n_tags(b)])
	for i=0,n-1 do b.(i) = cmpr2[i]
      endif 
      status = status and statcm
      if (not(status)) then efit_read_message,verbose,'Could not read CMPR2'

      csilop = mdsvalue('\TOP.MEASUREMENTS:CSILOP',quiet=quiet,status=statsi)
      if (statsi) then begin
	if (size(csilop))[0] eq 1 then csilop = csilop[im] $
				  else csilop = csilop[*,im]
        reada_psf_convert,csilop ; normalize to flux loop 1a
	psf = {psf}
	n = n_elements(csilop)
	statsi = (n eq n_tags(psf))
	if (statsi) then for i=0,n-1 do psf.(i) = csilop[i] 
      endif
      status = status and statsi
      if (not(status)) then efit_read_message,verbose,'Could not read CSILOP'

      ccbrsp = mdsvalue('\TOP.MEASUREMENTS:CCBRSP',quiet=quiet,status=statbr)
      if (statbr) then begin
	if (size(ccbrsp))[0] eq 1 then ccbrsp = ccbrsp[im] $
				  else ccbrsp = ccbrsp[*,im]
	reada_fcoil_convert,ccbrsp ; convert from amp-turns to amps/turn
	fcoil = {fcoil}
	n = n_elements(ccbrsp)
	statbr = (n eq n_tags(fcoil))
	if (statbr) then for i=0,n-1 do fcoil.(i) = ccbrsp[i]
      endif
      status = status and statbr
      if (not(status)) then efit_read_message,verbose,'Could not read CCBRSP'


    endif
  endif else efit_read_message,verbose,'Could not read MTIME'
  if (status) then begin
    return,{mpi:b, eccurt:cecurr, psf:psf, fcoil:fcoil} 
  endif else return,{mpi:'?', eccurt:'?', psf:'?', fcoil:'?'}
end

   
function reada_mdsread,info,itime,verbose=verbose,debug=debug,status=status

  forward_function efit_read_error, efit_read_mds_tags, mdsvalue, efit_read_getmdstime

  quiet = (1 - keyword_set(debug))
  atime = efit_read_getmdstime('A',debug=debug,status=status)

  if (status) then begin   
    ntime = n_elements(atime)
    siginfo = efit_read_mds_tags(info.type,debug=debug)
    status = siginfo.status

    if (status) then begin
      ;=== now read all parents (should be very fast)
      ntags = n_elements(siginfo.tags)
      stats = lonarr(ntags)
      data = fltarr(ntags)
      for i=0,ntags-1 do begin
        if (keyword_set(debug)) then print,siginfo.parents[i]
	d = mdsvalue(siginfo.parents[i],quiet=quiet,status=stat)
	;=== do not return data from signals with different timebase
	;=== (for example, DENSITY) - cannot subscript it with itime
        if (stat) then stat = (size(d,/tname) ne 'STRING')
        if (stat) then begin
          sz = size(d)
          case (sz[0]) of 
            0 : begin
              data[i] = d
              stat = 1
            end
            1 : begin
              stat = (n_elements(d) eq ntime) 
              if (stat) then data[i] = d[itime]
            end
            2 : begin
              itimedim = (where(sz eq ntime, ntimedim))[0]-1  ; convert to 0-based index
              stat = (ntimedim eq 1)
              if (stat) then begin
                case (itimedim) of
                  0 : data[i] = d[itime,*]
                  1 : data[i] = d[*, itime]
                  else : message,'weird'
                endcase
              endif
            end
            else : stat = 0
          endcase
          stats[i]=stat
        endif
      endfor
      ;=== get list of nodes from which data was obtained ok
      i = where((stats and 1) eq 1,n)
      status = (n gt 0)
      if (status) then begin
	;=== make a structure only out of nodes from which data was obtained ok
	;=== subscript both data and reada_tags so they stay aligned.
	data = data[i]
	tags = siginfo.tags[i]
	struct = reada_mdsmakestr(data,tags)

	;=== get header info and probe/current data
	ahead = reada_mdsheader(itime,verbose=verbose,debug=debug,status=stathead)
	mdata = reada_mdata(info.shot,atime[itime],verbose=verbose,debug=debug,status=statmdata)

	source = 'MDSplus, shot = '+strtrim(info.shot,2)+', run = '+info.runid+', time = '+strtrim(atime[itime[0]],2)
        a={shot:info.shot, time:atime[itime[0]], tree:info.runid, error:0, source:source, d:struct, $
	   uday:ahead.uday, mf1:ahead.mf1, mf2:ahead.mf2, ktime:1, $
	   mco2v:ahead.mco2v, mco2r:ahead.mco2r, $
	   limloc:ahead.limloc, qmflag:ahead.qmflag, $
	   mpi:mdata.mpi, eccurt:mdata.eccurt, psf:mdata.psf, fcoil:mdata.fcoil}

	if (not(stathead)) then efit_read_message,verbose,'READA_MDSREAD:  no header information available'
      endif else efit_read_message,keyword_set(debug),'READA_MDSREAD: No valid data'
    endif else efit_read_message,keyword_set(debug),'READA_MDSREAD: Unable to get signal info'
  endif else efit_read_message,keyword_set(debug),'READA_MDSREAD: Unable to get signal info'
	
  if (status) then begin
    efit_read_message,verbose,'READA_MDSREAD: MDSplus read from '+info.runid+' '+strtrim(info.shot,2)+' successful.'

  endif else begin

    a = efit_read_error()
    efit_read_message,verbose,'READA_MDSREAD: Error reading MDSplus AEQDSK data '+strtrim(info.shot,2)+' '+info.runid+' '+strtrim(info.time,2)
    if (keyword_set(debug)) then stop

  endelse


  return,a

end


;-------------------------------------------------------------------------------------

function reada, arg1, arg2, mks=mks, mode=mode, runid=runid, info=info, source=source, $
			    exact_time=exact_time, time_range=time_range, maxdt=maxdt, $
                            server=server, filter=filter, quiet=quiet, $
                            verbose=verbose, debug=debug, status=status 

  forward_function efit_read, read_units, efitde_read

  if keyword_set(quiet) then verbose=0

  if n_elements(runid) gt 0 then begin
    if strupcase(strtrim(strjoin(runid),2)) eq 'EFITDE' then begin
      a = efitde_read(arg1,type='a',debug=debug,status=status,server=server)
      return,a
    endif
  endif

  a = efit_read(arg1, arg2, type='a', mode=mode, runid=runid, info=info, source=source, $
			    exact_time=exact_time, time_range=time_range, $
                            server=server, maxdt=maxdt, quiet=quiet, $
			    verbose=verbose, debug=debug, status=status)


  ;=================== *** NOTE *** =================== 
  ; 
  ;AEQDSK data coming from a file written by EFIT will 
  ;be in CGS units.
  ;
  ;AEQDSK data coming from MDSplus will be in MKS.
  ;
  ;The function READA_UNITS is called to change from
  ;units system to the other.  If the MKS keyword is 
  ;set, it does nothing if the data was read from 
  ;MDSplus, and converts to MKS if the data was read 
  ;from a file.
  ;
  ;If the CGS keyword is set, it converts if the data
  ;was read from MDSplus, and does nothing if the data
  ;was read from a file.
  ;
  ;=================== *** NOTE *** =================== 

  if (status) then begin
    if keyword_set(filter) then begin
     ; filter if chisq or error is too big
       filter_chisq = 100		; default cutoffs for chisq, error
       filter_error = 0.01
       if n_elements(filter) eq 2 then begin
	  filter_chisq = filter[0]	; filter[0] can be cutoff for chisq
	  filter_error = filter[1]	; filter[1] can be cutoff for error
       endif
       if (a.d.chisq gt filter_chisq) or (a.d.eps gt filter_error) then begin
	  message,strcompress(a.shot,/re)+strcompress(a.time)+" rejected: "+ $
	   "chisq="+strcompress(a.d.chisq)+" error="+strcompress(a.d.eps),/info
	  return,{error: 2}
       endif
    endif

    a = reada_units(temporary(a),source,MKS=MKS,VERBOSE=VERBOSE, DEBUG=DEBUG, STATUS=STATUS)
  endif

  return,a

end
