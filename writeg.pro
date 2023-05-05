;+ 
; NAME: 
;	WRITEG
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
;	WRITEG, arg1 [,arg2] [,RUNID=runid] [,STATUS=status]

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
;	RUNID:  EFIT "run ID" to use in MDSplus.  This defaults to "EFIT01" - 
;		the non-MSE automatic control room EFIT.
;	
;	EXACT_TIME:  If set, forces READA to match the time specified, rather
;		     than using the default behavior of returning the nearest
;		     time.  
;
;	VERBOSE:  If set, READA will print out informational messages on its 
;                 progress.
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
;	GEQDSK file containing data retrieved for the shot and time specified.
;
; COMMON BLOCKS: 
;
;	None.  
;
; SIDE EFFECTS: 
;
;	Calls READG to read the EFIT from MDSplus.
;
; RESTRICTIONS:
;
;	None.
;
; PROCEDURE: 
;
;	WRITEG writes a GEQDSK data from an EFIT run stored in
;	MDSplus for a particular shot and timeslice.
;
; EASE OF USE: Can be used with existing documentation
;
; OPERATING SYSTEMS:  HP-UX, OSF/Unix, OpenVMS, MacOS
;
; EXTERNAL CALLS:  MDSplus
;
; RESPONSIBLE PERSON: Qian Peng and Jeff Schachter
;
; CODE TYPE: modeling, analysis, control  
;
; CODE SUBJECT:  handling, equilibrium
;
; DATE OF LAST MODIFICATION: 7/06/99
;
; MODIFICATION HISTORY:
;
;      1999.07.06:  Jeff Schachter - call readg instead of efit_read,
;                                    and clean up documentation header.
;      2004.06.09 Q.P. - Use the first 6 digits as shot number when creating
;                        G file. Needed for restoring EFIT from scratch tree.
;      2005.07.21 Q.P. - Fixed a bug in writeg_file when file is passed in
;      2012.04.09 SMF - Add support for microsecond file names (g######.#####_###)
;
;-

;-------------------------------------------------------------------------------------
; FILE SPECIFIC CODE
;-------------------------------------------------------------------------------------
PRO writeg_file, g, file=file

sshot = strtrim(g.shot,2);
IF strlen(sshot) GT 6 THEN BEGIN
   sshot = strmid(sshot,0,6)
   Message,'Using '+sshot+' for shot number',/Info
ENDIF
IF N_Elements(file) EQ 0 THEN BEGIN
   micro = (strtok(string(g.time,format='(f10.3)'),'.',/extract))[1]
   file = 'g'+string(sshot,format='(i6.6)')+'.'+string(g.time,format='(i5.5)')
   if ( long(micro) ne 0 ) then file=file+'_'+micro
ENDIF
openw, lun, file, /get_lun

format = '(5e16.8)'
ecase=['EFITD   ',g.code_version,'    #',string(sshot,format='(i6)'),$
	string(g.time,format='(i6)'),'']
printf,lun,ecase,3,g.mw,g.mh,format='(1a11,1a10,1a5,2a6,1a10,3i4)'
printf,lun,g.xdim,g.zdim,g.rzero,g.rgrid1,g.zmid,format=format
printf,lun,g.rmaxis,g.zmaxis,g.ssimag,g.ssibry,g.bcentr,format=format
printf,lun,g.cpasma,g.ssimag,0.,g.rmaxis,0.,format=format
printf,lun,g.zmaxis,0.,g.ssibry,0.,0.,format=format
printf,lun,g.fpol,format=format
printf,lun,g.pres,format=format
printf,lun,g.ffprim,format=format
printf,lun,g.pprime,format=format
printf,lun,g.psirz,format=format
printf,lun,g.qpsi,format=format
printf,lun,g.nbdry,g.limitr,format='(2i5)'
printf,lun,g.bdry[0:2*g.nbdry-1],format=format	; dim of bdry is not consistent with nbdry ?
printf,lun,g.lim,format=format
kvtor=0 & rvtor=0 & nmass=0
printf,lun,kvtor,rvtor,nmass,format='(i5,e16.9,i5)'
printf,lun,g.rhovn,format=format
IF (Where(g.epoten))[0] NE -1 THEN keecur = 1 ELSE keecur = 0
printf,lun,keecur,format='(i5)'
IF keecur THEN printf,lun,g.epoten,format=format

free_lun, lun
END

;-------------------------------------------------------------------------------------

PRO writeg, arg1, arg2, mode=mode, runid=runid, file=file, status=status, _EXTRA = e

  forward_function readg

  info=strtrim(arg1,2)+':'+strtrim(arg2,2)
  if (n_elements(runid) gt 0) then info=info+':'+strtrim(runid,2)
  if (n_elements(file) gt 0) then info=info+strtrim(file,2)
  routine_log,'/f/mdsplus/log/writeg.log',info=info,/no_callstack

  g = readg(arg1, arg2, mode='MDSPLUS', runid=runid, status=status, _EXTRA = e)

  IF status THEN writeg_file,g,file=file $
  ELSE Message,'failed reading G from MDSplus: '+strtrim(status,2),/Informational

end

