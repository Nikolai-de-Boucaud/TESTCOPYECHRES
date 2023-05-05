;+ 
; NAME: 
;	WRITEA
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
;	WRITEA, arg1 [,arg2] [,RUNID=runid] 
;		             [,MKS=mks] 
;			     [,EXACT_TIME=exact_time]	
;			     [,VERBOSE=verbose] 
;			     [,DEBUG=debug] [,STATUS=status]

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
;	MKS:  If set, values are returned in MKS units, rather than cgs.
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
;	AEQDSK file containing data retrieved for the shot and time specified.
;
; COMMON BLOCKS: 
;
;	None.  
;
; SIDE EFFECTS: 
;
;	Calls READA to read the EFIT from MDSplus.
;
; RESTRICTIONS:
;
;	None.
;
; PROCEDURE: 
;
;	WRITEA writes an AEQDSK data from an EFIT run stored in
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
;      1999.07.06:  Jeff Schachter - call reada instead of efit_read,
;                                    and clean up documentation header.
;      09-06-2000 Q. Peng - correct the format of the 2nd line (one w/ shot)
;      04-02-2001 Q.P. - write zeros when mpi,fcoil,or psf not available
;      06-09-2004 Q.P. - Use the first 6 digits as shot number when creating
;                        A file. Needed for restoring EFIT from scratch tree.
;      2012.04.09 SMF - Add support for microsecond file names (a######.#####_###)
;
;-


;-------------------------------------------------------------------------------------
; FILE SPECIFIC CODE
;-------------------------------------------------------------------------------------

PRO writea_file,a,verbose=verbose,debug=debug,status=status,file=file

IF N_Elements(file) EQ 0 THEN BEGIN
   sshot = strtrim(a.shot,2);
   IF strlen(sshot) GT 6 THEN BEGIN
      sshot = strmid(sshot,0,6)
      Message,'Using '+sshot+' for shot number',/Info
   ENDIF
   micro = (strtok(string(a.time,format='(f10.3)'),'.',/extract))[1]
   file = 'a'+string(sshot,format='(i6.6)')+'.'+string(a.time,format='(i5.5)')
   if ( long(micro) ne 0 ) then file=file+'_'+micro
ENDIF
openw, lun, file, /get_lun

format='(1x,4e16.8)'
printf,lun,a.uday,a.mf1,a.mf2,format='(1x,a10,2a5)'
ktime = 1
printf,lun,sshot,ktime, format='(1x,i6,11x,i5)'
printf,lun,a.time,format=format
jflag=1 & lflag=0
printf,lun,a.time,jflag,lflag,a.limloc,a.mco2v,a.mco2r,a.qmflag,$
             format='(1h*,f7.2,10x,i5,11x,i5,1x,a3,1x,i3,1x,i3,1x,a3)'
d = a.d
printf,lun,d.CHISQ,d.RCENRM,d.BCENTR,d.IPMEAS,format=format
printf,lun,d.IPMHD,d.RSURF,d.ZSURF,d.AMINOR,format=format
printf,lun,d.KAPPA,d.TRITOP,d.TRIBOT,d.VOLUME,format=format
printf,lun,d.RCUR,d.ZCUR,d.QSTAR,d.BETAT,format=format
printf,lun,d.BETAP,d.LI,d.GAPIN,d.GAPOUT,format=format
printf,lun,d.GAPTOP,d.GAPBOT,d.Q95,d.NINDX,format=format

printf,lun,d.PATH1V,d.PATHV2,d.PATH3V,format=format
printf,lun,d.CO2D1,d.DENSV2,d.CO2D3,format=format
printf,lun,d.PATH1R,format=format
printf,lun,d.CO2DR,format=format

printf,lun,d.SHEAR,d.BPLOAV,d.S1,d.S2,format=format
printf,lun,d.S3,d.QL,d.SEPIN,d.SEPOUT,format=format
printf,lun,d.SEPTOP,d.PSIBDY,d.AREA,d.WMHD,format=format
printf,lun,d.EPS,d.ELONGM,d.QM,d.DIAMGC,format=format
printf,lun,d.ALPHA,d.RTTT,d.PSIREF,d.INDENT,format=format
printf,lun,d.RXPT1,d.ZXPT1,d.RXPT2,d.ZXPT2,format=format
printf,lun,d.SEPEXP,d.SEPBOT,d.BTM,d.BTVAC,format=format
printf,lun,d.RQ1,d.RQ2,d.RQ3,d.SEPLIM,format=format
printf,lun,d.RM,d.ZM,d.PSIM,d.TAUMHD,format=format
printf,lun,d.BETAPD,d.BETATD,d.WDIA,d.DIAMAG,format=format
printf,lun,d.VLOOPT,d.TAUDIA,d.QMERCI,d.TAVEM,format=format

numpsi = N_Tags(a.psf)
nummpi = N_Tags(a.mpi)
numfcoil = N_Tags(a.fcoil)
IF numpsi GT 0 AND nummpi GT 0 AND numfcoil GT 0 THEN BEGIN
psf = FltArr(numpsi)
mpi = FltArr(nummpi)
fcoil = FltArr(numfcoil)
FOR i=0, numpsi-1 DO psf[i] = a.psf.(i)
FOR i=0, nummpi-1 DO mpi[i] = a.mpi.(i)
FOR i=0, numfcoil-1 DO fcoil[i] = a.fcoil.(i)
printf,lun,numpsi,nummpi,numfcoil,N_Elements(a.eccurt),format='(1x,4i5)'
printf,lun,psf,mpi,format=format
printf,lun,fcoil,format=format
printf,lun,a.eccurt,format=format
ENDIF ELSE BEGIN
   printf,lun,1,1,1,1,format='(1x,4i5)'
   printf,lun,0.0,0.0,format=format 
   printf,lun,0.0,format=format
   printf,lun,0.0,format=format
END

printf,lun,d.PBINJ,d.RVSIN,d.ZVSIN,d.RVSOUT,format=format
wpdot=0.0						      ; not defined in MDSplus ?
if struct_hastag(d,'VSURF') then vsurf=d.VSURF else vsurf=0.0 ; check vsurf 
if struct_hastag(d,'WBDOT') then wbdot=d.WBDOT else wbdot=0.0 ; check wbdot
printf,lun,d.ZVSOUT,vsurf,wpdot,wbdot,format=format
;printf,lun,d.ZVSOUT,d.VSURF,d.WPDOT,d.WBDOT,format=format
printf,lun,d.SLANTU,d.SLANTL,d.ZUPERTS,d.CHIPRE,format=format
printf,lun,d.CJOR95,d.PP95,d.SSEP,d.YYY2,format=format
printf,lun,d.XNNC,d.CPROF,d.ORING,d.J0N,format=format
printf,lun,d.FEXPAN,d.QMIN,d.CHIGAM,d.SSI01,format=format
printf,lun,d.FEXPVS,d.SEPNOSE,d.SSI95,d.RHOQMIN,format=format

if struct_hastag(d,'CJ1AVE') then cj1ave=d.CJ1AVE else cj1ave=0.0 ; old trees don't have cj1ave  
printf,lun,d.CJOR99,cj1ave,d.RMIDIN,d.RMIDOUT,format=format
printf,lun,d.PSURFA,0.,0.,0.,format=format

  ;
  ; close the file and free the lun
  ;
  free_lun,lun

end
      

;-------------------------------------------------------------------------------------

PRO writea, arg1, arg2, mode=mode, runid=runid, file=file, status=status, _EXTRA=e

  forward_function reada

  info=strtrim(arg1,2)+':'+strtrim(arg2,2)
  if (n_elements(runid) gt 0) then info=info+':'+strtrim(runid,2)
  if (n_elements(file) gt 0) then info=info+':'+strtrim(file,2)
  routine_log,'/f/mdsplus/log/writea.log',info=info,/no_callstack

  a = reada(arg1, arg2, mode='MDSPLUS', runid=runid, status=status, _EXTRA = e)

  IF status THEN writea_file,a,file=file $
  ELSE Message,'failed reading A from MDSplus: '+strtrim(status,2),/Informational

end

