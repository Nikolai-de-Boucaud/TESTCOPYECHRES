FUNCTION readm_m, arg1, arg2, _Extra=extra

; 9-17-98
; A wrapper to official readm.  Readm should be called without the CONVERT
; keyword.  This routine adds more info to the returned structure for 
; mse_plot.
;	01-19-99 Updated vport according to /u/rice/idl/getmse_m0.pro
;	03-17-99 Set vport if they are all 0 in M structure.
;       04-29-02 Added energy parameter to the M structure (mam)
;	05-01-03 Added 9 new MSE channels to VPORT if they're 0s starting 2003.
;	03-16-05 MSE channels >36 should start at 108800 rather than 111982
;       23FEB07  Use get_mseports to get vport info, remove hard-coding

Forward_Function struct_hastag, struct_strip, get_mseports

m = readm(arg1,arg2,_Extra=extra)

   ; Add variables used by mse_plot (copied from readm_convert).

IF NOT m.error THEN BEGIN
  r_mp=2.4168                                   ;magnetic probe position
  z_mp=0.0

  gam_deg=atan(m.tangam)*180/!pi
  err_gam_deg=atan(m.siggam)*180/!pi
  nch = n_elements(m.tangam)
  ch = indgen(nch)+1
   
  m = Create_Struct(m,$
	'gam_deg',	gam_deg,$
	'err_gam_deg',	err_gam_deg,$
;	'vport',	vport,$
	'ch',		ch,$
	'r_mp',		r_mp,$
	'z_mp',		z_mp,$
	'mp67',		-m.expmpi(14))

  ; Check whether vport is passed in and also has value.
  ; The later EFIT should writes VPORT to M file. But if EFIT is created
  ; from a K file where VPORTs are all 0, it just pass them along.

  novport = 0
  IF NOT struct_hastag(m,'vport') THEN novport = 1 ELSE $
  IF (Where(m.vport NE 0))[0] EQ -1 THEN BEGIN
     m = struct_strip(m,'VPORT')		; vport all 0, remove it.
     novport = 1
  ENDIF

  IF novport THEN BEGIN				; adds VPORT info.
     ports = get_mseports(m.shot)
     m = Create_Struct(m,'vport',ports.vport)
  ENDIF

  ; Channel mapping for the 'energy' parameter

  if ( m.shot lt 80540 ) then begin
    energy = replicate('full',8)
  endif else if ( m.shot lt 91300 ) then begin
    energy = replicate('full',16)
  endif else if ( m.shot lt 97400 ) then begin
    energy = replicate('full',35)
  endif else if ( m.shot lt 100500 ) then begin
    energy = replicate('full',36)
  endif else if ( m.shot lt 108800 ) then begin
    energy = [ replicate('full',36), replicate('half',4) ]
  endif else begin
    energy = [ replicate('full',36), replicate('half',4),                   $
               replicate('full',5) ]
  endelse
  m = Create_Struct(m,'energy',energy)


ENDIF

Return, m
END
