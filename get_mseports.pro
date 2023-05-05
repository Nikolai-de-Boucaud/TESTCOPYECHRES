;+
; NAME
;   Get_MESPorts
;
; PURPOSE
;   Returns MSE port information. 
;   Read from MSE files (msetup and mrz) or hard-code when info is unavailable.
;   Makowski provides a routine get_mse_config2.pro for the similar purpose
;   but it requires libmse.sl, which is inconvennient.
; 
; INPUTS AND KEYWORDS
;   shot: DIII-D shot number
;   all:  If set, return channel locations as well, default=0
;   goodportsonly: If set, filter off bad channels, default=0
;
; OUTPUT
;   Returns a structure with the following tags
;   vport:	vport names in integers
;   vports: 	vport names in strings (for 195L, 195U)
;   channel:    port numbers corresponding to vport (zeros are bad channels)
;   r,z:	MSE channel/port locations, omit if keyword all is unset.
;   err:        error code
;
; HISTORY
;
;   2007FEB23 Q.P - created
;   2007MAR01 Q.P - for longer-than-6-digit shot, assume it's from MDSplus
;                   scratch tree, convert to the real shot number
;-

FUNCTION Get_MSEPorts,shot,All=all,GoodPortsOnly=goodportsonly

on_error, 2 ; Return to caller
result = {error:0, msg:''}

; If scratch shot number (e.g. from MDS kinetic efit), find the real shot #
ss = StrCompress(shot,/re)
IF strlen(ss) GT 6 THEN shot = Long(strmid(ss,0,6))

msepath = getenv('MSE_CALIB_DIR')+'/'
IF shot LT 124000 THEN msetup = 'msetup.dat' ELSE msetup = 'msetup2.dat'
msetupfile = msepath + msetup

IF (file_search(msetupfile))[0] EQ '' THEN BEGIN
   Message,msetupfile+' cannot be found...',/Informational
   Return,result
ENDIF

  ; Read mse setup file to get the rz filename and,
  ; channel and vport info for the given shot.
  ; For early shots or shot >= 124000, channel and ports are hard-coded
  ; as they are not written in respective msetup file.

openr,lun,msetupfile,/get_lun
found = 0
mrzfile = ''
While NOT found AND NOT eof(lun) DO BEGIN
   string = ''
   readf,lun,string
   string = str_sep(strcompress(strtrim(string,2)),' ')
					; skip comment and blank lines
   IF strmid(string[0],0,1) NE '' AND strmid(string[0],0,1) NE '!' THEN BEGIN
      on_ioerror, pass1	
      shotmin = long(string[0])		; first read LINE 1 and skip the rest
      shotmax = long(string[1])
      IF shot GE shotmin AND shot LE shotmax THEN BEGIN
	 found = 1			; found the record for given shot.
					; use the 4th column for rz file name.
         mrzfile = msepath+'mrz_'+strcompress(string[3],/remove)+'.dat'
	 string = '' & str = '!'
	 IF shot LT 124000 THEN BEGIN
	 While NOT eof(lun) AND strmid(strtrim(str,2),0,1) NE '' DO BEGIN
	    str = ''			; read the rest of the record,
	    readf,lun,str		; and cat them into one string.
	    string = string + ' ' + str
	 END ; while
	 string = str_sep(strcompress(strtrim(string,2)),' ')
					; find channel numbers that are used.
	 nchan = N_Elements(string)/3	; each channel has 3 words,
	 channel = IntArr(nchan)	; channel number is the 3rd one.
         FOR i=0, nchan-1 DO channel[i] = Long(string[i*3+2])
					; hard-code vport for early shots.
	 IF shot LT 80540 THEN vports = StrArr(8)+'315' $
	 ELSE IF shot LE 91300 THEN vports = [StrArr(8)+'315',StrArr(8)+'45'] $
	 ELSE BEGIN			; read vport for shots with 
	    vports = StrArr(nchan)	; more than 35 channel MSE.
	    FOR i=0, nchan-1 DO vports[i] = string[i*3] ; take 1st word.
	 END ; else
         ENDIF ELSE BEGIN ; hard-code vport and channels if shot >= 124000
	   vports = [StrArr(11)+'315',StrArr(15)+'45',StrArr(10)+'15',$
	            StrArr(9)+'315',StrArr(8)+'195l',StrArr(16)+'195u'];
	   channel = IndGen(69)+1       ; channels 1-69
	   channel[37-1:40-1] = 0       ; 315 chords 37-40 not used anymore
         END
      ENDIF
pass1:on_ioerror,null			; turn off special handling.
   ENDIF
END
free_lun,lun

IF Keyword_Set(goodportsonly) THEN BEGIN
   index = where(channel NE 0)		; remove bad channels.
   channel = channel[index]
   vports = vports[index]
ENDIF

vport = Long(vports)
index = where(vports EQ '195l',c)
IF c GT 0 THEN vport(index) = 194	; for 195l channels
index = where(vports EQ '195u',c)
IF c GT 0 THEN vport(index) = 196	; for 195u channels

result = Create_Struct(result,'vport',vport,'vports',vports,'channel',channel)

IF NOT Keyword_Set(all) THEN Return,result

  ; Find mrz file.

IF mrzfile EQ '' OR (file_search(mrzfile))[0] EQ '' THEN BEGIN
   Message,'No MSE calibration file is found for'+strcompress(shot),/Info
   Return,result
ENDIF

  ; Read (R,Z) from mrz_nn.dat file.

openr,lun,mrzfile,/get_lun
r = 0.
z = 0.
While NOT eof(lun) DO BEGIN
   string = ''
   readf,lun,string
   string = str_sep(strcompress(strtrim(string,2)),' ')
   IF strmid(string[0],0,1) NE '!' AND strmid(string[0],0,1) NE '' THEN BEGIN
      on_ioerror, pass2
      IF string[1] EQ 'L' THEN BEGIN		; Left beam only.
         r = [r,float(string[2])]
         z = [z,float(string[3])]
      ENDIF
pass2:on_ioerror,null
   ENDIF
END
free_lun,lun

r = r[channel]				; channel starts from 1, but the 1st
z = z[channel]				; element of r,z is an extra 0.

Return,Create_Struct(result,'r',r,'z',z)

END
