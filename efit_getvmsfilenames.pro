;---------------------------------------------------------------
;+
; efit_getvmsfilenames.pro
; 
; Returns filenames (whole pathnames, actually) with names:
;      '%%%%%%%.%%%%%' or '%%%%%%%.%%%%%_%%%' or '%%%%%%%.nc'
; where the second through seventh characters in the filename are numbers.
; Filenames returned are just from the current directory, unless
; keywords say otherwise. 
;
; KEYWORDS:
;    SUBDIR - if present, search just this. e.g., '[.mysubs]', or 'mysubs'
;    ALLSUBS - if present, (and subdir not) searches subdirectories of the 
;              current directory, satisfying the name 'SHOTnnnnnn'
;              where 'n' is a digit.
;
; Example:  efitFileNames = getEFileNames_VMS()
;
; EASE OF USE: Can be used with existing documentation
;
; OPERATING SYSTEMS:  OpenVMS 
;
; EXTERNAL CALLS:  none
;
; RESPONSIBLE PERSON: Bill Davis
;
; Written: 17-Apr-98 Bill Davis
;---------------------------------------------------------------
FUNCTION efit_getvmsfilenames, SUBDIR=subdir, ALLSUBS=allsubs, SHOT=shot

subs = ''
IF N_ELEMENTS(subdir) GT 0 THEN BEGIN
    IF STRPOS ( subdir, '[' ) LT 0 THEN subdir = '[.'+subdir+']'
    subs = subdir
ENDIF ELSE IF N_ELEMENTS(allsubs) GT 0 THEN BEGIN
    subs = subdirs()
ENDIF
shotIndices = where(strpos( STRUPCASE(subs),'SHOT') eq 2,ndirs)

IF ndirs GT 0 THEN shotSubs = subs( shotIndices )

IF N_ELEMENTS(shot) EQ 0 THEN BEGIN
    fileSpecs = ['%%%%%%%.%%%%%' , '%%%%%%%.%%%%%_%%%', '%%%%%%%.nc' ]
ENDIF ELSE BEGIN
    fileSpecs = ['%'+strshot+'.%%%%%' ,    $
                 '%'+strshot+'.%%%%%_%%%', $
                 '%'+strshot+'.nc' ]
ENDELSE

;;;t0 = SYSTIME(1)
files = [file_search( fileSpecs(0) ), $
         file_search( fileSpecs(1) ), $
         file_search( fileSpecs(2) )     ]
        
FOR i=0, ndirs-1 DO BEGIN
	moreFiles = [ file_search( shotSubs(i)+fileSpecs(0) ), $
	              file_search( shotSubs(i)+fileSpecs(1) ), $
	              file_search( shotSubs(i)+fileSpecs(2) )    ]
	files = [temporary(files), temporary(morefiles)]
ENDFOR

   ; eliminate files that are not annnnnn.*

IF N_ELEMENTS( files ) EQ 0 THEN files=''  ELSE BEGIN
	   ; strip version numbers
	scpos = STRPOS( files, ';' )
	FOR i = 0, N_ELEMENTS( files ) -1 DO BEGIN
	   files(i) = STRMID(  files(i), 0, scpos(i))  
	ENDFOR
	   ; strip directory info from files
	FOR i = 0, N_ELEMENTS( files ) -1 DO BEGIN
	   lbpos = STRPOS( files(i), ']')
	   files(i) = STRMID(  files(i), lbpos+1, 1000)  
	ENDFOR
ENDELSE

lbpos = STRPOS(files, ']')  ; now there are no left brackets in filename
FOR i = 0, N_ELEMENTS( files ) -1 DO BEGIN
    
    char = STRMID( files(i), lbpos(i)+1, 1)
    asciiValue =  BYTE(char)
    asciiValue = asciiValue(0)
    IF ( asciiValue GE 97 AND asciiValue LE 122)  OR  $
       ( asciiValue GE 65 AND asciiValue LE 90 ) THEN BEGIN
       isaNumber = 1
       FOR ic = lbpos(i)+2, lbpos(i)+7 DO BEGIN
           char = STRMID( files(i), ic, 1)
           asciiValue = BYTE( char )
           asciiValue = asciiValue(0)
           IF ( asciiValue LT 48 OR asciiValue GT 57 ) THEN isaNumber = 0
       ENDFOR
       IF isaNumber THEN BEGIN  ; first part of filename is valid
          ; now check for a valid extension
          
           extension = STRMID( files(i), lbpos(i)+9, 9)
           extOK = 0
           IF extension EQ 'NC' THEN extOK = 1 ELSE BEGIN
               extOK = 1
               FOR ic = 0, 4 DO BEGIN
		           char = STRMID( extension, ic, 1)
		           asciiValue = BYTE( char )
		           asciiValue = asciiValue(0)
                   IF ( asciiValue LT 48 OR asciiValue GT 57 ) THEN extOK = 0
               ENDFOR
	           IF STRLEN( extension ) EQ 9 THEN BEGIN
	              FOR ic = 6,8 DO BEGIN
		              char = STRMID( extension, ic, 1)
		              asciiValue = BYTE( char )
		              asciiValue = asciiValue(0)
                      IF ( asciiValue LT 48 OR asciiValue GT 57 ) THEN extOK = 0
	              ENDFOR
	           ENDIF 
           ENDELSE

           IF extOK THEN BEGIN
	           IF N_ELEMENTS( goodFiles ) EQ 0 THEN BEGIN
	              goodFiles = files(i)
	           ENDIF ELSE BEGIN
	              goodFiles = [goodFiles, files(i)]
	           ENDELSE
           ENDIF
       ENDIF
    ENDIF

ENDFOR

;;;Print, SYSTIME(1)-t0, ' for finding ', N_ELEMENTS( goodFiles ), ' files'

IF N_ELEMENTS( goodFiles ) EQ 0 THEN goodFiles = '' 

return, goodFiles
end


