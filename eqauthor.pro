FUNCTION eqauthor, eq_struct
; 12-03-98 Q.Peng
; The function returns the author program of the G structure (eq_struct).

s = Size(eq_struct)
type = s[s[0]+1]
IF type NE 8 THEN Return,''		; Non structure.

					; No ECASE tag to indicate author
IF (Where(Tag_Names(eq_struct) EQ 'ECASE'))[0] LT 0 THEN Return,'' 
					; Return name of author program.
Return, StrCompress(eq_struct.ecase[0],/Remove_All)
END