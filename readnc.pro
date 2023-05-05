; readnc.pro

; arrsca
; Converts structure referenced in the first argument to one in the second
; argument by converting all elements of size 1 to scalars. This makes the help
; command more useful. Does the same thing for any argument not a structure.
@$ECHRES_PATH/arrsca

; make_array1.pro
; Define a modification of IDL's make_array function
; that can also make scalar variables if requested
function make_array1, s
if s(0) ne 0 then begin
	array = make_array(size=s)
	return, array
endif else begin
	if s(1) eq 1 then scalar=1b
	if s(1) eq 2 then scalar=1
	if s(1) eq 3 then scalar=1l
	if s(1) eq 4 then scalar=1.0
	if s(1) eq 5 then scalar=1d
	if s(1) eq 7 then scalar=''
	return, scalar
endelse
end

; Restore all variables from a netCDF file and 
; return a structure containing these variables;
; the structure uses the variable names from the
; netCDF file as tag names in the structure.
;

function readnc, filenamein, no_arrsca=no_arrsca
; set no_arrsca to skip changing arrays of size 1 to scalars

catch, error_status
if error_status ne 0 then begin
	print, '  !!! readnc error: '+!error_state.msg
	return, {filename:''}	; if nc file couldn't be opened
endif
cdfid = ncdf_open(filenamein,/NOWRITE)	; Open netCDF file
glob = ncdf_inquire( cdfid )		; Find out general info

filename=file_expand_path(filenamein)

; create a stucture for output
struct = create_struct('filename',filename)

for i=0,glob.nvars-1 do begin
    info = ncdf_varinq(cdfid, i)
	; print, info.name
    ; get dimension/type/length of this variable
    s = lonarr(info.ndims+3)
    s(0) = info.ndims
    ntot=1
    for n=0,info.ndims-1 do begin
        ncdf_diminq, cdfid, info.dim(n), name, size
        s(n+1) = size
        ntot=ntot*size
    endfor
    s(info.ndims+2)=ntot
    if info.datatype eq 'BYTE'   then s(info.ndims+1)=1
    if info.datatype eq 'INT'    then s(info.ndims+1)=2
    if info.datatype eq 'LONG'   then s(info.ndims+1)=3
    if info.datatype eq 'FLOAT'  then s(info.ndims+1)=4
    if info.datatype eq 'DOUBLE' then s(info.ndims+1)=5
    if info.datatype eq 'CHAR'   then s(info.ndims+1)=7
    ; Make an IDL variable to store the netCDF variable
    vartem = make_array1(s)
    ; fill IDL variable with values from netCDF file
    ncdf_varget, cdfid, i, vartem
    ; if necessary, convert variable to an IDL string
    if info.datatype eq 'CHAR' then vartem=string(vartem)
    ; put variable into structure with appropriate tag name
    struct = create_struct(struct,info.name,vartem)
endfor

ncdf_close,cdfid	; close netCDF file

if not keyword_set(no_arrsca) then begin
	arrsca, struct, struct2 
	return, struct2
endif else return, struct

end

