;; efitde_read.pro
;; This function reads a design efit from MDSplus and returns the data 
;; in a structure.  Set the type keyword to 'a' or 'g' to specify
;; either the aeqdsk or geqdsk data.
;; 07-30-2004 Q.P. Ignore type m.

function efitde_read, shot, type=type, server=server, $
                      status=status, debug=debug

  status=0
  errorStruct = {error:1L}
  debug = keyword_set(debug)

  if not debug then begin
    catch, error
    if error ne 0 then begin
      message,'Unexpected error: '+!ERROR_STATE.MSG,/info
      return, errorStruct
    endif
  endif

  if n_elements(shot) ne 1 then begin
    message, 'usage: data = efitde_read(shot [,type={"a","g"}] ' + $
        '[,status=status]',/info
    return,errorStruct
  endif

  type = keyword_set(type) ? type : 'a'

  if type eq 'm' then begin
    if keyword_set(debug) then message, 'No M data exists for EFITDE',/info
    return,errorStruct
  endif

  status = mdsplus_setup(server=server)
  if not status then message,'Could not initialize MDSplus'
  
  MdsOpen, 'EFITDE', shot, status=status
  if not status then begin
    message,'error when opening EFITDE tree:'+mdsgetmsg(status),/info
    return,errorStruct
  endif

  MdsTCL,'set def \top.'+type,status=status
  if not status then begin
    message,'could not set def to .'+type+' branch',/info
    return,errorStruct
  endif

  data = {shot:long(shot)}

  forward_function efitde_read_r
  efit = efitde_read_r(debug=debug)

  data = create_struct(data,efit)

  MdsClose, 'EFITDE', shot, status=status

  status=1
  return,data
end
