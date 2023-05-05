;; efitde_read_r.pro
;; recursive read (used because reada returns structures with
;; structures) for use with efitde_read.pro.

function efitde_read_r, debug=debug
  
  dbg = keyword_set(debug)

  data = {error:0}

  nodes = MdsValue('getnci("*","NODE_NAME")',status=status,/quiet)
  for i=0, n_elements(nodes)-1 do begin
    node = strtrim(nodes[i],2)
    value = MdsValue(node,status=status,/quiet)
    if status then data = create_struct(node,value,data) else $
        message,'warning: could not get data from '+nodes[i],/info
  endfor

  subs = MdsValue('getnci(".*","NODE_NAME")',status=status,/quiet)
  if status then begin
    for i=0, n_elements(subs)-1 do begin
      sub = strtrim(subs[i],2)
      MdsTCL,'set def .'+sub
      if dbg then message,'recursively checking .'+sub+'...',/info 
      subdat = efitde_read_r(debug=debug)
      if size(subdat,/type) eq 8 then data = create_struct(data,sub,subdat)
      MdsTCL,'set def .-'
    endfor
  endif

  return,data
end
