;; efitde_write.pro
;; Writes efit structure to MDSplus. 
;; Set the setup keyword to have this function add nodes.  Note that
;; it does NOT open the tree for edit or write changes.  Suggested
;; usage is that you check the return value and write changes if > 0.
function efitde_write, efit, setup=setup, debug=debug, status=status

  dbg = keyword_set(debug)

  if size(efit,/type) ne 8 then begin
    message,'1st arg to this function is a structure',/info
    return,0
  endif

  tags = tag_names(efit)

  ;; For each tag, figure out the data type and if we need to add it 
  ;; to the model.  Ignore the shot and error tags.
  written=0
  for i=0, n_elements(tags)-1 do begin
    type=0
    if tags[i] ne 'SHOT' and tags[i] ne 'ERROR' then begin
      status = execute('type = size(efit.'+tags[i]+',/type)')
      if status then begin
        
        ;; If we have a numeric or text value, write to MDSplus tree.
        ;; If setup keyword is set, we will write the node first.
        ;; Otherwise we just try to MdsPut the value.
        if type gt 1 and type lt 8 then begin
          
          if keyword_set(setup) then begin

            ;; Check for the node and add it if it is not there.
            ;; Don't write the value to the node.
            expr = 'getnci("'+tags[i]+'","NID_NUMBER")'
            nid  = MdsValue(expr,/quiet)
            if nid[0] eq 0 then begin
              expr = 'add node '+tags[i]+'/usage='
              expr += (type eq 7) ? 'text' : 'numeric'
              if dbg then message,expr,/info
              MdsTCL,expr,status=status
              if not status then message,'warning: this failed: '+expr,/info $
              else written++
            endif

          endif else begin

            ;; Just write the value.
            status = execute('value = efit.'+tags[i])
            if not status then begin
              message,'warning: problem with efit.'+tags[i],/info
            endif else begin
              MdsPut, tags[i], '$', value, status=status
              if not status then message,'warning: error writing to '+$
                  tags[i],/info
            endelse

          endelse
          
        endif else begin
          
          ;; If we have a strucutre, then make a structure node
          ;; if necessary and if setup is set, then call this function
          ;; recursively.
          if type eq 8 then begin
            if keyword_set(setup) then begin
              expr = 'getnci(".'+tags[i]+'","NID_NUMBER")'
              nid = MdsValue(expr,/quiet)
              if nid[0] eq 0 then MdsTCL,'add node .'+tags[i]
            endif
            MdsTCL,'set def .'+tags[i]
            status = execute('sub = efit.'+tags[i])
            if not status then message,'error when doing efit.'+tags[i]
            written += efitde_write(sub, setup=setup, debug=debug)
            MdsTCL,'set def .-'
            
          endif else message,'warning: '+tags[i]+' cannot be added ' + $
              '(wrong data type)',/info
          
        endelse
        
      endif else message,'warning: execute() error with tag '+tags[i],/info
      
    endif
    
  endfor
  
  return, written

end
