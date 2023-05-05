pro cw_efitpick_query_done,ev
  widget_control,ev.top,map=0
end

pro cw_efitpick_query_event,ev
  forward_function cw_efitpick_state_get
  t = tag_names(ev,/str)
  if (t eq '') then begin

    ;;; user selected a row in RunSelector

    if (ev.select) then begin
      shot = ev.shot
    endif else shot = 0l
    widget_control,ev.top,get_uvalue=ux
    widget_control,ux.top,get_uvalue=utop
    u=cw_efitpick_state_get(ux.top,/noparent)
    widget_control,u.f.mdsShots,set_value=shot
    cw_efitpick_state_set,ux.top,u,/noparent
    if (ev.select) then begin
      event = {CW_EFITPICK_QUERY, $
               id:ev.id, $
               top:ev.top, $
               handler:ev.handler, $
               tree:ev.tree,$
	       run_id:ev.run_id}
      widget_control,ev.id,send_event=event  
      ;;; Above needed because setting value of u.f.mdsShots generates
      ;;; event which sets value of u.f.mdsruns.  This generated event
      ;;; must happen first, and then can call cw_listfield_set_selected 
      ;;; to set the particular run.  Would be much cleaner if these
      ;;; were all objects, but oh well.
    endif

  endif else begin

    ;;; generated event to set value of run widget
    ;;; run list can be either tree (efit01,02,...) or run_id (efit,efitde)

    widget_control,ev.top,get_uvalue=ux
    widget_control,ux.top,get_uvalue=utop
    u=cw_efitpick_state_get(ux.top,/noparent)
    id = u.f.mdsruns
    cw_efitpick_state_set,ux.top,u,/noparent
    cw_listfield_set_selected,id,[strupcase(ev.tree),strtrim(ev.run_id,2)]
  endelse

end


function cw_efitpick_query,wTop
  user = getenv('USER')
  wTLB = widget_base(/column, title="Query Database for EFIT runs", $
                     group_leader=wTop, event_pro='cw_efitpick_query_event')

  oRS = obj_new("RunSelector",code='EFIT', parent=wTLB, /notify, $
                mandatoryColumns=['run_by','run_type'], $
                mandatoryWhere='deleted=0',$
                orderBy='shot desc, tree asc, run_id', nSimple=2, $
               simpleValue = {andor:'', column:'RUN_BY', operator:'=', text:user[0]})

  r = widget_base(wTLB, /row)
  dismiss = widget_button(r, value="Done", event_pro='cw_efitpick_query_done')

  widget_control,wTLB,/realize,set_uvalue={top:wTop, oRS:oRS}
  xmanager,'cw_efitpick_query',wTLB,/no_block
  return,wTLB
end
