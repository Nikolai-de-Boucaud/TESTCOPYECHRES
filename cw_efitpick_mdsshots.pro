pro cw_efitpick_mdsshots_set,id,value
  widget_control,widget_info(id,/child),get_uvalue=u
  widget_control,u.listID,set_value=value
  widget_control,u.fieldID,set_value=value[0]
end

function cw_efitpick_mdsshots_get,id
  widget_control,widget_info(id,/child),get_uvalue=u
  widget_control,u.mapped,get_value=value
  return,value
end

function cw_efitpick_mdsshots_query,ev
  widget_control,ev.id,get_uvalue=uquery
  if (widget_info(uquery.id,/valid)) then begin
    widget_control,uquery.id,/map,/show
  endif else begin
    widget_control,ev.top,/hourglass
    uquery.id = cw_efitpick_query(uquery.base)
    widget_control,ev.id,set_uvalue=uquery
  endelse
  return,''
end

function cw_efitpick_mdsshots_manage,ev
  widget_control,ev.id,get_uvalue=oManage
  if (obj_valid(oManage)) then begin
    oManage->ShowDialog
  endif else begin
    oManage = obj_new('RunManager', 'EFIT', group_leader=ev.top, parentTree='MHD')
    widget_control,ev.id,set_uvalue=oManage
  endelse
  return,''
end
  
function cw_efitpick_mdsshots_getlist,ev
  widget_control,ev.id,get_uvalue=bBoard
  widget_control,widget_info(bBoard,/child),get_uvalue=u
  widget_control,u.baseField,map=0
  widget_control,u.baseList,map=1
  u.mapped = u.listID
  widget_control,widget_info(bBoard,/child),set_uvalue=u
  event = {CW_EFITPICK_MDSSHOT_GETLIST, $
           id:bBoard, $
           top:ev.top, $
           handler:0l}
  return,event
end

function cw_efitpick_mdsshots_event,ev
  widget_control,ev.top,/hourglass
  widget_control,widget_info(widget_info(ev.handler,/parent),/child),get_uvalue=u
  ev.id = u.base
  return,ev
end

function cw_efitpick_mdsshots,parent,event_func=event_func,height=height,shot=shot,_EXTRA=e

  if not(keyword_set(shot)) then shot=0
  bBoard = widget_base(parent, event_func=event_func, space=0, $
                       func_get_value='cw_efitpick_mdsshots_get', $
                       pro_set_value='cw_efitpick_mdsshots_set')

  baseList = widget_base(bBoard, /column, space=0, map=0, event_func='cw_efitpick_mdsshots_event') ; first child!
  listID   = cw_listfield(baseList, xsize=9, ysize=height, /long,   format='(i6)', title='Shot', _EXTRA=e)

  baseField = widget_base(bBoard, /column, map=1, event_func='cw_efitpick_mdsshots_event')
  bx = widget_base(baseField, /column)
  bx = widget_base(bx, /column, /base_align_center, /frame)
  x = widget_label(bx, value="Enter Shot")
  fieldID = cw_field(bx, title=" ", /long, xsize=10, /return)
;  fieldID = cw_field_check(bx, title=" ", /long, xsize=10, /column, /nolabel, value=strtrim(shot,2))

;  but = widget_button(bx, value="List of Shots", event_func='cw_efitpick_mdsshots_getlist', uvalue=bBoard)
  uquery = {base:bBoard, id:0l}
  but = widget_button(bx, value="Query Database", event_func='cw_efitpick_mdsshots_query', uvalue=uquery)
  but = widget_button(bx, value="Manage my Runs", event_func='cw_efitpick_mdsshots_manage', uvalue=obj_new())

  x = widget_label(bx, value=" ")

  widget_control,widget_info(bBoard,/child),set_uvalue={baseList:baseList, baseField:baseField, base:bBoard, $
                                                        fieldID:fieldID, listID:listID, mapped:fieldID}

  return,bBoard

end
