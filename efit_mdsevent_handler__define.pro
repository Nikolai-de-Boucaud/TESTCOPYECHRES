PRO efit_mdsevent_handler::ShowRegistry
message,'',/info
END

PRO efit_mdsevent_handler::HandleEvent
shot = mdsvalue('CURRENT_SHOT("D3D")',/quiet,status=status)
Message,strcompress(shot)+' '+self.event,/info
IF self.event EQ 'NEW_SHOT' THEN runid = 'EFITRT1' $
			    ELSE runid = (Str_Sep(self.event,'_'))[0]
Widget_Control,self.widgetID,Set_Value={shot:shot,runid:runid,now:1}
END

FUNCTION efit_mdsevent_handler::GetStatus
Return, 1
END

FUNCTION efit_mdsevent_handler::Init, event
status = self->mdsevent_handler::Init(event,/debug)
Return, status
END

PRO efit_mdsevent_handler__define
s = {EFIT_MDSEVENT_HANDLER, $
     Inherits MDSEVENT_HANDLER $
    }
END

