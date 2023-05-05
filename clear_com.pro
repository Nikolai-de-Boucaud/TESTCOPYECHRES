pro Clear_Com, save_profs=save_profs, save_inputs=save_inputs
; clears common storage after reading a configuration file

Common ECHCOM
Common WIDGETIDS, WidIDs

if not keyword_set(save_profs) then save_profs=0
if not keyword_set(save_inputs) then save_inputs=0

if not save_inputs then begin
	clear_struct, ECHSys
	PrgDat.Status.PlotRays[*]=0
endif

if not save_profs then begin
	clear_struct, PrgDat.Profiles
	PrgDat.cql_done=0B
	PrgDat.cql_pwr[*]=0.
	PrgDat.cql_cur[*]=0.
	PrgDat.cql_rya[*]=0.
	PrgDat.cql_totpwr=0.
	PrgDAt.cql_totcur=0.
endif


PrgDat.Status.ArchivedData=0
PrgDat.Status.OutputsValid=0

if PrgDat.Prefs.nlinputfile eq '' then ECHSys.Tank.xfrac[*]=1.

widget_control, WidIDs.ConfigTable, set_value=ECHSys.Tank
widget_control, WidIDs.InputTable, set_value=ECHSys.InputTable
;widget_control, WidIDs.OutsValid, set_value=PrgDat.Status.OutputsValid
widget_control, WidIds.RayTrace, set_value=PrgDat.Status.PlotRays
widget_control, WidIds.Archive, set_value=PrgDat.Status.ArchivedData
widget_control, WidIds.Bt0, set_value=Prgdat.Profiles.Bt0
widget_control, WidIds.CentralDensity, set_value=PrgDat.Profiles.CentralDensity
widget_control, WidIds.CentralTe, set_value=PrgDat.Profiles.CentralTe

end
