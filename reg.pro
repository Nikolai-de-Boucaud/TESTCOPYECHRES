; Defines ECHCOM for echres codes
Common ECHCOM, ECHSys, Results, PrgDat, fce0, fuh0, frhco0, rays, gstruct, ffstruct, rad_res, echres_uvalue


envel=getenv('USER')
if envel eq '' then envel=getenv('USERNAME')
if envel eq '' then envel=getenv('LOGNAME')
if envel eq '' then begin
	result=dialog_message('Environment variable USER must be set to proceed')
	exit
endif		
end
