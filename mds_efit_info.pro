function default_str,shot 
 	print,'Could not connect to SQL'
	machine 	= mdsvalue('MACHINE()')
        runs            = ['EFIT01','EFIT02','EFIT11','EFITRT','EFITRT1','EFITRT2','EFITRT_GA','EFIT_EAST','EFITRT_EAST']
        trees           = ['EFIT01','EFIT02','EFIT11','EFITRT','EFITRT1','EFITRT2','EFITRT_GA','EFIT_EAST','EFITRT_EAST']
        runids          = [shot,shot,shot,shot,shot,shot,shot,shot,shot]
        runtypes        = ['DEFAULT','DEFAULT','DEFAULT','DEFAULT','DEFAULT','DEFAULT','DEFAULT','DEFAULT','DEFAULT']
        comments        = ['Could not connect to SQL.  This run may / may not exist... efitpicker is listing this as a default run name.', $
                           'Could not connect to SQL.  This run may / may not exist... efitpicker is listing this as a default run name.', $
                           'Could not connect to SQL.  This run may / may not exist... efitpicker is listing this as a default run name.', $
                           'Could not connect to SQL.  This run may / may not exist... efitpicker is listing this as a default run name.', $
			   'Could not connect to SQL.  This run may / may not exist... efitpicker is listing this as a default run name.', $
                           'Could not connect to SQL.  This run may / may not exist... efitpicker is listing this as a default run name.', $
                           'Could not connect to SQL.  This run may / may not exist... efitpicker is listing this as a default run name.', $
                           'Could not connect to SQL.  This run may / may not exist... efitpicker is listing this as a default run name.', $
			   'Could not connect to SQL.  This run may / may not exist... efitpicker is listing this as a default run name.' ]
        userids         = ['DEFAULT','DEFAULT','DEFAULT','DEFAULT','DEFAULT','DEFAULT','DEFAULT','DEFAULT','DEFAULT']
        runtags         = ['DEFAULT','DEFAULT','DEFAULT','DEFAULT','DEFAULT','DEFAULT','DEFAULT','DEFAULT','DEFAULT']
 	dates		= ['DEFAULT','DEFAULT','DEFAULT','DEFAULT','DEFAULT','DEFAULT','DEFAULT','DEFAULT','DEFAULT'] 
	experiments	= [machine,machine,machine,machine,machine,machine,machine,'EAST','EAST']
     	return,{runs:runs,trees:trees,runids:runids,runtypes:runtypes,comments:comments,userids:userids,runtags:runtags,stat:1,dates:dates,experiments:experiments}
end

function mds_efit_info,shot,machine,debug=debug
  forward_function dsql

  if ((machine eq 'DIII-D') && (shot eq 0) && (!VERSION.OS ne 'darwin')) then shot=mdsvalue('CURRENT_SHOT("D3D")')

  catch,error_status
  if error_status ne 0 then begin
     catch,/cancel
     return,default_str(shot)
  endif

  sexpr = set_experiment(machine)
  if (sexpr.db eq '') then return,default_str(shot)
  set_database,sexpr.db,status=status,/quiet ;code_rundb   
  if ((status eq 0) || (status eq -1)) then return,default_str(shot)

  mdsserver = sexpr.server 
  nchar = strlen(mdsserver)
  cmd = 'select convert(varchar, date_run, 102), REPLACE(experiment,''D3D'',''DIII-D''), tree, run_id, '+$
        'run_type, run_comment, run_by, runtag from plasmas '+$
        'where code_name=''EFIT'' and shot=? and deleted=0 and '+$
        '( (mdsserver like '''+mdsserver+'%'') or mdsserver is null ) ' +$
        'order by run_id, tree'
  n=dsql(cmd,shot,dates,experiments,tree,runid,run_type,run_comments,run_by,runtag)

  if (n gt 0) then begin
    runs = strupcase(tree)
    i = where(runs eq 'EFIT' or runs eq 'EFITDE',n)
    if (n gt 0) then begin
      runs[i] = strtrim(runid[i],2)
    endif
    return,{dates:dates, $
	    experiments:experiments, $
	    runs:runs, $
            trees:strtrim(strupcase(tree),2), $
            runids:runid, $
            runtypes:strtrim(strupcase(run_type),2), $
            comments:strtrim(run_comments,2), $
            userids:strtrim(run_by,2), $
            runtags:strtrim(runtag,2), $
            stat:1}
  endif else return,{stat:0}

end

