
function efit_shotlist,path,types=types,reverse=reverse_flag

  typeLen=1

  ;==========================================================================================
  ;======
  ;====== Get list of EFIT files
  ;======
  ;====== CODE IS OPERATING SYSTEM DEPENDENT!
  ;======
  ;==========================================================================================

  case (!VERSION.OS_FAMILY) of
    'unix' : begin

      if (keyword_set(types)) then begin
	typeLet = "["+types[0]
        for i=1,n_elements(types)-1 do typeLet = typeLet + types[i]
	typeLet = typeLet + "]"
      endif else typeLet = "[a-zA-Z]"

      pushd,path
        subs = subdirs()
        i = where(strpos(subs,'shot') eq 0,n)
        if (n gt 0) then dirs=['.',subs(i)] else dirs=['.']
       
        cmds = 'ls '+dirs+'/'+typeLet+'[0-9][0-9][0-9][0-9][0-9][0-9].[0-9][0-9][0-9][0-9][0-9]_[0-9][0-9][0-9] 2> /dev/null       
        ncmds=n_elements(cmds)
        spawn,['sh','-c',cmds(0)],files,/noshell
        for i=1,n_elements(cmds)-1 do begin
          spawn,['sh','-c',cmds(i)],morefiles,/noshell
          files = [temporary(files), temporary(morefiles)]
        endfor
      popd       
    end

    'vms' : begin
      pushd,path
	files = efit_getvmsfilenames()	; written by Bill Davis. Released 98.04.17
      popd
    end

  endcase


  ;==========================================================================================
  ;======
  ;====== Now parse files returned to determine shots, types, and times
  ;======
  ;==========================================================================================

  i=where(files ne '',n)

  if (n gt 0) then begin

    files = temporary(files[i])
    info = efit_filename_parse(files)
    iTimeIndep = where(info.times eq -1, nTimeIndep)
    if (nTimeIndep gt 0) then begin
      timeIndep = {shots:info.shots[iTimeIndep], types:info.types[iTimeIndep], files:files[iTimeIndep]}
    endif else timeIndep = {shots:[0], types:[''], files:['']}  ; structure must be defined even if no time
								; independent EFIT files

    iTimeDep = where(info.times ge 0., nTimeDep)

    if (nTimeDep gt 0) then begin
      files=files[iTimeDep]

      isort=sort(info.shottimes[iTimeDep])
      iuniq = uniq(info.shottimes[iTimeDep[isort]])
      files=temporary(files[isort])

      ishotuniq=uniq(info.shots[iTimeDep[isort[iuniq]]])
      shots=info.shots[iTimeDep[isort[iuniq[ishotuniq]]]]
      times=info.times[iTimeDep[isort[iuniq]]]

      idxTimes = [-1,ishotuniq]
      idxTypes = [-1,iuniq]


      ntimes=n_elements(times)
      typeList=strarr(ntimes)
      for i=0,ntimes-1 do begin    
        i0=idxTypes[i]+1
        i1=idxTypes[i+1]
        n=i1-i0+1
        typeList[i]=string(reform(byte(info.types[iTimeDep[isort[i0:i1]]]),n*typeLen))
      endfor
      types = info.types[iTimeDep[isort]]
    endif else begin
      files = ['']
      shots = [0l]
      times = [0.0d0]
      idxTimes = [0]
      types = ['']
      idxTypes = [0]
      typeList = ['']
    endelse 

    retval={files:files, shots:shots, times:times, idxTimes:idxTimes, types:types, $
	    idxTypes:idxTypes, typeList:typeList, timeIndep:timeIndep, status:1}
  endif else retval={status:0}

  return,retval

end

  

