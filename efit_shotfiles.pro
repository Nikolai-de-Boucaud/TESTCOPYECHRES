;revised 98.05.15: bug in efit_shotfiles_info 
; - should have returned complete set of info for time indep files
; 07-20-99 QP If 'ls' returns empty file list (because of arg list too long), 
;             split input types and try one at a time and cat the
;             results.
;
;2000.01.07 - allow any file extension FNNNNNN.TTTTT* and convert only 
;             those of form FNNNNNN.TTTTT_TTT to sub-millisecond form 
;             - Jeff Schachter


function efit_shotfiles_info,files,info,indecies,typeLen,timeDep=timeDep,timeIndep=timeIndep
  if (keyword_set(timeDep)) then begin
    ix = where(info.times[indecies] ge 0.,nx)
  endif else begin
    ix = where(info.times[indecies] lt 0.,nx)
  endelse;;

  if (nx gt 0) then begin
    times = info.times[indecies[ix]]
    types = info.types[indecies[ix]]

;    if (keyword_set(timeDep)) then begin
      isort = sort(times)
      types = temporary(types(isort))
      iuniq = uniq(times[isort])
      idxTypes = [-1,iuniq]

      tuniq = times[isort[iuniq]]
      nuniq=n_elements(tuniq)
      typeList=strarr(nuniq)
      for i=0,nuniq-1 do begin    
        i0=idxTypes[i]+1
        i1=idxTypes[i+1]
        n=i1-i0+1
        typeList[i]=string(reform(byte(types[i0:i1]),n*typeLen))  
      endfor
      return,{ntime:nx,                     $
	    files:files[indecies[ix]],      $
	    times:info.times[indecies[ix]], $
	    types:info.types[indecies[ix]], $
	    tuniq:temporary(tuniq),         $
	    nuniq:temporary(nuniq),	    $
	    typeList:temporary(typeList)}
;    endif else begin
;      return,{ntime:nx,                     $
;	    files:files[indecies[ix]],      $
;    	    times:info.times[indecies[ix]]}
;    endelse
  endif else return,{ntime:0}

end

function efit_shotfiles,path,subdir=subdir,shot=shot,types=types

  forward_function efit_getvmsfilenames

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
      searchString = '[0-9][0-9][0-9][0-9][0-9][0-9].[0-9][0-9][0-9][0-9][0-9]*'
      if (keyword_set(types)) then begin
	    typeLet = "["+types[0]
        for i=1,n_elements(types)-1 do typeLet = typeLet + types[i]
	    typeLet = typeLet + "]"
      endif else typeLet = "[a-zA-Z]"
      if (not(keyword_set(subdir))) then subdir = '.'
      cmd = 'ls '+subdir+'/'+typeLet+searchString+' 2> /dev/null'
      pushd,path
      spawn,['sh','-c',cmd],files,/noshell
      ; Check if files is empty, if so, it could be arg list too long.
      ; split types and try again.  - 07/20/99 QP.
      if (n_elements(files) eq 1 and files[0] eq '') $
         and (keyword_set(types)) then begin	
         for i=0,n_elements(types)-1 do begin
            typeLet="["+types[i]+"]"
            cmd = 'ls '+subdir+'/'+typeLet+searchString+' 2> /dev/null'
	    spawn,['sh','-c',cmd],files0,/noshell
	    if i eq 0 then files = files0 else files = [files,files0]
         end
      endif
      popd       
    end
    'vms' : begin
        pushd,path
	;******* BILL DAVIS:  THIS FUNCTION NEEDS TO BE UPDATED TO THE NEW FUNCTIONALITY
	;******* PLEASE CONTACT JEFF FOR INFORMATION.
	    files = efit_getvmsfilenames(subdir=subdir, /allsubs)	; written by Bill Davis. Released 98.04.17
        popd
    end
    'MacOS' : begin
      if (keyword_set(subdir))then pathspec=path+subdir+':' else pathspec=path
      files=''
      for i=0,n_elements(types)-1 do $
    	    files=[files,file_search(pathspec+string(format='(a,''??????.*'')',types(i)))]
    	end
  endcase
  
  ;;;IF GETENV('DEBUG') NE '' THEN PRINT,'Files found: ', files

  i=where(files ne '',n)

  if (n gt 0) then begin

    files = temporary(files[i])
    info = efit_filename_parse(files)

    isort = sort(info.shots)
    shots=info.shots[isort[uniq(info.shots[isort])]]
    nshots=n_elements(shots)

    if (keyword_set(shot)) then begin
      ishots = where(shots eq shot,nshots)
      if (nshots eq 1) then shots=shots[ishots] else nshots = 0
    endif

    if (nshots gt 0) then begin
      ; for shots found in current directory, load up ptrs (will contain filenames, types, and times)
      ptrs = ptrarr(nshots,/allocate_heap)
      for i=0,nshots-1 do begin
        indecies = where(info.shots eq shots[i],nindecies)
        if (nindecies gt 0) then begin	; weird if it isn''t (cuz got the shot from info.shots)
          *ptrs[i] = {dep   : temporary(efit_shotfiles_info(files,info,indecies,typeLen,/timeDep)), $
		      indep : temporary(efit_shotfiles_info(files,info,indecies,typeLen,/timeIndep))}
        endif
      endfor
      return,{nshots:temporary(nshots), shots:temporary(shots), ptrs:temporary(ptrs)}
    endif else return,{nshots:0}
  endif else return,{nshots:0}

end

  

