;***********************************************************************
;
;  4dlib/EFITLIB/getafiles.pro
;
;  created:
;    8-19-91
;
;  modified:
;   10-19-2001	 Q.P. cd to path then reada from the current dir to avoid
;                file_search failing on long path. cd back afterwards.
;     2-8-99	 Q. Peng - Convert to MKS unit in reada only when mdsplus
;		 is installed.
;     9-8-97     M. Wade -- Change '*' to '.*' to make search work
;                on VMS systems.
;    11-1-95	 K. Greene -- If EFIT "a" files are not found in current
;		 directory or in "shot..." subdirectory, then look for
;		 them in default directory:  /scratch/shot...
;   20150625 SMF Removed reliance on {aeqdsk} now that reada.pro no 
;                a named structure (and is no an anonymous structure).     
;
;***********************************************************************
;
; return an array of structures containing all of the afiles for a specified
; shot.
;

FUNCTION getafiles,shot_number,ierr,mode=mode,run=run,path=path

Forward_Function mdsplus_intalled, getenv_

; Check keywords
 
  if not Keyword_Set(mode) then mode='FILE'
  if not Keyword_Set(run) then run=''
  if (!VERSION.OS_FAMILY eq 'unix') then begin
      if not Keyword_Set(path) then path='./' else path = path+'/'
      delim = '/'
      scratchDir = '/scratch/'
  endif else if (!VERSION.OS_FAMILY eq 'vms') then begin
      if not Keyword_Set(path) then path=''
      delim = '.'
      scratchDir = ''
  endif else if (!VERSION.OS_FAMILY eq 'MacOS') then begin
      if not Keyword_Set(path) then path=''
      delim = ':'
      scratchDir = ''
  endif

cd,cur=current_dir
cd,path

filename = 'a'+$
           strtrim(string(shot_number,'(i6.6)'),2)+'.*'
s = file_search(filename,count=count)

if (!VERSION.OS_FAMILY eq 'vms') then BEGIN
   path = STRMID( path, 0, STRLEN(path)-1 ) + '.'
   delim = ']'
ENDIF

if count eq 0 then $
	s =  file_search('shot' +  strtrim(string(shot_number,'(i6.6)'),2)+ $
	     delim+'a' + strtrim(string(shot_number,'(i6.6)'),2) +  '.*',count=count)

if count eq 0 then  $
	s =  file_search('shot' +  strtrim(string(shot_number),2)+ $
	     delim+'a' + strtrim(string(shot_number,'(i6.6)'),2) +  '.*',count=count)

if count eq 0 then $
	s =  file_search(scratchDir+'shot' +  strtrim(string(shot_number),2)+ $
	     delim+'a' + strtrim(string(shot_number,'(i6.6)'),2) +  '.*',count=count)

mdsplus_installed = mdsplus_installed()
IF StrUpcase(getenv_('MDSPLUS')) EQ "NO" THEN mdsplus_installed = 0
if count ne 0 then begin
  a = reada(s(0),MKS=mdsplus_installed)		; mks units
  n=n_elements(s)
  afiles = [a]
  for i=1,n-1 do afiles=[afiles,reada(s(i),MKS=mdsplus_installed)]
endif else afiles={err:1}
cd,current_dir

if count eq 0 then ierr = 1 else ierr = 0

return,afiles
end
