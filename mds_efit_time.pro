; 07-30-2004 Q.P. DIII-D EFITDE do not have tags, use paths directly.

function mds_efit_time,shot,run,count=count
;  forward_function common_elements, mdsopen, mdsvalue
  forward_function common_elements, mdsvalue
  mdsopen,run,shot,/quiet,status=stat
  if (stat) then begin
    if (mdsvalue('MACHINE()') eq 'CMOD') then begin
      time = mdsvalue('\EFIT_AEQDSK:TIME',/quiet,status=stat)
    endif else begin
      IF (run NE 'EFITDE') THEN BEGIN
         gtime=mdsvalue('\GTIME',/quiet,status=stat_g)
         atime=mdsvalue('\ATIME',/quiet,status=stat_a)
      ENDIF ELSE BEGIN
         gtime=mdsvalue('.g:time',status=stat_g)
         atime=mdsvalue('.a:time',status=stat_a)
      END
      if (stat_g and stat_a) then time=common_elements(atime,gtime,status=stat) else stat=0
    endelse
  endif 
  mdsclose,run,shot,/quiet
  if (stat) then begin
    count=n_elements(time)
  endif else begin
    count=0
    time=[0.]
  endelse
  return,time
end
