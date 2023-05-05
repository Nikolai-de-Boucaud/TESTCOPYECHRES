; HISTORY:
;	04-12-2000 Q.Peng created
;	04-26-2000 Q.Peng passed the zoomed range to animation.
;       07-01-2003 make it compatible with CMOD efit, convert sec to ms
;                  use limitr[0],rgrid1[0],xdim[0],zdim[0],mw[0],mh[0]
;                  for all time slices
;       09-12-2003 add autoupdate option: when turned on, listens to mdsevent
;	           to test: /f/mdsplus/dispatching/set_event EFIT01_LOADED
;       09-15-2003 add command line arguments for fg, bg, line thickness (th)
;	09-17-2003 add command line -ch for charsize
;	06-15-2004 use timer event for autoupdate as a temporary solution
;		   will remove when MDSplus event works again
;		   Add command line -shot -runid -x -y -w -h -icon -auto
;	06-16-2004 non-floating control window, cannot iconify on Linux o.w.
;	06-25-2004 add -chth/-charthick option for char thickness
;       20120726   SMF: Disabled time step filter.  It prevents non-d3d EFITs
;                       from using the EFIT Animate features. 

;==============================================================================
Function Eq_Animate_Create,wEqAnimateBase,shot,runid,epref,$
			   wEqAnimate=wEqAnimate,winid=winid,draw=draw,$
			   tmin=tmin,tmax=tmax,step=step,goodfit=goodfit,$
			   zoom=zoom,xrange=xrange,yrange=yrange,$
			   charsize=charsize
;==============================================================================
Forward_Function getenv_

IF getenv_('DEBUG') EQ '' THEN Catch, error_status ELSE error_status=0
IF error_status NE 0 THEN BEGIN
   Message,!Err_String,/Info
   Return,-1
ENDIF

  ; Open mdsplus tree, get time base

IF N_Elements(runid) EQ 0 THEN tree='EFIT01' ELSE tree=runid
IF tree EQ '' THEN tree = 'EFIT01'
mdsopen,tree,shot,status=status
IF NOT status THEN BEGIN
   ok = Dialog_Message('Error in opening MDSplus tree'+StrCompress(shot)+' '+tree,Dialog_Parent=wEqAnimateBase)
   Return,-1
ENDIF

; determine MDSplus header path for the eqdsk
header = [mdsvalue('GETNCI($,"MINPATH")','\EFIT_AEQDSK',/quiet,status=status),$
          mdsvalue('GETNCI($,"MINPATH")','\EFIT_GEQDSK',/quiet,status=status)]
IF NOT status THEN header = ['\TOP.RESULTS.AEQDSK','\TOP.RESULTS.GEQDSK']
header = header+':'

time = mdsvalue('DIM_OF('+header[1]+'PSIRZ,2)')		; time
IF min(time) LE 1 THEN time = time*1000                 ; sec -> msec
ntime = N_Elements(time)

  ; Filter based on time range, step and fitting quality.

removed = lonarr(ntime)
filter = '1'
t0 = time[0]
IF N_Elements(tmin) NE 0 THEN IF tmin NE 0 THEN BEGIN
   index = Where(time GE tmin)
   IF index[0] EQ -1 THEN BEGIN
      ok = Dialog_Message('No time slices >'+StrCompress(tmin)+' available.',$
		Dialog_Parent=wEqAnimateBase)
      Return,-1
   ENDIF
   t0 = tmin
   filter = filter + ' AND time GE tmin'
   index = Where(time lt tmin and removed eq 0)
   if index[0] NE -1  THEN removed[index] = 1
ENDIF

IF N_Elements(tmax) NE 0 THEN IF tmax NE 0 THEN BEGIN
   index = Where(time LE tmax)
   IF index[0] EQ -1 THEN BEGIN
      ok = Dialog_Message('No time slices <'+StrCompress(tmax)+' available.',$
		Dialog_Parent=wEqAnimateBase)
      Return,-1
   ENDIF
   filter = filter + ' AND time LE tmax'
   index = Where(time gt tmax)
   if index[0] NE -1  THEN removed[index] = 1
ENDIF

; 20120726 SMF: Why was this a filter?  It prevents non-d3d EFITs from 
;               being able to use the efit animate featur.
;IF N_Elements(step) NE 0 THEN IF step NE 0 THEN BEGIN	; step is int or long
;   minstep = Min(time[1:ntime-1] - time[0:ntime-2])
;   IF minstep EQ 0 THEN minstep = 1.0
;   filter = filter + ' AND (time-t0)-Long(time-t0)/step*step LT minstep'
;   index = where((time-t0)-Long(time-t0)/step*step GE minstep)
;   IF index[0] NE -1  THEN removed(index) = 1
;ENDIF

IF Keyword_Set(goodfit) THEN BEGIN
   chisq = mdsvalue(header[0]+'CHISQ')
   error = mdsvalue(header[0]+'ERROR')
   filter = filter + ' AND chisq LE 100 AND error LE 0.01'
   index = where(removed eq 0 and chisq GT 100)
   IF index[0] NE -1  THEN removed(index) = 1
   index = where(error GT 0.01)
   IF index[0] NE -1  THEN removed(index) = 1
   n = n_elements(chisq)
   if (n lt n_elements(removed)) then removed(n:*) = 1
   n = n_elements(error)
   if (n lt n_elements(removed)) then removed(n:*) = 1
ENDIF

index = where(removed eq 0)
timeindex = where(removed eq 0)
;print,StrCompress(t0),filter
;IF filter NE '1' THEN result = Execute('timeindex = Where('+filter+')') $
;                 ELSE timeindex = Indgen(ntime)
IF timeindex[0] EQ -1 THEN BEGIN
   ok = Dialog_Message('No time slices available',Dialog_Parent=wEqAnimateBase)
   Return,-1
ENDIF
;if(n_elements(timeindex) ne n_elements(index) or $
;   total(timeindex) ne total(index)) then begin
;   print,time(timeindex)
;   print
;   print,time(index)
;endif

  ; Repeat the last slice for 10 more times to emulate pausing
timeindex = [timeindex,IntArr(10)+timeindex[N_Elements(timeindex)-1]]

  ; Retrive flux data.

psirz = mdsvalue(header[1]+'PSIRZ')		; flux
r = mdsvalue(header[1]+'R')
z = mdsvalue(header[1]+'Z')

limitr = mdsvalue(header[1]+'LIMITR')			; limiter
lim = mdsvalue(header[1]+'LIM')

nbdry = mdsvalue(header[1]+'NBDRY')			; boundary
bdry = mdsvalue(header[1]+'BDRY')

ssimag = mdsvalue(header[1]+'SSIMAG')			; psi at the mag axis
ssibry = mdsvalue(header[1]+'SSIBRY')			; psi at the boundary

r0 = mdsvalue(header[0]+'R0')				; magnetic axis
z0 = mdsvalue(header[0]+'Z0')

mw = mdsvalue(header[1]+'MW')
mh = mdsvalue(header[1]+'MH')
rgrid1 = mdsvalue(header[1]+'RGRID1')
zmid = mdsvalue(header[1]+'ZMID')
xdim = mdsvalue(header[1]+'XDIM')
zdim = mdsvalue(header[1]+'ZDIM')

rxpt1 = mdsvalue(header[0]+'RXPT1')			; X points
zxpt1 = mdsvalue(header[0]+'ZXPT1')
rxpt2 = mdsvalue(header[0]+'RXPT2')
zxpt2 = mdsvalue(header[0]+'ZXPT2')
rvsin = mdsvalue(header[0]+'RVSIN')
zvsin = mdsvalue(header[0]+'ZVSIN')
rvsout = mdsvalue(header[0]+'RVSOUT')
zvsout = mdsvalue(header[0]+'ZVSOUT')

  ; Save graphics variables, then set the new onew

oldP = !P
oldX = !X
oldY = !Y

bg = getenv_('BG') & IF bg EQ '' THEN bg = 'Background'
fg = getenv_('FG') & IF fg EQ '' THEN fg = 'Foreground'
thick = getenv_('THICK') & IF thick EQ '' THEN !P.Thick = 0.
charthick = getenv_('CHARTHICK') & IF charthick EQ '' THEN !P.CharThick = 0.
!P.Background = color_index(bg)
!P.Color = color_index(fg)
!P.Thick = thick
!P.CharThick = charthick
IF N_Elements(charsize) NE 0 THEN !P.CharSize = charsize ELSE !P.CharSize = 0
charsize = getenv_('CHARSIZE') & IF charsize NE '' THEN !P.CharSize = long(charsize) 
!X.Style = 5 & !Y.Style=5
IF Keyword_Set(zoom) AND N_Elements(xrange) NE 0 AND N_Elements(yrange) NE 0 $
THEN BEGIN
   !X.Range = xrange
   !Y.Range = yrange
ENDIF ELSE IF limitr[0] GT 0 THEN BEGIN		; set range by limiter
   inc = 0.2
   !X.Range = [min(lim[0,0:limitr[0]-1])-inc,max(lim[0,0:limitr[0]-1])+inc]
   !Y.Range = [min(lim[1,0:limitr[0]-1])-inc,max(lim[1,0:limitr[0]-1])+inc]
END

  ; Create or re-initialize Animation

IF N_Elements(draw) EQ 0 THEN draw=0L ELSE BEGIN
   IF Widget_Info(draw,/Valid) THEN Widget_Control,draw,Get_Value=winid
ENDELSE
IF N_Elements(winid) NE 0 THEN BEGIN
   WSet,winid
   xs = !D.X_VSize
   ys = !D.Y_VSize
ENDIF ELSE BEGIN
   xs = 200
   ys = 350
ENDELSE

IF N_Elements(wEqAnimate) EQ 0 THEN BEGIN
   wEqAnimate = CW_Animate(wEqAnimateBase,xs,ys,N_Elements(timeindex),/Track,$
			   Draw=draw)
   Widget_Control,wEqAnimateBase,/Realize
   ;Widget_Control,wEqAnimate+26,Sensitive=0	; disable Write MPEG button
ENDIF ELSE BEGIN
   CW_Animate_Init,wEqAnimate,xs,ys,N_Elements(timeindex)
ENDELSE

Forward_Function position			; ga_plot_new (/link/idl)
!P.Position = position(!X.Range,!Y.Range)	; keep aspect ratio, use vsize

FOR j=0, N_Elements(timeindex)-1 DO BEGIN

   i = timeindex[j]				; convert to the real index

      ; Catch and skip errors

   Catch, error_status 
   IF error_status NE 0 THEN BEGIN
      print,!Err_String,j,i
      error_status = 0
      goto, skip
   ENDIF

      ; Limiter

   IF limitr[0] GT 0 THEN Plot,lim[0,0:limitr[0]-1],lim[1,0:limitr[0]-1]

      ; PSI contours

   IF (ssibry[i] EQ 0 AND ssimag[i] EQ 0) THEN BEGIN
      iz0 = Where(z EQ 0)
      psi = Min(psirz[*,iz0,i]) + (Max(psirz[*,iz0,i]) - Min(psirz[*,iz0,i])) $
		* Findgen(epref.npsi+1)/(epref.npsi+1)
   ENDIF ELSE $
      psi = ssimag[i] + ( ssibry[i] - ssimag[i] ) $
		* Findgen(epref.npsi+1)/(epref.npsi+1)

   Contour,psirz[*,*,i],r,z,Levels=psi,C_LineStyle=2,/NoErase

   coordtop = Convert_Coord(0.6,0.95,/Normal,/To_Data)
   coordbot = Convert_Coord(0.6,0.05,/Normal,/To_Data)
   xyoutS,coordtop[0],coordtop[1],StrCompress(shot)+' '+StrMid(tree,4,3);,color=color_index('Red')
   xyouts,coordbot[0],coordbot[1],StrCompress(time[i])+'ms';,color=color_index('Red')
   ;xyouts,1.8,1.4,StrCompress(shot)+' '+StrMid(tree,4,2)
   ;xyouts,2,-1.5,StrCompress(time[i]),color=color_index('Red')

      ; Boundary and flux in scrape off layer

   IF nbdry[i] GT 0 THEN BEGIN
      OPlot,bdry[0,0:nbdry[i]-1,i],bdry[1,0:nbdry[i]-1,i],thick=2

      rout = Max(bdry[0,0:nbdry[i]-1,i],ivalue)
      zout = bdry[1,ivalue,i]
      rout = rout+epref.deltascrape+Findgen(epref.nscrape)*epref.deltascrape
      zout = zout + Fltarr(epref.nscrape)
      rall = rgrid1[0] + xdim[0]*Findgen(mw[0])/Float(mw[0]-1)
      zall = zmid[0]-zdim[0]/2. + zdim[0]*Findgen(mh[0])/Float(mh[0]-1)
      psiout = bispline(psirz[*,*,i],rall,zall,rout,zout)

      Contour,psirz[*,*,i],r,z,Levels=psiout,/NoErase
   ENDIF

      ; Magnetic axis, X points and straight-lined sparatrix
 
   IF r0[i] NE 0 THEN oplot,[r0[i],r0[i]],[z0[i],z0[i]],Psym=1,SymSize=3

   IF rxpt1[i] GT 1 AND rxpt1[i] LT 2 AND $
      zvsin[i] LT 0 AND zvsout[i] LT 0 THEN BEGIN
      oplot,[rxpt1[i],rvsout[i]],[zxpt1[i],zvsout[i]]  
      oplot,[rxpt1[i], rvsin[i]],[zxpt1[i], zvsin[i]]
   ENDIF ELSE IF rxpt2[i] GT 1 AND rxpt2[i] LT 2 AND $
                 zvsin[i] GT 0 AND zvsout[i] GT 0 THEN BEGIN
      oplot,[rxpt2[i],rvsout[i]],[zxpt2[i],zvsout[i]]   
      oplot,[rxpt2[i], rvsin[i]],[zxpt2[i], zvsin[i]]   
   ENDIF

   CW_Animate_Load,wEqAnimate,Frame=j,Window=!D.Window
skip:
END ; for
winid = !D.Window
Catch,/Cancel

;CW_Animate_GetP,wEqAnimate,pixmap_vect,/Kill_Anyway

CW_Animate_Run,wEqAnimate,50

  ; Restore graphics variables

!P = oldP
!X = oldX
!Y = oldY

Return, wEqAnimate
END

;==============================================================================
PRO Eq_Animate_Event,ev
;==============================================================================
Forward_Function getenv_

IF getenv_('DEBUG') EQ '' THEN Catch, error_status ELSE error_status=0
IF error_status NE 0 THEN BEGIN
   IF N_Elements(state) THEN Widget_Control,ev.top,Set_UValue=state,/No_Copy
   ok = Dialog_Message(!Err_String,Dialog_Parent=ev.id)
   Return
ENDIF

Widget_Control,ev.top,Get_UValue=state,/No_Copy
Widget_Control,ev.id,Get_UValue=uvalue
Case StrCompress(uvalue[0]) OF 
   'CREATE':BEGIN
	Widget_Control,state.wFilter,Get_Value=goodfit
	IF Widget_Info(state.wShot,/Valid) THEN $
	   Widget_Control,state.wShot,Get_Value=shot ELSE shot = state.shot
	IF Widget_Info(state.wRunID,/Valid) THEN $
	   Widget_Control,state.wRunID,Get_Value=runid ELSE runid = state.runid
	IF Widget_Info(state.wShotLabel,/Valid) THEN $
	  Widget_Control,state.wShotLabel,Set_Value=StrCompress(shot)+' '+runid
        Widget_control,state.wTmin,Get_Value=tmin
        Widget_control,state.wTmax,Get_Value=tmax
        Widget_control,state.wStep,Get_Value=step
        result = Eq_Animate_Create(ev.top,shot,runid[0],*state.ptr_epref,$
		tmin=tmin,tmax=tmax,step=step,goodfit=goodfit[0],$
                wEqAnimate=state.wAnimate,winid=state.winid,$
		zoom=state.zoom,xrange=state.xrange,yrange=state.yrange)
	END
   'NONE':
    ELSE:BEGIN
	Obj_Destroy,state.oEvent
	IF Widget_Info(state.wKillBase,/Valid) THEN Widget_Control,state.wKillBase,/Destroy
	Widget_Control,ev.top,/Destroy
	END
ENDCASE

IF Widget_Info(ev.top,/Valid) THEN $
Widget_Control,ev.top,Set_UValue=state,/No_Copy
END; eq_animate_event

;==============================================================================
FUNCTION Eq_Animate_Timer_Event,ev
;==============================================================================
Forward_Function getenv_

IF getenv_('DEBUG') EQ '' THEN Catch, error_status ELSE error_status=0
IF error_status NE 0 THEN BEGIN
   IF N_Elements(state) THEN Widget_Control,ev.top,Set_UValue=state,/No_Copy
   ok = Dialog_Message(!Err_String,Dialog_Parent=ev.id)
   Return,0
ENDIF

wBase = widget_info(ev.id,/parent)
Widget_Control,wBase,Get_UValue=state,/No_Copy
oldshot = state.shot
runid = state.runid
Widget_Control,wBase,Set_UValue=state,/No_Copy

shot = mdsvalue('CURRENT_SHOT("D3D")')
IF shot ne oldshot or runid eq 'efitrt1' THEN BEGIN
   mdsopen,'efit01',shot,status=status,/quiet
   IF status THEN runid = 'efit01' $
   ELSE IF shot ne oldshot THEN BEGIN
	mdsopen,'efitrt1',shot,status=status,/quiet
	runid = 'efitrt1'
   ENDIF
   IF status THEN Widget_Control,wBase,Set_Value={shot:shot,runid:runid,now:1}
ENDIF

Widget_Control,ev.id,Timer=20
return,0
END; eq_animte_timer_event

;==============================================================================
PRO Eq_Animate_SetV,tbl,value
;==============================================================================
; Set_Value
; The value is of struct {shot:shot,runid:runid,epref:epref,now:1}

Forward_Function struct_hastag

On_Error,2

Widget_Control,tbl,Get_UValue=state,/No_Copy
IF struct_hastag(value,'SHOT') THEN state.shot = value.shot
IF struct_hastag(value,'RUNID') THEN state.runid = value.runid
IF struct_hastag(value,'EPREF') THEN *state.ptr_epref = value.epref
IF struct_hastag(value,'DRAW') THEN $
IF Widget_Info(value.draw,/Valid) THEN BEGIN
   Widget_Control,value.draw,Get_Value=winid
   state.winid = winid
ENDIF
IF struct_hastag(value,'ZOOM') THEN state.zoom = value.zoom
IF struct_hastag(value,'XRANGE') THEN state.xrange = value.xrange
IF struct_hastag(value,'YRANGE') THEN state.yrange = value.yrange

IF struct_hastag(value,'NOW') THEN BEGIN		; create the animation
   IF Widget_Info(state.wShotLabel,/Valid) THEN $
      Widget_Control,state.wShotLabel,$
	Set_Value=StrCompress(state.shot)+' '+state.runid

   Widget_Control,state.wFilter,Get_Value=goodfit
   Widget_control,state.wTmin,Get_Value=tmin
   Widget_control,state.wTmax,Get_Value=tmax
   Widget_control,state.wStep,Get_Value=step
        
   result = Eq_Animate_Create(tlb,state.shot,state.runid,$
	*state.ptr_epref,tmin=tmin,tmax=tmax,step=step,goodfit=goodfit[0],$
	wEqAnimate=state.wAnimate,winid=state.winid,$
	zoom=state.zoom,xrange=state.xrange,yrange=state.yrange)
   IF result NE -1 THEN BEGIN
      IF Widget_Info(state.wShot,/Valid) THEN $
      	Widget_Control,state.wShot,Set_Value=state.shot
      IF Widget_Info(state.wRunID,/Valid) THEN $
      	Widget_Control,state.wRunID,Set_Value=state.runid
   ENDIF
ENDIF

Widget_Control,tbl,Set_UValue=state,/No_Copy

END

;==============================================================================
PRO Eq_Animate_AutoUpdate,wBase
;==============================================================================
n = 3
oEventHandler = ObjArr(n)
oEventHandler[0] = Obj_New('EFIT_MDSEvent_Handler','EFITRT1_LOADED')
oEventHandler[1] = Obj_New('EFIT_MDSEvent_Handler','EFIT01_LOADED')
oEventHandler[2] = Obj_New('EFIT_MDSEvent_Handler','NEW_SHOT')

Widget_Control,wBase,Get_UValue=state,/No_Copy
state.oEvent = Obj_New('MDSEvent','atlas.gat.com')
;state.oEvent = Obj_New('MDSEvent','NOSERVER',/noserver)
FOR i=0,n-1 DO BEGIN
   state.oEvent->RegisterHandler,oEventHandler[i] ; does handler->SetWidgetID,why???
   oEventHandler[i]->SetWidgetId, wBase	    ; last to assure the correct wID
ENDFOR
state.oEvent->Activate,1
Widget_Control,wBase,Set_UValue=state,/No_Copy
END

;==============================================================================
Function Eq_Animate,shot,parent,runid=runid,epref=epref,group=group,$
	            standalone=standalone,tearoff=tearoff,$
		    autoupdate=autoupdate,wkillbase=wkillbase,_Extra=extra
;==============================================================================
Forward_Function color_index,edit_entry		; general - color_setup 
Forward_Function mdsplus_setup,mdsvalue		; data (/link/idl)
Forward_Function default_eq_pref		; efitview - eq_plot
Forward_Function bispline			; $VERSION4D/EFITLIB/kupfer/idl_math/
Forward_Function getenv_
Forward_Function struct_hastag

IF getenv_('DEBUG') EQ '' THEN Catch, error_status ELSE error_status=0
IF error_status NE 0 THEN BEGIN
   Message,!Err_String,/Information
   Return,-1
ENDIF

  ; Check inputs and keywords

IF N_Elements(parent) EQ 0 THEN status=mdsplus_setup()

IF N_Elements(shot) EQ 0 THEN shot = 0
IF shot EQ 0 THEN BEGIN
   shot = mdsvalue('CURRENT_SHOT("D3D")')
   shot = edit_entry(Title='shot',Label='shot',/Long,Value=shot)
ENDIF
IF N_Elements(epref) EQ 0 THEN epref = default_eq_pref()
IF N_Elements(runid) EQ 0 THEN runid = getenv('RUNID')	; runid=''
IF runid EQ '' THEN runid='EFIT01' 

  ; Create the top-level widget

IF Keyword_Set(parent) THEN $
   wEqAnimateBase = Widget_Base(parent,/Column,Title='Equilibrium Animation',$
	Group_Leader=group,_Extra=extra) ELSE $
   wEqAnimateBase = Widget_Base(/Column,Title='Equilibrium Animation',$
	Group_Leader=group,_Extra=extra)

wProcessBase = Widget_Base(wEqAnimateBase,/Column,/Frame)

IF Keyword_Set(standalone) THEN BEGIN
   wProcessBase1 = Widget_Base(wProcessBase,/Row)
   wProcessBase2 = Widget_Base(wProcessBase,/Row)
   wCreateButton = Widget_Button(wProcessBase1,Value='Generate Animation',$
	UValue='CREATE')
   wFilterButton = CW_BGroup(wProcessBase1,['converged'],$
	/NonExclusive,Set_Value=[1],UValue='NONE')
   wShotField = CW_Field(wProcessBase1,/Long,Title='shot',Value=shot,XSize=6,$
	/Return_Event,UValue='CREATE')
   wRunIDField = CW_Field(wProcessBase1,Title='id',Value=runid,XSize=7,$
	/Return_Event,UValue='CREATE')
   wShotLabel = 0L
   wTminField = CW_Field(wProcessBase2,/Float,Title='min time',Value='',XSi=6,$
	/Return_Event,UValue='CREATE')
   wTmaxField = CW_Field(wProcessBase2,/Float,Title='max time',Value='',XSi=6,$
	/Return_Event,UValue='CREATE')
   wStepField = CW_Field(wProcessBase2,/Long,Title='step (ms)',Value='100',XSize=6,$
	/Return_Event,UValue='CREATE')

ENDIF ELSE BEGIN
   wCreateButton = Widget_Button(wProcessBase,Value='Generate Animation',$
	UValue='CREATE')
   IF Keyword_Set(tearoff) THEN BEGIN
      wProcessBase0 = Widget_Base(wProcessBase,/Row)
      wShotField = CW_Field(wProcessBase0,/Long,Title='shot',Value=shot,$
	XSize=6,/Return_Event,UValue='CREATE')
      wRunIDField = CW_Field(wProcessBase0,Title='id',Value=runid,XSize=7,$
	/Return_Event,UValue='CREATE')
   ENDIF ELSE BEGIN
      wShotField = 0L
      wRunIDField= 0L
   ENDELSE
   wProcessBase1 = Widget_Base(wProcessBase,/Row)
   wProcessBase2 = Widget_Base(wProcessBase,/Row)
   wShotLabel = 0L
;  wShotLabel = Widget_Label(wProcessBase1,Value=StrCompress(shot)+' '+runid,$
;		/Dynamic_Resize)
   wTminField = CW_Field(wProcessBase1,/Float,Title='Tmin',Value='',XSize=5,$
	/Return_Event,UValue='CREATE')
   wTmaxField = CW_Field(wProcessBase1,/Float,Title='Tmax',Value='',XSize=5,$
	/Return_Event,UValue='CREATE')
   IF struct_hastag(extra,'step') THEN vstep=strcompress(extra.step,/re) ELSE vstep='100'
   wStepField = CW_Field(wProcessBase2,/Long,Title='step',Value=vstep,XSize=5,$
	/Return_Event,UValue='CREATE')
   wFilterButton = CW_BGroup(wProcessBase2,['converged'],$
	/NonExclusive,Set_Value=[1],UValue='NONE')
ENDELSE

wEqAnimate = Eq_Animate_Create(wEqAnimateBase,shot,runid,epref,step=100,/good,$
		winid=winid,_Extra=extra)
IF wEqAnimate LE 0 THEN Return, 0

; Use timer for autoupdate - temporary solution
IF Keyword_Set(autoupdate) THEN BEGIN
   wTimerBase = Widget_Base(wEqAnimateBase,Event_Func='eq_animate_timer_event')
   Widget_Control,wTimerBase,Timer=10
ENDIF
Widget_Control,wEqAnimateBase,Iconify=Long(getenv('ICONIFY'))

IF struct_hastag(extra,'zoom') THEN zoom = extra.zoom ELSE zoom = 0
IF struct_hastag(extra,'xrange') THEN xrange = extra.xrange ELSE xrange=[0.,0.]
IF struct_hastag(extra,'yrange') THEN yrange = extra.yrange ELSE yrange=[0.,0.]
 
IF N_Elements(wKillBase) EQ 0 THEN wKillBase = -1L
Widget_Control,wEqAnimateBase,Set_UValue = { $
	shot:shot,$
	runid:runid,$
	ptr_epref:Ptr_New(epref),$
	wAnimate:wEqAnimate,$
	wFilter:wFilterButton,$
	wShot:wShotField,$
	wRunID:wRunIDField,$
	wShotLabel:wShotLabel,$
	wTmin:wTminField,wTmax:wTmaxField,wStep:wStepField,$
	wKillBase:wKillBase,$
	zoom:zoom,xrange:xrange,yrange:yrange,$
	winid:winid,$
	oEvent:Obj_New() $
	}
Widget_Control,wEqAnimateBase,Pro_Set_Value='Eq_Animate_SetV'

XManager,'Eq_Animate',wEqAnimateBase,Event_Handler='Eq_Animate_Event',/No_Block

IF Keyword_Set(autoupdate) THEN eq_animate_autoupdate,wEqAnimateBase

Return, wEqAnimateBase
END

;==============================================================================
PRO Eq_Animate_TearOff_Event,ev
;==============================================================================
Help,ev,/st
END

;==============================================================================
PRO Eq_Animate_CommandLine
;==============================================================================
; command line arguments
;
; -sh or -shot 123456
; -run or -runid efit01
; -fg red		foreground color
; -bg black		background color
; -th 2			line thickness
; -ch 2			charsize
; -chth or -charthick 2	charthick
; -x 300 -y 100		x,y offset from upper left corner, in pixel
; -w 360 -h 640		width, heigh
; -icon or -iconify	iconify control window
; -auto or -autoupdate	auto update to the current shot

cl = StrCompress(getenv_('EQ_ANIMATE_COMMANDLINE'))
args = Str_Sep(StrLowCase(cl),' ')
nargs = N_Elements(args)

i = (Where((args EQ '-shot' or args EQ '-sh'),n))[0]	; shot number
IF (n GT 0) AND ((i+1) LT nargs) THEN setenv,'SHOT='+args[i+1]

i = (Where((args EQ '-runid' or args EQ '-run'),n))[0]	; runid e.g. efit01
IF (n GT 0) AND ((i+1) LT nargs) THEN setenv,'RUNID='+args[i+1]

i = (Where(args EQ '-x',n))[0]	; xoffset
IF (n GT 0) AND ((i+1) LT nargs) THEN setenv,'XOFFSET='+argS[i+1]

i = (Where(args EQ '-y',n))[0]	; yoffset
IF (n GT 0) AND ((i+1) LT nargs) THEN setenv,'YOFFSET='+args[i+1]

i = (Where(args EQ '-w',n))[0]	; xsize
IF (n GT 0) AND ((i+1) LT nargs) THEN setenv,'XSIZE='+args[i+1]

i = (Where(args EQ '-h',n))[0]	; ysize
IF (n GT 0) AND ((i+1) LT nargs) THEN setenv,'YSIZE='+args[i+1]

i = (Where(args EQ '-fg',n))[0]	; foreground
IF (n GT 0) AND ((i+1) LT nargs) THEN BEGIN
   IF args[i+1] EQ 'black' THEN setenv,'FG='+'foreground' $
   ELSE IF args[i+1] EQ 'white' THEN setenv,'FG='+'background' $
   ELSE IF (where(StrLowCase(color_list()) EQ args[i+1]))[0] NE -1 THEN setenv,'FG='+args[i+1]
print,(where(StrLowCase(color_list()) EQ args[i+1]))
ENDIF

i = (Where(args EQ '-bg',n))[0]	; background
IF (n GT 0) AND ((i+1) LT nargs) THEN BEGIN
   IF args[i+1] EQ 'black' THEN setenv,'BG='+'foreground' $
   ELSE IF args[i+1] EQ 'white' THEN setenv,'BG='+'background' $
   ELSE IF (where(StrLowCase(color_list()) EQ args[i+1]))[0] NE -1 THEN setenv,'BG='+args[i+1]
ENDIF

i = (Where(args EQ '-th',n))[0]	; line thinkness
IF (n GT 0) AND ((i+1) LT nargs) THEN setenv,'THICK='+args[i+1]

i = (Where(args EQ '-ch',n))[0]	; charsize
IF (n GT 0) AND ((i+1) LT nargs) THEN setenv,'CHARSIZE='+args[i+1]

i = (Where((args EQ '-chth' or args EQ '-charthick'),n))[0]; charthick
IF (n GT 0) AND ((i+1) LT nargs) THEN setenv,'CHARTHICK='+args[i+1]

i = Where((args EQ '-icon' or args EQ '-iconify'),n) ; iconify control widget
IF (n GT 0) THEN setenv,'ICONIFY=1'

i = Where((args EQ '-auto' or args EQ '-autoupdate'),n) ; auto update
IF (n GT 0) THEN setenv,'AUTOUPDATE=1'

END

;==============================================================================
PRO Eq_Animate,tearoff=tearoff,xsize=xsize,ysize=ysize,_Extra=extra
;==============================================================================
Forward_Function getenv_
; check command line argument
IF getenv_('EQ_ANIMATE_COMMANDLINE') NE '' THEN Eq_Animate_Commandline
color_setup,/reverse
IF NOT Keyword_Set(tearoff) THEN $
   wid = Eq_Animate(Long(getenv('SHOT')),/Standalone,_Extra=extra) $
ELSE BEGIN
 ; tearoff mode is standalone but with a separate draw window
   IF getenv('XSIZE') ne '' THEN xsize = Long(getenv('XSIZE'))
   IF getenv('YSIZE') ne '' THEN ysize = Long(getenv('YSIZE'))
   IF N_Elements(xsize) EQ 0 THEN xsize = 360
   IF N_Elements(ysize) EQ 0 THEN ysize = 640
   wDrawBase = Widget_Base(Title='EFIT Equilibrium Animation',$
	XOffset=Long(getenv('XOFFSET')),YOffset=Long(getenv('YOFFSET')))
   wDraw = Widget_Draw(wDrawBase,XSize=xsize,YSize=ysize,Retain=2)
   Widget_Control,wDrawBase,/Realize
   wAnimate = Eq_Animate(Long(getenv('SHOT')),step=25,draw=wDraw,$
	group=wDrawBase,$	;/Floating,$
	/Tearoff,wKillBase=wDrawBase,_Extra=extra,$
	autoupdate=Long(getenv('AUTOUPDATE')),$
	XOffset=Long(getenv('XOFFSET'))+xsize,YOffset=Long(getenv('YOFFSET')))
   XManager,'eq_animate_tearoff',wDrawBase,Event_Handler='eq_animate_tearoff_event',/No_Block
ENDELSE
END

