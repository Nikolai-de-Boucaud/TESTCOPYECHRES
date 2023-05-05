;==============================================================================
PRO wrap_eq_plot,prefstruct,prefwid,viewer
;==============================================================================
;       04-06-98
;	05-19-98 Q. Peng replace getenv with CMG's getenv_ for MacOS.
;	09-21-98 Q. Peng enlarge the plotting area so that f-coils are not
;			 cut off.
; wrapper to eq_plot, called by cw_struct when return is pressed
; the parameters are required by cw_struct
; prefstruct	preference structure
; prefwid	widget ID of the preference window
; viewer	widget ID of the cw_struct's caller which carries the info
;		structure that is needed.  In this case, the viewer.

  if getenv_('DEBUG') eq '' then catch, error_status else error_status=0
  if error_status ne 0 then begin
     if n_elements(state) then $
	Widget_Control,viewer,Set_UValue=state,/No_Copy
     ok = Dialog_Message(!Err_String,Dialog_Parent=ev.id)
     return
  endif
 
  Widget_Control,viewer,Get_UValue=state,/No_Copy
  state = state.efit->Equilibrium(Info=state)
;  all_plots,prefwid,state,plot=0
  Widget_Control,viewer,Set_UValue=state,/No_Copy

END ; wrap_eq_plot

;==============================================================================
FUNCTION default_eq_pref
;==============================================================================
; 04-09-98
; Returns the default preference for the equilibrium
; 06-DEC-2005 change default contour number from 6 to 9

  return, {EPREF,$
	npsi: fix(9),$			; number of psi contour
	nscrape: fix(4),$		; number of outer scrapes
	deltascrape: float(.01),$	; distance between scrapes in m
	contour: 0,$			; contour of ['psi','rho','rho_n']
	;deltarho: float(.2),$		; distance between rho contour
	separatrix: 1 $			; straight-line(0) or contoured(1)
	}				; 	separatrix
END

;==============================================================================
PRO eq_plot,ptr_a,ptr_g,ptr_time=ptr_time,wid=wid,oplot=oplot,prefwid=prefwid,$
	    ptr_pref=ptr_pref,machine=machine
;==============================================================================
;Message,'',/Informational
; 03-02-98
; 04-06-98 added contoured separatrix by Tony Leonard. 
;	   changed units from cm to m.
; 04-24-98 not overwrite shot,time labels in multi-slice overlay
; 05-12-98 set color of limiter to the foreground
; 09-08-98 added time keyword to keep track of sub-millisecond.
; 12-01-98 do not plot contours if the source of G structs is MFITD.
; 12-03-98 fixed a bug for an error that happened before oldP got assigned.
; 08-13-99 plot inner flux for MFIT with nobound per Schaffer's request.
; 10-26-99 plot contours and separatrix for MFIT if there is any per Lao.
;	   do NOT plot magnetic axis if not available (a.d.rm = 0).
; 02-11-04 To accommodate ITER efit, use bdry along with lim to decide range, 
;	   reverse psi if in decending order for contour.
; 06NOV02 added option "mod Bp" to plot poloidal field (per Jackson's request)
; 06NOV28 added annotations for mod Bp contours (Jackson)
;         show values in Gauss intead of T.
; 20090311 SMF - Added case statement for machine, since limiter and fcoil
;                ranges are very different between D3D and EAST.  Default is
;                still 0.33 (D3D setting).
;
; time	The time in milli-second. The true time if sub-millisecond. Both
;	A and G are truncated to millisecond.
; wid	widget id whose uvalue is used to store system variables for 
;	data coordinates
; prefwid 	cw_struct widget id for the equilibrium preference
;		they are always read in here
; ptr_pref	pointer to epref struct, alternative to prefwid
;

Forward_Function eqauthor

  ; Set the default machine to D3D, and translate DIII-D into D3D
  if not(keyword_set(machine)) then machine='D3D'
  if ( (machine eq 'DIII-D') or (machine eq 'DIII-Ds')) then machine='D3D'
  
  if getenv('DEBUG') eq '' then catch, error_status else error_status=0
  if error_status ne 0 then begin
     print,!Err_String
     goto, done				; restore
  endif

; Number of shot overlays.  Note that the first in the array is the lastest.
  if not Keyword_Set(oplot) then nplots=1 else nplots=N_Elements(ptr_g)

  oldP = !p
  oldX = !x
  oldY = !y

; Return if g struct has error.
  gerror = 0
  if not Ptr_Valid(ptr_g[0]) then gerror = 1 else $
  if (*ptr_g[0]).error ne 0 then gerror = 1
  if gerror then begin
     plot,[0,0],[0,0],background=color_index('Background'),xstyle=5,ystyle=5,$
	position=position([0,1],[0,1])
     xyouts,0.2,0.7,color=color_index('Red'),$
	'EFIT G file or MDSplus !Cequivalent is NOT ' + $
	'!Cavailable for the !Cequilibrium.'
     return
  endif

; retrive the graphic variables from the widget's uvalue 
  if KEYWORD_SET(wid) then begin
     WIDGET_CONTROL,wid,GET_UVALUE=uvalue,/No_Copy
     if uvalue.zoom ne 0 then begin
	!x.range = uvalue.!x.range
	!y.range = uvalue.!y.range
     endif
  endif

; set up backgroud color
  !p.background = color_index('Background')

  !x.style=5 & !y.style=5         ; force the range, no axes
; !x.style=1 & !y.style=1         ; force the range

; find ranges from the limiter dimension and force the ranges
; in zoom mode, use the ranges stored in uvalue of wid
; Increase inc from 0.3 to 0.33 so that the f-coils are not cut off.
  if uvalue.zoom eq 0 and (*ptr_g[0]).limitr gt 0 then begin
     case machine of
        'D3D':   begin
                 nxinc = 0.33
                 pxinc = 0.33 
                 nyinc = 0.33
                 pyinc = 0.33
                 end
        'EAST':  begin
                 nxinc = 0.775
                 pxinc = 0.95
                 nyinc = 0.90
                 pyinc = 0.90
                 end
        'KSTAR': begin
                 nxinc = 0.85
                 pxinc = 1.05
                 nyinc = 0.90
                 pyinc = 0.95
                 end
     else: begin
           nxinc = 0.33
           pxinc = 0.33
           nyinc = 0.33
           pyinc = 0.33
           end
     endcase
     if struct_hastag(*ptr_g[0],'bdry') then begin  
       if (*ptr_g[0]).nbdry gt 0 then begin
     	 gbdry_r = (*ptr_g[0]).bdry[0,*]       
     	 gbdry_r = gbdry_r[where(gbdry_r)]
     	 gbdry_z = (*ptr_g[0]).bdry[1,*]       
     	 gbdry_z = gbdry_z[where(gbdry_z)]
       	 if gbdry_r[0] ne -1 then begin	
            br = [min(gbdry_r),max(gbdry_r)]
            bz = [min(gbdry_z),max(gbdrY_z)]
	 endif
       endif
     endif     
     if n_elements(br) ne 0 then begin
     	!x.range = $
           [min([br[0],min((*ptr_g[0]).lim(0,0:(*ptr_g[0]).limitr-1))])-nxinc,$
            max([br[1],max((*ptr_g[0]).lim(0,0:(*ptr_g[0]).limitr-1))])+pxinc] 
        !y.range = $
	   [min([bz[0],min((*ptr_g[0]).lim(1,0:(*ptr_g[0]).limitr-1))])-nyinc,$
            max([bz[1],max((*ptr_g[0]).lim(1,0:(*ptr_g[0]).limitr-1))])+pyinc] 
     endif else begin
     	!x.range = [min((*ptr_g[0]).lim(0,0:(*ptr_g[0]).limitr-1))-nxinc,$
                    max((*ptr_g[0]).lim(0,0:(*ptr_g[0]).limitr-1))+pxinc]
        !y.range = [min((*ptr_g[0]).lim(1,0:(*ptr_g[0]).limitr-1))-nyinc,$
                    max((*ptr_g[0]).lim(1,0:(*ptr_g[0]).limitr-1))+pyinc]
     endelse
  endif

; keep aspect ratio accordingly
  !p.position = position(!x.range,!y.range)

; Get the preference from the widget or the pointer, 
; If nothing is available, set the default preference.
  epref = default_eq_pref()
  if not Keyword_Set(prefwid) then prefwid=long(0)
  if Widget_Info(prefwid,/Valid) then $
     Widget_Control,prefwid,Get_Value=epref $
  else if Keyword_Set(ptr_pref) then if N_Elements(*ptr_pref) ne 0 then $
     epref = *ptr_pref

; Check values in the epref
  if epref.nscrape lt 0 then epref.nscrape=0
  if epref.nscrape gt 60 then epref.nscrape=60	; maximum number of contours
  if epref.deltascrape lt 0 then epref.deltascrape=0.0
  if epref.npsi lt 0 then epref.npsi=0
  if epref.npsi gt 58 and epref.contour ne 2 then epref.npsi=58	; leave 2 for center and bndry, contour 2 = 'mod Bp'
  ;if epref.deltarho lt 0 then epref.deltarho=0.0
  if Widget_Info(prefwid,/Valid) then Widget_Control,prefwid,Set_Value=epref

; Put plots on.  Start from the oldest one to warrent the same colors.

for iplot = nplots-1, 0, -1 do begin

  a = *ptr_a[iplot]
  g = *ptr_g[iplot]
  IF N_Elements(ptr_time) NE 0  THEN time = (*ptr_time)[iplot] $
				ELSE time = g.time

; Deceide color and whether plot or oplot
  !p.color = sync_color(nplots-iplot)
  color0 = color_index('Foreground')	; color for limiter
  color  = !p.color

  IF g.limitr GT 0 THEN BEGIN
  if iplot eq nplots-1 then begin
   ; The last and oldest plot
     plot, g.lim(0,0:g.limitr-1),g.lim(1,0:g.limitr-1),color=color0
  endif else begin
   ; The overlay plots.  If error, skip.
     if g.error ne 0 then goto, skip		
     oplot, g.lim(0,0:g.limitr-1),g.lim(1,0:g.limitr-1),color=color0
  end
  ENDIF ELSE plot,[0,0],[0,0],color=color0

  if g.nbdry gt 0 then $
  oplot,g.bdry(0,0:g.nbdry-1),g.bdry(1,0:g.nbdry-1),color=color,thick=2

;  curve = Obj_New('GA_PLOT',transpose(g.lim(0,0:g.limitr-1)),$
;			    transpose(g.lim(1,0:g.limitr-1)),Aspect=1)
;  curve->Add_Data,transpose(g.bdry(0,0:g.nbdry-1)),$
;		  transpose(g.bdry(1,0:g.nbdry-1)),color=0

  r=g.rgrid1 + findgen(g.mw)/(g.mw-1.) * g.xdim
  z=g.zmid + findgen(g.mh)/(g.mh-1.0) * g.zdim - g.zdim/2.
  cfpl=g.psirz(0:g.mw-1,0:g.mh-1)
  IF g.limitr GT 1 THEN BEGIN	; 0 -> 1: kludge for ITER efit that has 1 lim
  rves_min = .95  * min(g.lim(0,where(g.lim(0,*) gt 0.)))
  cfpl = temporary(cfpl( where(r ge rves_min), *))
  r = temporary(r(where(r ge rves_min)))
  ENDIF

; Put on shot and time stamp
; outcoord = convert_coord(0.1,intt * .08,/normal,/to_data)
  outcoord = convert_coord(0.1,(nplots-iplot) * 0.02,/normal,/to_data)

;  if strtrim(!D.(0)) ne 'PS' then begin
     ; correct slice stamp for sub-millisecond times
     ; add tag if file is in format xnnnnnn.nnnnn_zzz
     subs = str_sep(g.source,'/')
     subs = str_sep(subs[n_elements(subs)-1],'_')
     if n_elements(subs) gt 1 then substr = '_'+subs[n_elements(subs)-1] else substr=''
     xyouts,outcoord(0),outcoord(1),color=color,$
	;strtrim( g.shot,2) + ' ' + strtrim(time,2) + substr,charsize=0.75
	strtrim(g.source,2),charsize=0.75
;  endif

; Do not plot inner contours or separatrix if G structs are from MFIT,
; unless there is no boundary. 8-13-99 per Schaffer
; Try to plot inner contours or separatrix even for MFIT. 10-26-99 per Lao.

  IF eqauthor(g) EQ 'MFITD' THEN BEGIN
     xyouts,outcoord(0)+0.7,outcoord(1),color=color,'MFITD',charsize=0.75
     ;if (g.ssibry ne 0 or g.ssimag ne 0) then goto, skip
  ENDIF

  contours = ['psi','rho','mod Bp','rho_n']
  econtour = contours(epref.contour)
  contour_label = 0  ; flag to annotate contour lines
  case econtour of
    'psi' : begin
      ; Plot contours on psi (default).
        If epref.npsi gt 0 Then Begin
 	   psi =   g.ssimag   +  $	
		  (g.ssibry - g.ssimag) * (findgen(epref.npsi) + 1) $
        	   / (epref.npsi  + 1)
	   if (g.ssibry eq 0 and g.ssimag eq 0) then begin ; no boundary
	      iz0 = where(g.z eq 0)
	      if iz0[0] ne -1 then psi = min(g.psirz[*,iz0]) + $
	         (max(g.psirz[*,iz0]) - min(g.psirz[*,iz0])) $
                  * (findgen(epref.npsi) + 1) / (epref.npsi + 1)	   
	   endif
        EndIf
        end
    'rho' : begin
      ; Plot contours on grid of equal spaced rho.
      ; Use number of contours instead of distance between rho - Q.P.
	if epref.npsi gt 0 then begin
	   pqr = fluxfun(g)
 	   ;rho = findgen(10)* epref.deltarho
	   ;rho = temporary(rho(where(rho lt max(pqr.rho))))
	   rho = (pqr.rho)[indgen(epref.npsi+2) * $
			   N_Elements(pqr.rho)/(epref.npsi+2-1)]
	   psi = spline( pqr.rho,pqr.psi,rho)
	endif
	end
    'rho_n' : begin
      ; Plot contours normalized on RHO
      ; Not used since it is very close to rho.
	if epref.npsi gt 0 then begin
	   pqr = fluxfun(g)
	   rho = (pqr.rho)[0] + $
		 ((pqr.rho)[N_Elements(pqr.rho)-1]-(pqr.rho)[0]) * $
		 (findgen(epref.npsi)+1) / (epref.npsi + 1)
	   psi = spline( pqr.rho,pqr.psi,rho)
	endif
	end
    'mod Bp' : begin  
       ; Plot contours of Bpol
       ; added 06NOV01 per G.Jackson's request
	if epref.npsi gt 0 then begin
  	   r=g.rgrid1 + findgen(g.mw)/(g.mw-1.) * g.xdim
  	   z=g.zmid + findgen(g.mh)/(g.mh-1.0) * g.zdim - g.zdim/2.
	   bpol = bfield_pol(g)
  	   cfpl = bpol.pol
	   psi = min(cfpl)+findgen(epref.npsi)*(max(cfpl)-min(cfpl))/(epref.npsi-1)
  	   contour_label = 1  ; flag to annotate contour lines
	endif
	end
     else:help,econtour
  endcase

;
; PLOT INTERIOR CONTOURS OF EQUAL PSI OR EQUAL RHO
;
 
  If epref.npsi gt 0 Then Begin
     if n_elements(psi) ge 2 then begin
	if psi[1] lt psi[0] then psi = reverse(psi)
     endif
     if contour_label eq 0 then $
     contour, cfpl, r, z, levels=psi,ystyle=1+4, xstyle=1+4, $
	color = color, /noerase, c_linestyle = 2 $
     else $  ; show contour values in Gauss (converted from T)
     contour, cfpl, r, z, levels=psi,ystyle=1+4, xstyle=1+4, $
	color = color, /noerase, c_linestyle = 2, $
	c_annotation=strtrim(string(psi*1e4,format='(f7.1)'),2),c_charsize=0.6
  EndIf Else Begin
;     Print, 'Eqplot: No interior contours selected in Preferences (npsi)'
  EndElse
;
; PLOT CONTOURS OF FLUX IN SCRAPE OFF LAYER
;
  if epref.nscrape gt 0 and epref.deltascrape gt 0 and g.nbdry gt 0 then begin
     rout = max( g.bdry(0,0:g.nbdry-1) , ivalue ) 
     zout = g.bdry(1, ivalue )
     rout = rout + findgen(epref.nscrape+1) * epref.deltascrape
     zout = zout + fltarr(epref.nscrape+1)

     ;IF g.rmaxis NE 0 THEN 
     rhdum = rho_rz(g,rout,zout,psi_pts)

   ; curve->Add_Data,r,z,cfpl,linestyle=2,/contour
     if n_elements(psi_pts) ge 2 then begin
	if psi_pts[1] lt psi_pts[0] then psi_pts = reverse(psi_pts)
     endif
     contour,cfpl,r,z,levels=psi_pts,ystyle=1+4,xstyle=1+4,$
	/noerase,color = color
  endif Else Begin
;     Print, 'Eqplot: No contours for scrape layer selected in'+ $
;	    ' Preferences (nscape)'
  EndElse

  if a.error eq 0 then begin
     if struct_hastag(a.d,'RM') then begin
        rmagx = a.d.rm
        zmagx = a.d.zm
     endif else if struct_hastag(a.d,'R0') then begin
        rmagx = a.d.r0
        zmagx = a.d.z0
     endif else rmagx = 0
     IF rmagx NE 0 THEN $
	oplot,rmagx*[1.,1.],zmagx* [1.,1.],psym = 1,  $
	symsize = 3,color = color
   ; rxpt1 and rxpt2 are in MKS unit m as oppose to cm
     if struct_hastag(a.d,'RXPT1') then begin ; possible for CMOD
     if a.d.rxpt1 gt 1. and a.d.rxpt1 lt $
        2. and a.d.zvsin lt 0. and a.d.zvsout lt  0. then begin
	if epref.separatrix eq 0 then begin	
	 ; Straight-lined sparatrix
    	   oplot,[a.d.rxpt1,a.d.rvsout],[a.d.zxpt1,a.d.zvsout],color=color
    	   oplot,[a.d.rxpt1,a.d.rvsin],[a.d.zxpt1,a.d.zvsin],color=color
	endif else begin
         ; Contoured separatrix as oppose to straight lines
           zxp=a.d.zxpt1+0.1 ; 10cm above Xpt
           rsout=a.d.rvsout
           rsin=a.d.rvsin
           nspln=100
           rspln=rsin+findgen(nspln)/(nspln-1.)*(rsout-rsin)
           zspln=Min(z)+findgen(nspln)/(nspln-1.)*(zxp-Min(z))
           rind=(rsin-r(0)+findgen(nspln)*(rspln(1)-rspln(0)))/(r(1)-r(0))
           zind=findgen(nspln)*(zspln(1)-zspln(0))/(z(1)-z(0))
           temppsi=interpolate(cfpl,rind,zind,/grid,cubic=-0.5)
           psi_pt=[g.ssibry]
           contour,temppsi,rspln,zspln,levels=psi_pt,/noerase,$
		xstyle=1+4,ystyle=1+4,color=color
	endelse
     endif else if a.d.rxpt2 gt 1. and a.d.rxpt2 lt $
        2. and a.d.zvsin gt 0. and a.d.zvsout gt  0. then begin
	if epref.separatrix eq 0 then begin	
	 ; Straight-lined sparatrix
	   oplot,[a.d.rxpt2,a.d.rvsout],[a.d.zxpt2,a.d.zvsout],color=color
    	   oplot,[a.d.rxpt2,a.d.rvsin],[a.d.zxpt2,a.d.zvsin],color=color
	endif else begin
         ; Contoured separatrix as oppose to straight lines
           zxp=a.d.zxpt2-0.1 ; 10cm below upper Xpt
           rsout=a.d.rvsout
           rsin=a.d.rvsin
           nspln=100
           rspln=rsin+findgen(nspln)/(nspln-1.)*(rsout-rsin)
           zspln=zxp+findgen(nspln)/(nspln-1.)*(Max(z)-zxp)
           rind=(rsin-r(0)+findgen(nspln)*(rspln(1)-rspln(0)))/(r(1)-r(0))
           zind=(findgen(nspln)-(nspln-1))*(zspln(1)-zspln(0))/(z(1)-z(0))$
            	+(n_elements(z)-1)
           temppsi=interpolate(cfpl,rind,zind,/grid,cubic=-0.5)
           psi_pt=[g.ssibry]
           contour,temppsi,rspln,zspln,levels=psi_pt,/noerase,$
		xstyle=1+4,ystyle=1+4,color=color
	endelse ; epref.separatrix
     endif
     endif else message,'no rxpt1',/info; rxpt1 tag
  endif ; a.error

skip:
endfor ; iplot

done:
; save the graphic variables in the widget's uvalue 
  if KEYWORD_SET(wid) and n_elements(uvalue) ne 0 then begin
     uvalue.!p = !p
     uvalue.!x = !x
     uvalue.!y = !y
     WIDGET_CONTROL,wid,SET_UVALUE=uvalue,/NO_COPY
  endif

; restore the system graphic variables
  !p = oldP
  !x = oldX
  !y = oldY

; return, curve

END
