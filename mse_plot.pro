; procedure mse_plot.pro      B. Rice 7/15/97
; makes plots of MSE data, for use with msefit.pro routine
;
;   06-02-98   Q.Peng added shot-time label to the plots and reajusted range.
;   01-19-98   Q.Peng use _extra to all ga_plot calls to get the correct
;		      color, etc.
;   02-09-99   Q.Peng added a branch for JET's efit per Rice.  Added a new
;		      environment variable MACHINE (assume DIII-D if unset).  
;		      Use g.bdry for xrange instead of hard-coded DIII-D geom.
;   04-15-99   B.Rice - fixed sign for reversed shear
;   11-30-2000 Q.Peng !y.range become double from float in IDL-5.4. Convert
;                     back to float when used in Add_Data where data types of
;                     xArg and yArg need to be the same.
;   02-01-2001 Q.Peng commented out the lines that filter out bad 315 channels
; 		      so that they show up in the plots (Luce, Makowski)
;   08-23-2002 Q.P. use X1,Y1 instead of X,Y as keywords. In IDL5.5, if _extra
;		    has Yxxx as keyword, keyword Y becomes ambiguous.
;   08-07-2004 Q.P. Do not filter MSE channels, show un-used channels in
;                   unfilled symbols; add Hide keyword to hide them. Channels
;                   can be marked with numbers with ga_plot's marking feature.
;   08-13-2004 Q.P. fixed a bug that crashed when there was no w315.
;   03-15-2005 Q.P. turned half channels 37-40 of 315 system off (Makowski)
;   23FEB2007  Q.P. added 195l and 195u channels (new 210RT beam)
;   08-01-2007 Q.P. added keyword get_channels_only to return on/off info of
;                   all the channels in a structure, and skip plotting.
;   20090311 SMF - Added error bars to some of the profiles2 mse plots.

; INPUTS:
;   b_mse: b field at MSE locations (structure)
;   b_mid: b field at midplane (mag axis) (structure)
;   r_mid: major radius vector that goes with b_mid
;   mse1:  mse data (structure)
;   a:     a0 file data (structure)
;   g:     g0 file data (structure)
;   f:     fluxfun data (structure) (contains rho)
;   er_s:  Er switch, 0=Er included, 1 = Er off
;   y1:    ordinate data: 0 = gamma,  1 = Bz
;   x1:    abscissa data:
;               0 = R
;               1 = rho (sqrt toroidal flux)
;               2 = psi_n (normalized poloidal flux)
;   number:the plot number for the ga_data object


FUNCTION mse_plot,b_mse,b_mid,r_mid,mse1,a,g,f,$
	 Er=er,X1=x1,Y1=y1,$
;	 er_s,y1,x1,$
	 name=name,curve=curve,nodata=nodata,hide=hide,$
	 get_channels_only=get_channels_only,_extra=e

Forward_Function getenv_, struct_hastag

   on_error, 2 ; Return to caller

   machine = getenv_('MACHINE')

   ; Check keywords.

   er = Keyword_Set(er)
   IF N_Elements(x1) EQ 0 THEN x1 = 'r'		; 'r','rho','psi_n'
   IF N_Elements(y1) EQ 0 THEN y1 = 'bz'	; 'gamma','bz'
   x1 = StrLowCase(x1)
   y1 = StrLowCase(y1)

;   if not keyword_set(number) then number=1
   if not keyword_set(name) then name=''
   if not keyword_set(curve) then replace=0 else replace=1
   if not keyword_set(hide) then hide = 0	; show channels off in efit

   ; Check extra structu.

   IF N_Elements(e) NE 0 THEN BEGIN
      e2 = e
      IF (where(Tag_Names(e2) EQ 'YNAME'))[0] GE 0 THEN e2.yname = '' 
   ENDIF

   ; Save the graphic system variables.

   old_P = !P
   old_X = !X
   old_Y = !Y

IF machine EQ 'JET' THEN BEGIN				; JET (efitj)
   w_all = where((abs(mse1.err_gam_deg) lt 1.5) and $
              (abs(mse1.gam_deg) lt 20.) and (mse1.error eq 0))
   w315 = w_all
ENDIF ELSE BEGIN					; DIII-D for now
  ;w315_full = where((mse1.vport eq 315) and (abs(mse1.err_gam_deg) lt 1.5) $
  ;           and (abs(mse1.gam_deg) lt 20.0) and (mse1.error eq 0) and     $
  ;           (mse1.energy eq 'full'))
  ;w315_half = where((mse1.vport eq 315) and (abs(mse1.err_gam_deg) lt 1.5) $
  ;           and (abs(mse1.gam_deg) lt 20.0) and (mse1.error eq 0) and     $
  ;           (mse1.energy eq 'half'))

  ; Turn off the half energy channels 37-40
   IF N_Elements(mse1.vport) GE 40 THEN mse1.vport[36:39] = 0
   w315 = where(mse1.vport eq 315 and mse1.fwtgam gt 0)
   w315_off = where(mse1.vport eq 315 and mse1.fwtgam eq 0)

;  commented out per Luce, Makowski 020201
;   w = where(w315_full LT 15)   
;   IF a.shot LT 94700 THEN w = where(w315_full LT 10)   ;channels 11-16 are bad
;   w315_full = w315_full(w)

  ;w15=where((mse1.vport eq 15) and (abs(mse1.err_gam_deg) lt 1.5) and $
  ;          (abs(mse1.gam_deg) lt 20.) and (mse1.error eq 0))
  ;w45=where((mse1.vport eq 45) and (abs(mse1.err_gam_deg) lt 1.5) and $
  ;          (abs(mse1.gam_deg) lt 20.) and (mse1.error eq 0))
   w15 = where(mse1.vport eq 15 and mse1.fwtgam gt 0)
   w15_off = where(mse1.vport eq 15 and mse1.fwtgam eq 0)
   w45 = where(mse1.vport eq 45 and mse1.fwtgam gt 0)
   w45_off = where(mse1.vport eq 45 and mse1.fwtgam eq 0)

  ;w_all = [w315_full,w315_half,w45,w15]
   w_all = [w315,w315_off,w45,w45_off,w15,w15_off]

   w195l = where(mse1.vport eq 194 and mse1.fwtgam gt 0,c)
   w195l_off = where(mse1.vport eq 194 and mse1.fwtgam eq 0,c)
   w195u = where(mse1.vport eq 196 and mse1.fwtgam gt 0,c)
   w195u_off = where(mse1.vport eq 196 and mse1.fwtgam eq 0,c)

   w_all = [w_all,w195l,w195l_off,w195u,w195u_off]
ENDELSE

IF Keyword_Set(get_channels_only) THEN BEGIN
   channels = {w315:w315,  w315_off:w315_off, $
                w45:w45,    w45_off:w45_off, $
                w15:w15,    w15_off:w15_off, $
              w195l:w195l,w195l_off:w195l_off, $
              w195u:w195u,w195u_off:w195u_off}
   return,channels
ENDIF

   ig = indgen(g.mw)


; E_R, E_Z at mse locations
   er_mse = b_mse.z*0.0
   ez_mse = er_mse
   xyo = ''
   ;if er_s eq 0 then xyo = 'with Er' else xyo = 'w/o Er '
   
   IF (  n_elements(g.epoten) NE 0 ) AND er THEN begin
      x = findgen(g.mw) / ( g.mw -1.0 )
      er_rbpol = (-1.0) * interpol( g.epoten(ig), x, b_mse.psi_n )
      w = where( b_mse.psi_n GT 1 )
      IF ( w[0] NE -1 ) THEN er_rbpol(w) = 0.0
      bpol = sqrt( b_mse.r^2 + b_mse.z^2 )
      erho_mse = er_rbpol * mse1.rrgam * bpol
      ;E_R and E_z at mse points
      er_mse = (-1.0)* erho_mse * b_mse.z / bpol * g.cpasma / abs(g.cpasma)
      ez_mse = erho_mse * b_mse.r / bpol
      if ( a.error eq 0 ) then xyo = 'chisq=' + string(a.d.chigam,'(f5.1)')
   ENDIF

;calculate fields and gamma at MSE chord locations

IF machine EQ 'JET' THEN BEGIN
   b_mid = b_mse		; JET's mse is not on the mid-plane
   IF struct_hastag(mse1,'rrgam') THEN r_mid = mse1.rrgam
   z_mid = mse1.zzgam

   ; copied from /home/brice/idl/msefitj.pro 02-09-99 QP
   ; gam_calc=180/!pi*atan((mbz*a0+mbr*a1+mbt*a2)/(mbz*a3+mbr*a4+mbt*a5))
   ; mse_bz=(a1*mbr+a2*mbt-tgamma*(a4*mbr+a5*mbt)) / $
   ;        (tgamma*a3-a0)				;mse vertical field
   ; A0 A1 A2	-efitj->  A1 A5 A6
   ; A3 A4 A5	-efitj->  A4 A3 A2
   gam_calc=180/!pi*atan((b_mse.z*mse1.a1gam+b_mse.r*mse1.a5gam+$
	    b_mse.phi*mse1.a6gam) / $
	    (b_mse.z*mse1.a4gam+b_mse.r*mse1.a3gam+b_mse.phi*mse1.a2gam))
   mse_bz=(mse1.a5gam*b_mse.r+mse1.a6gam*b_mse.phi-$
	   mse1.tangam*(mse1.a3gam*b_mse.r+mse1.a2gam*b_mse.phi)) / $
	  (mse1.tangam*mse1.a4gam-mse1.a1gam)		;mse vertical field

ENDIF ELSE BEGIN
   z_mid = r_mid * 0.0
   den = b_mse.z * mse1.a4gam + b_mse.r * mse1.a3gam +                      $
	 b_mse.phi * mse1.a2gam + mse1.a6gam * ez_mse
   gam_calc = 180.0 / !pi * atan( ( b_mse.z * mse1.a1gam + mse1.a5gam *     $
              er_mse ) / den )
   mse_bz = ( den * mse1.tangam - mse1.a5gam * er_mse ) / mse1.a1gam
ENDELSE

   xp = 0.0
   yp = 0.0

; Choose abscissa

   CASE 1 OF
      (x1 EQ 'r'):BEGIN           ; R
         xd = mse1.rrgam
         xf = r_mid
         !x.range = [ 1.4, 2.5 ]
         !x.style= 1 
         ; use bdry for xrange instead of hard-coding
	 if struct_hastag(g,'bdry') then begin	
	    gbdry_r = g.bdry[0,*]	
	    gbdry_r = gbdry_r[where(gbdry_r)]
	    if gbdry_r[0] ne -1 then !x.range = [min(gbdry_r),max(gbdry_r)]
	 endif
         !x.title = 'R (m)'

      END

      ((x1 EQ 'rho') OR (x1 EQ 'psi_n')):BEGIN ; rho

         rho_map, mse1.rrgam, mse1.zzgam, g, f, mse_psin, mse_rho
         wr1 = where( mse1.rrgam lt g.rmaxis )
         mse_psin(wr1) = -mse_psin(wr1)
         mse_rho(wr1) = -mse_rho(wr1)
         IF ( x1 EQ 'rho' ) THEN xd = mse_rho ELSE xd = mse_psin
         rho_map, r_mid, z_mid, g, f, rmid_psin, rmid_rho
         wr1 = where( r_mid LT g.rmaxis )
         rmid_psin(wr1) = -rmid_psin(wr1)
         rmid_rho(wr1) = -rmid_rho(wr1)
         IF ( x1 EQ 'rho' ) THEN xf = rmid_rho ELSE xf = rmid_psin
         !x.range = [ -0.4, 1 ]
         !x.style = 1
         ;IF 9 x1 EQ 'rho' 0 THEN !x.title = 'rho' ELSE !x.title = 'psi_n'
         IF x1 EQ 'rho' THEN !x.title = 'rho' ELSE !x.title = 'psi_n'
      END

   ENDCASE

   s_err = 4 * abs(mse1.err_gam_deg)
   indices = where(mse1.siggam eq 100)
   if (indices[0] ne -1) then s_err[where(mse1.siggam eq 100)] = 0

   ;  Make plots

   st = 'shot ' + strtrim(string(g.shot),2)+', time ' +                     $
     strtrim(string(g.time),2)
   ptitle= 'MSE Profile' + st

   CASE 1 OF

      ( y1 EQ 'gamma' ): BEGIN           ;gamma plot
         yd = mse1.gam_deg               ;mse data
         yf = gam_calc                   ;fit
         !y.title = 'gamma (deg)'
         !y.range = [ min( yd[w_all] ), max( yd[w_all] ) ]
         IF ( w315[0] NE -1 ) THEN begin
	    if ( not replace ) then begin
                Curve = Obj_New( 'GA_PLOT',xd[w315], yd[w315] )
	        Curve->Set_Plot_Property, _Extra=e
	        Curve->Set_Data_Property, _Extra=e,                          $
		  Symbol=0, SymSize=0.5, Symfrac=100, /SymFill, Linestyle=6,$
		  /Marker,markLabels=w315+1,markSize=0.8
            endif else begin
	        ok = Curve->Add_Data( /NoAutoRange, xd[w315], yd[w315],      $
                _Extra=e, Symbol=0, SymSize=0.5, Symfrac=100, /SymFill,     $
                Linestyle=6, /Marker,markLabels=w315+1,markSize=0.8, ErrorBar=s_err[w315] )
	    end
	    Curve->Set_Plot_Property,title='Pitch Angle (2-sigma error bars)', $
		xtitle=!x.title, ytitle=!y.title, SetXRange=!x.range,          $
                XAutoRange=[0,0], SetYrange=!y.range, xstyle=1, _Extra=e
            ok = Curve->Add_Data( /NoAutoRange, xd[w315], yf[w315], _Extra=e2 )
	 ENDIF ELSE IF (not replace) THEN BEGIN
	    Curve = Obj_New( 'GA_Plot',[0.9,1.],[0.,0.] ) 
	    Curve->Set_Plot_Property, _Extra=e
	    Curve->Set_Data_Property, _Extra=e, Linestyle=6
         END
	 IF ( w315_off[0] NE -1 ) THEN BEGIN
	    ok = Curve->Add_Data( /NoAutoRange, xd[w315_off], yd[w315_off], $
		_Extra=e2, Symbol=0, SymSize=0.5, Symfrac=100, Hide=hide,    $
		Linestyle=6,/Marker,markLabels=w315_off+1,markSize=0.8,ErrorBar=s_err[w315_off])
	 ENDIF
	 IF ( machine NE 'JET' ) THEN BEGIN
         IF ( w15(0) NE -1 ) THEN begin
            ok = Curve->Add_Data( /NoAutoRange, xd[w15], yd[w15],           $
                _Extra=e2, Symbol=2, SymSize=0.5, Symfrac=100, /SymFill,    $
		Linestyle=6,/Marker,markLabels=w15+1,markSize=0.8,ErrorBar=s_err[w15])
            ok = Curve->Add_Data( /NoAutoRange, xd[w15], yf[w15], _Extra=e2 )
         ENDIF
	 IF ( w15_off[0] NE -1 ) THEN BEGIN
            ok = Curve->Add_Data( /NoAutoRange, xd[w15_off], yd[w15_off],   $
                _Extra=e2, Symbol=2, SymSize=0.5, Symfrac=100, Hide=hide,   $
		Linestyle=6,/Marker,markLabels=w15_off+1,markSize=0.8,ErrorBar=s_err[w15_off])
	 ENDIF

         IF w45(0) NE -1 THEN BEGIN
            ok = Curve->Add_Data(/NoAutoRange,xd(w45),yd(w45),_Extra=e2,$
		Symbol=3,SymSize=0.6,Symfrac=100,/SymFill,Linestyle=6,$
		/Marker,markLabels=w45+1,markSize=0.8,ErrorBar=s_err[w45])
            ok = Curve->Add_Data(/NoAutoRange,xd(w45),yf(w45),_Extra=e2)
         ENDIF
	 IF w45_off[0] NE -1 THEN BEGIN
            ok = Curve->Add_Data(/NoAutoRange,xd(w45_off),yd(w45_off),$
		_Extra=e2,Symbol=3,SymSize=0.6,Symfrac=100,Linestyle=6,$
		Hide=hide,/Marker,markLabels=w45_off+1,markSize=0.8,ErrorBar=s_err[w45_off])
	 ENDIF

         IF w195l(0) NE -1 THEN BEGIN
            ok = Curve->Add_Data(/NoAutoRange,xd(w195l),yd(w195l),_Extra=e2,$
		Symbol=4,SymSize=0.6,Symfrac=100,/SymFill,Linestyle=6,$
		/Marker,markLabels=w195l+1,markSize=0.8,ErrorBar=s_err[w195l])
            ok = Curve->Add_Data(/NoAutoRange,xd(w195l),yf(w195l),_Extra=e2)
         ENDIF
	 IF w195l_off[0] NE -1 THEN BEGIN
            ok = Curve->Add_Data(/NoAutoRange,xd(w195l_off),yd(w195l_off),$
		_Extra=e2,Symbol=4,SymSize=0.6,Symfrac=100,Linestyle=6,$
		Hide=hide,/Marker,markLabels=w195l_off+1,markSize=0.8,ErrorBar=s_err[w195l_off])
	 ENDIF

         IF w195u(0) NE -1 THEN BEGIN
            ok = Curve->Add_Data(/NoAutoRange,xd(w195u),yd(w195u),_Extra=e2,$
		Symbol=1,SymSize=0.6,Symfrac=100,/SymFill,Linestyle=6,$
		/Marker,markLabels=w195u+1,markSize=0.8,ErrorBar=s_err[w195u])
            ok = Curve->Add_Data(/NoAutoRange,xd(w195u),yf(w195u),_Extra=e2)
         ENDIF
	 IF w195u_off[0] NE -1 THEN BEGIN
            ok = Curve->Add_Data(/NoAutoRange,xd(w195u_off),yd(w195u_off),$
		_Extra=e2,Symbol=1,SymSize=0.6,Symfrac=100,Linestyle=6,$
		Hide=hide,/Marker,markLabels=w195u_off+1,markSize=0.8,ErrorBar=s_err[w195u_off])
	 ENDIF
	
	 ENDIF ; machine
      END

      (y1 EQ 'bz'):BEGIN
         yd = mse_bz            ;mse data
         yf = b_mid.z
	 if er then title = 'Bz with Er' else title = 'Bz w/o Er' 
         !y.title = 'Bz (T)'
         !y.range=[min(yd(w_all)) , max(yd(w_all))]
         IF ( w315[0] NE -1 ) THEN BEGIN
	    if not replace then begin
            	Curve = Obj_New('GA_PLOT',xd(w315),yd(w315))
	 	Curve->Set_Plot_property,_Extra=e
	 	Curve->Set_Data_Property,_Extra=e,$
			Symbol=0,SymSize=0.5,Symfrac=100,/SymFill,Linestyle=6,$
			/Marker,markLabels=w315+1,markSize=0.8
	    endif else $
         	ok = Curve->Add_Data(/NoAutoRange,xd(w315),yd(w315),_Extra=e,$
			Symbol=0,SymSize=0.5,Symfrac=100,/SymFill,Linestyle=6,$
			/Marker,markLabels=w315+1,markSize=0.8)
	    Curve->Set_Plot_Property,$	;title=title,
		xtitle=!x.title,ytitle='Tesla',$
		SetXRange=!x.range,XAutoRange=[0,0],SetYRange=!y.range,$
		xstyle=1,_Extra=e
	 ENDIF ELSE IF (not replace) THEN BEGIN
	    Curve = Obj_New( 'GA_Plot',[0.9,1.],[0.,0.] ) 
	    Curve->Set_Plot_Property, _Extra=e
	    Curve->Set_Data_Property, _Extra=e, Linestyle=6
	 END
	 IF w315_off[0] NE -1 THEN $
	 ok = Curve->Add_Data(/NoAutoRange,xd(w315_off),yd(w315_off),$
		_Extra=e2,Symbol=0,SymSize=0.5,Symfrac=100,Linestyle=6,$
		/Marker,markLabels=w315_off+1,markSize=0.8,Hide=hide)

	 IF machine NE 'JET' THEN BEGIN
         IF w15(0) NE -1 THEN ok = Curve->Add_Data(/NoAutoRange,_Extra=e2,$
		xd(w15),yd(w15),Symbol=2,SymSize=0.5,Symfrac=100,/SymFill,$
		Linestyle=6,/Marker,markLabels=w15+1,markSize=0.8)
         IF w15_off(0) NE -1 THEN ok = Curve->Add_Data(/NoAutoRange,_Extra=e2,$
		xd(w15_off),yd(w15_off),Symbol=2,SymSize=0.5,Symfrac=100,$
		Linestyle=6,Hide=hide,$
		/Marker,markLabels=w15_off+1,markSize=0.8)
         IF w45(0) NE -1 THEN ok = Curve->Add_Data(/NoAutoRange,_Extra=e2,$
		xd(w45),yd(w45),Symbol=3,SymSize=0.6,Symfrac=100,/SymFill,$
		Linestyle=6,/Marker,markLabels=w45+1,markSize=0.8)
         IF w45_off(0) NE -1 THEN ok = Curve->Add_Data(/NoAutoRange,_Extra=e2,$
		xd(w45_off),yd(w45_off),Symbol=3,SymSize=0.6,Symfrac=100,$
		Linestyle=6,Hide=hide,$
		/Marker,markLabels=w45_off+1,markSize=0.8)
         IF w195l(0) NE -1 THEN ok = Curve->Add_Data(/NoAutoRange,_Extra=e2,$
		xd(w195l),yd(w195l),Symbol=4,SymSize=0.6,Symfrac=100,/SymFill,$
		Linestyle=6,/Marker,markLabels=w195l+1,markSize=0.8)
         IF w195l_off(0) NE -1 THEN ok = Curve->Add_Data(/NoAutoRange,_Extra=e2,$
		xd(w195l_off),yd(w195l_off),Symbol=4,SymSize=0.6,Symfrac=100,$
		Linestyle=6,Hide=hide,$
		/Marker,markLabels=w195l_off+1,markSize=0.8)
         IF w195u(0) NE -1 THEN ok = Curve->Add_Data(/NoAutoRange,_Extra=e2,$
		xd(w195u),yd(w195u),Symbol=1,SymSize=0.6,Symfrac=100,/SymFill,$
		Linestyle=6,/Marker,markLabels=w195u+1,markSize=0.8)
         IF w195u_off(0) NE -1 THEN ok = Curve->Add_Data(/NoAutoRange,_Extra=e2,$
		xd(w195u_off),yd(w195u_off),Symbol=1,SymSize=0.6,Symfrac=100,$
		Linestyle=6,Hide=hide,$
		/Marker,markLabels=w195u_off+1,markSize=0.8)
	 ENDIF ; machine

         ok = Curve->Add_Data(/NoAutoRange,xf,yf,_Extra=e2)

	 IF machine NE 'JET' THEN BEGIN
         xp = [mse1.r_mp,mse1.r_mp]			; mag probe
         yp = [mse1.mp67,mse1.mp67]
         ;yp = [-mse1.expmpi(14),-mse1.expmpi(14)]
         ok = Curve->Add_Data(/NoAutoRange,xp,yp,_Extra=e2,$
		Symbol=10,Symfrac=100,Linestyle=6)
	 ENDIF ; machine

         if a.error eq 0 then ok = Curve->Add_Data(/NoAutoRange,_Extra=e2,$
		[a.d.rmidout,a.d.rmidout],float(!y.range),linestyle=1)
;        xr = !x.crange(0) +(!x.crange(1)-!x.crange(0))*0.6
;        yr = !y.crange(0) +(!y.crange(1)-!y.crange(0))*0.9
         xr = !x.range(0) +(!x.range(1)-!x.range(0))*0.5
         yr = !y.range(0) +(!y.range(1)-!y.range(0))*0.9

        ;Curve->Add_Text,xr,yr,xyo,charsize=0.7,color=color_index('Foreground')

      END

   ENDCASE

   ; Retore graphic system variable.
   
   !P = old_P
   !X = old_X
   !Y = old_Y

   return,Curve
END
