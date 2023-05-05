; pro plot_toray
; plots the toray output to a window and overplots a gaussian fit
; also writes a ps file

pro plot_toray_event, ev
Common ECHCOM
Common WIDGETIDS, WidIDs
Common plot_toray_widids, pt_widids

catch, an_error
if an_error ne 0 then begin
	print, '  !!! plot_toray_event: error: ', !err_string
	PrgDat.Status.OutputsValid=0
	return
endif

widget_control, ev.id, get_uvalue=uval

min_power=0.050	; MW
case uval of
'powers': begin	
	; use incident power of 1 MW (inc_powers=0) or actual powers (inc_powers=1)
;	if PrgDat.inc_powers eq 0 then PrgDat.inc_powers=1 else $
;		PrgDat.inc_powers=0

	widget_control, pt_widids.pt_powers, get_value=pw
	if pw eq 0 then PrgDat.inc_powers=0 else PrgDat.inc_powers=1
	
	; but if no gyrotron has power, use 1 MW
	in=where(((ECHSys.Tank.power_MW gt min_power) and $
		(ECHSys.Tank.power_MW ne 1.0)), count)
	if count eq 0 then PrgDat.inc_powers=0
	widget_control, pt_widids.pt_powers, set_value=PrgDat.inc_powers
	plot_toray
end

'expandPT':begin
	if PrgDat.toray_plot_expand eq 0 then PrgDat.toray_plot_expand=1 $
		else PrgDat.toray_plot_expand=0
	plot_toray
end

'printit': begin
	if strtrim(PrgDat.printer_name,2) eq '' then s=dialog_message('Set printer name in window first.') $
	else if strtrim(PrgDat.print_file,2) eq '' then begin
		fn=dialog_pickfile(title='Set print file')
		if strmid(fn, strlen(fn)-3) ne '.ps' then fn=fn+'.ps'
		PrgDat.print_file=fn
	endif
	spawn, 'lp -d' + PrgDat.printer_name + ' ' + PrgDat.print_file
	print, 'Printed file ' + PrgDat.print_file + ' to printer ' + PrgDat.printer_name
end

'setptr': begin
	prntr=''
	widget_control, ev.id, get_value=prntr
	PrgDat.printer_name=prntr
	print, 'Changed default printer to ' + PrgDat.printer_name
end

'closePT': begin
	PrgDat.inc_powers=1
	widget_control, ev.top, /destroy
	return
end

else:

endcase
end


;**************
pro plot_toray
Common ECHCOM
Common WIDGETIDS, WidIDs
Common plot_toray_widids, pt_widids

;catch, an_error
;if an_error ne 0 then begin
;	print, '  !!! plot_toray: ', !err_string
;	return
;endif

if xregistered('plot_toray') then begin
start_plot:		; plotting goes here
old_window=!D.WINDOW
wset, PrgDat.toray_window
ctr=PrgDat.ctr & ctg=PrgDat.ctg & ctb=PrgDat.ctb
tvlct, ctr, ctg, ctb	; use color table from Plasma Equil window

;*********************************************************************
thk=1.75
time_slice=PrgDat.Time
shottime=string(PrgDat.Shot, format='(i6.6)')+'.'+ $
	string(time_slice, format='(i5.5)')

; look for input file (idl save file) and return if not found
fh=file_search(PrgDat.toray_sav_file, count=count)
if count NE 1 then begin
	msg=['  !!! plot_toray: ','Input file not found, ', PrgDat.toray_sav_file]
	result=dialog_message(msg)
	return
endif

; get the data to plot
restore, PrgDat.toray_sav_file	; restores toray_data array

; find how many systems have data written
sys=toray_data.system_number-1
nsys=n_elements(sys)

; use 1 MW if all the powers are below 50 kW 
in=where(((ECHSys.Tank.power_MW gt 0.050) and (ECHSys.Tank.power_MW ne 1.0)), count)
if count eq 0 then PrgDat.inc_powers=0

for ii=0, nsys-1 do if toray_data[ii].f_abs gt 0.02 then goto, okay
result=dialog_message(['plot_toray:', 'No systems have over 2% absorption: no profile plotting.'])
return
okay:

dest=0		; 0 for X, 1 for PS
chs=0.60	; character size for X
dyt=0.02525	; line spacing for X
xt=0.001		; starting location on x axis
yt=1.-dyt/4.	; starting location on y axis (top of page)

!P.MULTI = [0,1,2]

factor_FW1oe=2.*sqrt(2.)	; factor to convert to FW 1/e
factor_FWHM=2.*sqrt(2.*alog(2.))	; factor to convert to FWHM
fitrho=findgen(1001)/1000.	; rho grid for gaussian fit

; get date input file written
spawn, 'ls -aopq '+fh, res
lsp=rstrpos(res[0],' ')
fhdate=strmid(res[0], lsp-12, 12)

ii=1 & ij=0
while ii gt 0 do begin
	ii=strpos(PrgDat.toray_sav_file, '/', ii)
	if ii ne -1 then ij=ii
	ii=ii+1
endwhile

PrgDat.print_file=PrgDat.gdir+'toray'+PrgDat.shottime+'.ps'

; toray grid 1 smaller than onetwo radial grid
toray_rho=toray_data[0].rho
if toray_rho[n_elements(toray_rho)-1] lt 0.000001 then begin
	toray_rho[n_elements(toray_rho)-1]=toray_rho[n_elements(toray_rho)-2]+toray_rho[0]
	print, '  !!! toray error on last element of rho'
endif
ledge=n_elements(toray_rho)+1	; n elements on onetwo grid
rho=get_range(0.,1.,ledge)	; onetwo rho grid

; enter power factor
q_data=toray_data.q_e	; from toray.nc file, so excludes power factor
j_data=toray_data.j_eccd
I_eccd=toray_data.eccd
if PrgDat.inc_powers then begin
	for iii=0, nsys-1 do begin
		if ECHsys.Tank[sys[iii]].Power_MW gt 0.02 then $
			powW=ECHsys.Tank[sys[iii]].Power_MW*1.e6 else powW=0.
		q_data[*,iii] = toray_data[iii].q_e*powW
		j_data[*,iii] = toray_data[iii].j_eccd*powW
		I_eccd[iii]   = toray_data[iii].eccd*powW
	endfor
endif else begin
	q_data = toray_data.q_e*1.e6	; 1 MW nominal
	j_data = toray_data.j_eccd*1.e6
	I_eccd = toray_data.eccd*1.e6
	widget_control, pt_widids.pt_powers, set_value=0
endelse

if max(q_data) lt 1.E-8 then begin
	print, '  !!! No systems being plotted have power. Switching to nominal 1 MW per system.'
	PrgDat.inc_powers=0
	widget_control, pt_widids.pt_powers, set_value=0
	q_data = toray_data.q_e*1.e6	; 1 MW nominal
	j_data = toray_data.j_eccd*1.e6
	I_eccd = toray_data.eccd*1.e6
endif

; get total power density and current density
ldge=n_elements(q_data[*,0])
q_total=fltarr(ldge)
j_total=fltarr(ldge)
for iii=0, ldge-1 do begin	
	q_total[iii]=total(q_data[iii,*])
	j_total[iii]=total(j_data[iii,*])
endfor

; reconstruct total profiles to right number of points
q_tot=spline(toray_rho, q_total, rho[1:ledge-2])
q_tot=[q_total[0], q_tot, q_total[ledge-2]]
j_tot=spline(toray_rho, j_total, rho[1:ledge-2])
j_tot=[j_total[0], j_tot, j_total[ledge-2]]

; set the x range for plotting
txr=[0.,1.]
indc=where(q_tot gt 0.05*max(q_tot))
if indc[0] ge 0 then begin
	xrho=rho[indc]
	if PrgDat.toray_plot_expand then begin
		txr=[min(xrho)-0.02,max(xrho)+0.02]
		txr[0]=max([0.0, txr[0]])
	endif else txr=[0.,1.]
endif

if PrgDat.inc_powers then begin
	; for actual powers
	maxpower=max(q_tot)
	maxcur=max(j_tot)
	mincur=min(j_tot)
	maxcurrent=float(fix( (max([maxcur,2.])+1.)*1.35))
	mincurrent=float(round(min(mincur*1.2)))
endif else begin
	; for 1 MW nominal powers
	maxpow=fltarr(nsys)
	maxcur=fltarr(nsys)
	mincur=fltarr(nsys)
	for ii=0, nsys-1 do begin
		maxpow[ii]=max(toray_data[ii].q_e*1.e6)
		maxcur[ii]=max(toray_data[ii].j_eccd*1.e6)
		mincur[ii]=min(toray_data[ii].j_eccd*1.e6)
	endfor
	maxpower=max(maxpow)
	maxcurrent=float(fix( (max([max(maxcur),2.])+1.)*1.35))
	mincurrent=float(round(min(mincur*1.2)))
endelse

; some output line headings
tankhds='    System Launcher  Gyrotron  Port  ' + $
	'Pol.Cnts  Tor.Cnts    Pol1      Pol2     P(MW)'
resulthds='                % X    Polar ang  Azi ang   ' + $
	'Pol_incl   X_incl  Pol_ellip  X_ellip   f_abs'
if PrgDat.inc_powers then $
	torayhds='               ECCD(kA)    j_max(A/cm2)   rho      FWHM    ' + $
        '  q_max(W/cm3)   rho      FWHM       CD_eff' else $
	torayhds='             ECCD(kA/MW) j_max(A/cm2/MW)  rho      FWHM  ' + $
        ' q_max(W/cm3/MW)   rho      FWHM      CD_eff'

if n_elements(ffstruct) eq 0 then ffstruct=fluxfun_ech(gstruct)
rho2= rq(gstruct, 2.0, f=ffstruct)
rho32=rq(gstruct, 1.5, f=ffstruct)
rho1= rq(gstruct, 1.0, f=ffstruct)

vpos=0.01
vht=0.25
vsp=0.04	
black=1

plot_it:
; place to store the toray data
lgd=strarr(nsys)

; plot current density in lower panel
if PrgDat.inc_powers then ytit='j (A/cm2)' else ytit='j (A/cm2/MW)'
plot, txr,[0.,0.], yrange=[mincurrent, maxcurrent], psym=3, charsize=chs, $
	xtitle='rho', ytitle=ytit, pos=[0.1,vpos+vsp-0.005,0.9,vpos+vht+vsp-0.005], color=black

 
; plot q surfaces
yl=(!y.crange[1]-!y.crange[0])*0.9 + !y.crange[0]	; legend location
dyl=(!y.crange[1]-!y.crange[0])*0.1
ddxl=(!X.CRANGE[1]-!X.CRANGE[0])
xl=!X.CRANGE[1] - 0.2*ddxl
if rho1.rho_q[0] ne 0. then begin
	oplot, [rho1.rho_q[0], rho1.rho_q[0]],	[-1000.,1000], color=3, linestyle=2
	xyouts, xl, yl, 'q=1', charsize=chs, color=3
	yl -= dyl
	if rho1.rho_q[1] ne 0. then $
		oplot, [rho1.rho_q[1], rho1.rho_q[1]],	[-1000.,1000], color=3, linestyle=2
endif
	
if rho32.rho_q[0] ne 0. then begin
	oplot, [rho32.rho_q[0], rho32.rho_q[0]], [-1000.,1000], color=5, linestyle=2
	xyouts, xl, yl, 'q=3/2', charsize=chs, color=5
	yl -= dyl
	if rho32.rho_q[1] ne 0. then $
		oplot, [rho32.rho_q[1], rho32.rho_q[1]], [-1000.,1000], color=5, linestyle=2
endif
	
if rho2.rho_q[0] ne 0. then begin
	oplot, [rho2.rho_q[0], rho2.rho_q[0]], [-1000.,1000], color=4, linestyle=2
	xyouts, xl, yl, 'q=2', charsize=chs, color=4
	yl -= dyl
	if rho2.rho_q[1] ne 0. then $
		oplot, [rho2.rho_q[1], rho2.rho_q[1]], [-1000.,1000], color=4, linestyle=2
endif

if PrgDat.inc_powers then begin
	yl -= dyl
	xyouts, xl, yl, 'ECCD=' + strtrim(string(total(I_ECCD)/1000., format='(f8.3)'),2) + $
		' kA', charsize=chs, color=black
	if PrgDat.cql_totpwr gt 0.01 then begin
		yl -= dyl
		xyouts, xl, yl, 'ECCD CQL='+strtrim(string(PrgDat.cql_totcur/1000., $
			format='(f8.3)'),2) + ' kA', charsize=chs, color=black
		plots, xl-0.01,yl+0.05, psym=5, symsize=0.75, color=black
	endif
endif

;**********************************
; Plot the driven current densities
;**********************************
col=2
toteccdfit=fltarr(n_elements(rho))
for ii=0, nsys-1 do begin
	if max(abs(q_data[*,ii])) lt 0.01 then goto, skip_j_plot
	toray_cd=j_data[*,ii]
	if ii eq 2 then jj=7 else jj=ii+1	; for line and symbol type
	if max(abs(toray_cd)) gt 0.001 then begin
		; get it onto the onetwo grid (ledge points) from toray grid (ledge-1)
		cd=spline(toray_rho, toray_cd, rho[1:ledge-2])
		cd=[toray_cd[0], cd, toray_cd[ledge-2]]
		!err=0
		fit=gaussfit(rho, cd, a, nterms=3)
		if !err lt 0 then begin	; gaussfit may not converge (eg, if cd[*]=0)
			fit=cd
			fit[*]=0.
			a=[0.,0.,1.]
		endif else toteccdfit += fit
		fittot=gaussfit(rho, j_tot, atot, nterms=3)
		if !err lt 0 then begin	; gaussfit may not converge (eg, if cd[*]=0)
			fittot=j_tot
			fittot[*]=0.
			atot=[0.,0.,1.]
		endif	
		indc=where(abs(cd) gt max(abs(cd))*0.005)
		if n_elements(indc) gt 2 then begin
			oplot, rho[indc], cd[indc], psym=1, symsize=0.4, color=col
			;tmp=max(abs(cd[indc]),indc_mx)
			;rhoc_tmp=rho[indc]
			;print,'peak j @ rho of',rhoc_tmp[indc_mx]
			z=(fitrho-a[1])/a[2]
			y=a[0]*exp(-z^2/2)
			indd=where(abs(y) gt max(abs(cd))*0.005)
			if indd[0] gt -1 then $
				oplot, fitrho[indd], y[indd], linestyle=0, color=col, thick=thk ; overplot the fitted data
			inx=toray_data[ii].system_number-1

			lgd[ii]=$
				string(ECHSys.Tank[inx].GyroName, format='(a10)') + ':' + $
				string(I_eccd[ii]/1000., format='(f11.3)') + $
				'    ' + string(a[0], format='(f11.3)') + $
				'    ' + string(a[1], format='(f5.3)') + $
				'    ' + string(a[2]*factor_FWHM, format='(f6.4)')
		endif
	endif else begin
		inx=toray_data[ii].System_Number-1
		lgd[ii]=$
		string(ECHSys.Tank[inx].GyroName, format='(a9)') + ':' + $
		string(0., format='(e11.3)') + $
		'    ' + string(0., format='(f11.3)') + $
		'    ' + string(0., format='(f5.3)') + $
		'    ' + string(0., format='(f6.3)')
	endelse
	skip_j_plot:
	col++
	if col eq 5 then col++
endfor
oplot, [0.,1.], [0.,0.], color=black	; add baseline

; overplot the total current density
ncql=0
if (PrgDat.inc_powers and (abs(total(I_ECCD)) gt 100.)) then begin
	
	indc=where(abs(toteccdfit) gt max(abs(toteccdfit))*0.005)
	if n_elements(indc) gt 2 then begin
		oplot, rho[indc], toteccdfit[indc], color=black, thick=1.75
	endif

	if PrgDat.cql_done then begin
		cpts=where(abs(PrgDat.cql_pwr) gt 0., ncql)
		if ncql gt 0 then begin
			oplot, PrgDat.cql_rya[cpts], PrgDat.cql_cur[cpts]
			oplot, PrgDat.cql_rya[cpts], PrgDat.cql_cur[cpts], psym=5, symsize=0.75
		endif
	endif

endif

;********************************
; Plot the power densities
;********************************
col=2
; now do the q profile in the upper box
if PrgDat.inc_powers then ytit='q_e (W/cm3)' else ytit='q_e (W/cm3/MW)'
plot, txr, [0.,0], yrange=[0.,maxpower*1.3], psym=3, charsize=chs, $
	ytitle=ytit, pos=[0.1,vpos+vht+vsp,0.9,vpos+2.*vht+0.03], color=black,xtickname=[' ',' ',' ',' ',' ',' ']

yl=!y.crange[1]*0.85	; legend location
dyl=yl/10.
if PrgDat.inc_powers then begin
	oplot, [xl,xl], [yl,yl], psym=1, color=black, symsize=0.4
	oplot, [xl+0.01*ddxl,xl+0.05*ddxl], [yl,yl], line=0, color=black
	xyouts, xl+0.055*ddxl, yl, 'Total', charsize=chs, color=black
	yl -= dyl
endif

totpowfit=fltarr(n_elements(rho))
total_absorbed_power=0.
for ii=0, nsys-1 do begin
	if max(abs(q_data[*,ii])) lt 0.01 then goto, skip_q_plot
	total_absorbed_power += toray_data[ii].power*toray_data[ii].f_abs
	toray_pow=q_data[*,ii]
	if ii eq 2 then jj=7 else jj=ii+1
	if max(toray_pow) gt 0. then begin

		; get it onto the onetwo grid (ledge points) from toray grid (ledge-1)
		pow=spline(toray_rho, toray_pow, rho[1:ledge-2])
		ipow=where(finite(pow) lt 1, nipow)
		if nipow gt 0 then begin
			cd=interpp(toray_rho, toray_pow, rho[1:ledge-2])
			print, '  *** plot_toray found spline failure in power; using interpp'
		endif
		pow=[toray_pow[0], pow, toray_pow[ledge-2]]
		fit=gaussfit(rho, pow, a, nterms=3)
		z=(fitrho-a[1])/a[2]
		y=a[0]*exp(-z^2/2.)
		indp=where(y gt maxpower*0.001)
		if indp[0] ge 0 then $
			oplot, fitrho[indp], y[indp], linestyle=0, thick=thk, color=col
		indp=where(pow gt maxpower*0.001)
		if indp[0] ge 0 then $
			oplot, rho[indp], pow[indp], psym=1, symsize=0.4, color=col
		totpowfit += fit
		;tmp=max(pow[indp],indp_mx)
		;rhop_tmp=rho[indp]
		;print,'peak q_e @ rho of',rhop_tmp[indp_mx]		
		; now do the legend
		oplot, [xl,xl], [yl,yl], psym=1, color=col, symsize=0.4
		oplot, [xl+0.01*ddxl,xl+0.05*ddxl], [yl,yl], line=0, color=col, thick=thk
		
		xyouts, xl+0.055*ddxl, yl, string(sys[ii]+1, format='(i1)') + ': ' + $
			toray_data[ii].gyroname, charsize=chs, color=black
		yl=yl-dyl

		lgd[ii]=lgd[ii] + $
			;'    ' + string(toray_data[ii].f_abs, format='(f6.3)') + $
			'    ' + string(a[0], format='(f11.3)') + $
			'    ' + string(a[1], format='(f5.3)') + $
			'    ' + string(a[2]*factor_FWHM, format='(f6.4)') + $
			'    ' + string(toray_data[ii].cd_eff, format='(f8.5)')

		skip_q_plot:
		col++
		if col eq 5 then col++
		
	endif
endfor

if PrgDat.inc_powers then begin
	yl=yl-dyl
	xyouts, xl, yl, 'P_ABS='  + strtrim(string(total_absorbed_power/1.e6, format='(f6.3)'),2) + $
		' MW', charsize=chs, color=black
	if ncql gt 0 then begin
		yl -= dyl
		xyouts, xl, yl, 'P_ABS CQL=' +strtrim(String(PrgDat.cql_totpwr/1.e6, $
			format='(f5.3)'),2) + ' MW', charsize=chs, color=black
		plots, xl-0.01, yl+0.005, psym=5, symsize=0.5, color=black
	endif
endif

ilgd=where(lgd ne '')
if ilgd[0] ge 0 then lgd=lgd[ilgd]

; overplot the total power density
if PrgDat.inc_powers then begin
	indc=where(abs(totpowfit) gt max(abs(totpowfit))*0.0001)
	if n_elements(indc) gt 2 then begin
		oplot, rho[indc], totpowfit[indc], color=black, thick=1.75
	endif
	if PrgDat.cql_done and ncql gt 0 then begin
		oplot, PrgDat.cql_rya[cpts], PrgDat.cql_pwr[cpts]
		oplot, PrgDat.cql_rya[cpts], PrgDat.cql_pwr[cpts], psym=5, symsize=0.5
	endif

endif

;********************************
; Add the integtrals
;********************************

lgdtot='     Total:' + string(total(I_ECCD)/1000., format='(f11.3)')
if n_elements(atot) gt 0 then lgdtot+=$
	'    ' + string(atot[0], format='(f11.3)') + $
	'    ' + string(atot[1], format='(f5.3)') + $
	'    ' + string(atot[2]*factor_FWHM, format='(f6.4)')

fittotq=gaussfit(rho, q_tot, b, nterms=3)

lgdtot += 	'    ' + string(b[0], format='(f11.3)') + $
			'    ' + string(b[1], format='(f5.3)') + $
			'    ' + string(b[2]*factor_FWHM, format='(f6.4)')

; plot the date and equilibrium name, configuration is consistent with the main ECHRES window	
widget_control, WidIDs.UsePresent,get_value=pres
	
if pres EQ 1 then configuration='Present' else $
	configuration='Historical'	
yt=0.98
xyouts, xt, yt, 'Run: '+ systime() + ' ;  Eq: ' + PrgDat.gfile + $
	';  Configuration=' + configuration, charsize=chs, font=0, color=black, /normal
yt-=dyt
xyouts, xt, yt, PrgDat.Source, charsize=chs, font=0, color=black, /normal
; RP xyouts, xt, yt, 'Toray ver 1.8' + $
;	;strtrim(string(nl.namelis2.toray_version[0], format='(f20.3)'),2) + $
;	; nray=' + strtrim(run_data.nray,2) + $
;	'; Gauss zones=' + strtrim(torayin.gauszone[0],2) + $
;	'; ds=' + strtrim(string(torayin.ds[0], format='(f20.3)'),2) + $
;	' cm; dsmin=' + strtrim(string(torayin.dsmin[0], format='(f20.3)'),2) + $
;	' cm; nharm=' + strtrim(torayin.nharm[0],2), $
;	charsize=chs, font=0, color=black, /normal
yt -= dyt
profstr='n_e(0)=' + strtrim(PrgDat.Profiles.CentralDensity,2) + $
	'/cm^3; T_e(0)=' + strtrim(PrgDat.Profiles.CentralTe,2) + ' keV'
if abs(PrgDat.Profiles.Bt_orig) gt 0.01 and $
	abs(PrgDat.Profiles.Bt0 - PrgDat.Profiles.Bt_orig) gt 1.e-4 then $
	profstr += '; B_t0 changed from ' + strtrim(PrgDat.Profiles.Bt_orig,2) + ' T to ' + $
		strtrim(PrgDat.Profiles.Bt0,2) + ' T' $
	else profstr += '; B_t0=' + strtrim(PrgDat.Profiles.Bt0,2) + ' T'
xyouts, xt, yt, profstr, charsize=chs, font=0, color=black, /normal
yt-=1.5*dyt   

if dest eq 1 then begin	; dest=1 => ps output
	; output the input table
	xyouts, xt, yt, tankhds, charsize=chs, /normal, font=0, color=black
	yt-=dyt
	ptot=0.0
	for jj=0, nsys-1 do begin
		inx=toray_data[jj].System_Number-1
		ptot += ECHSys.Tank[inx].Power_MW
		sin= '   ' + strtrim(inx+1,2) + ':' + $
			stg(ECHSys.tank[inx].TankNo, '(i2)', 4) + $
			stg(ECHSys.Tank[inx].Antenna, '(a12)', 10) + $
			stg(ECHSys.tank[inx].GyroName, '(a12)', 10) + $
			stg(ECHSys.Tank[inx].AntNum, '(i2)', 4) + $
			stg(ECHSys.InputTable[inx].PolCts, '(i8)', 10) + $
			stg(ECHSys.InputTable[inx].TorCts, '(i8)', 10) + $
			stg(ECHSys.InputTable[inx].pol1, '(f10.1)', 10) + $
			stg(ECHSys.InputTable[inx].pol2, '(f10.1)', 10) + $
			stg(ECHSys.Tank[inx].Power_MW, '(f12.3)', 10)
			
		if jj eq nsys-1 then sin += '  P_INC=' + $
			stg(ptot, '(f6.3)', 6) + ' MW'
			
		xyouts, xt, yt, sin, color=black, charsize=chs, /normal, font=0
		yt-=dyt
	endfor
	yt=yt-0.5*dyt
	
	; output the output table
	xyouts, xt, yt, resulthds, charsize=chs, /normal, font=0, color=black
	yt=yt-dyt
	for jj=0, nsys-1 do begin
		inx=toray_data[jj].System_Number-1
		sout=stg(ECHSys.Tank[inx].gyroname, '(a10)', 10)+ ':'
		sout += $
			stg(ECHSys.InputTable[inx].xfrac, '(f14.3)', 10) + $
			stg(ECHSys.InputTable[inx].PolarAng, '(f14.3)', 10) + $
			stg(ECHSys.InputTable[inx].AziAng, '(f14.3)', 10) + $
			stg(ECHSys.Antenna[inx].alpha_a, '(f14.3)', 10) + $
			stg(ECHSys.Antenna[inx].alpha_x, '(f14.3)', 10) + $
			stg(ECHSys.Antenna[inx].beta_a, '(f14.3)', 10) + $
			stg(ECHSys.Antenna[inx].beta_x, '(f14.3)', 10) + $
			stg(ECHSys.InputTable[inx].f_abs, '(f14.3)', 10)
		xyouts, xt, yt, sout, charsize=chs, /normal, font=0, color=black
		yt-=dyt
	endfor
	yt-=0.5*dyt
	
	; output the ECCD data
	xyouts, xt, yt, torayhds, charsize=chs, /normal, font=0, color=black
	yt-=dyt
	for ii=0, n_elements(lgd)-1 do begin
		xyouts, xt, yt, lgd[ii], charsize=chs, /normal, font=0, color=black
		yt-=dyt
	endfor
	if PrgDat.inc_powers then begin
		xyouts, xt, yt, lgdtot, charsize=chs, /normal, font=0, color=black
		yt-=dyt
	endif
	yt -= 0.5*dyt
	;xyouts, xt, yt, '  Peak abs:    R (m)     Z(m)    phi(deg)', charsize=chs, /normal, font=0, color=black
	;for jj=0, nsys-1 do begin
	;	phi_pk=toray_data[jj].phi_peak/!dtor 
	;	jjj=where(echsys.tank.gyroname eq toray_data[jj].gyro_name)
	;	if jjj[0] lt 0 then begin
	;		print, '  !!! No gyrotron by name of ', toray_data[0].gyro_name
	;		goto, time_to_go
	;	endif
	;	phi_ant=float(strmid(ECHSys.Tank[jjj].antport, 0, $
	;		strpos(ECHSys.Tank[jjj].antport, ' ')))
	;	phi_peak= phi_ant - phi_pk
	;	yt -= dyt
	;	xyouts, xt, yt, stg(toray_data[jj].gyro_name, '(a12)', 10) + ':    ' +$
	;		stg(toray_data[jj].r_peak, '(f6.3)', 5) + '    ' + $
	;		stg(toray_data[jj].z_peak, '(f6.3)', 5) + '    ' + $
	;		stg(phi_peak, '(f8.3)', 7), charsize=chs, /normal, font=0, color=black
	;endfor
	
	; close the file and return to X
	device, /close
	set_plot, 'x'

	;PrgDat.print_file=fn
	print, '  *** Wrote postscript file ' + PrgDat.print_file
	print, ''
	!P.MULTI=0
	return
endif else begin
	; output the ECCD table
	xyouts, xt, yt, torayhds, charsize=chs, /normal, color=black, font=0
	yt=yt-dyt
	for ii=0, n_elements(lgd)-1 do begin	; write legend
		xyouts, xt, yt, lgd[ii], charsize=chs, /normal, color=black, font=0
		yt-=dyt
	endfor
	if PrgDat.inc_powers then begin
		xyouts, xt, yt, lgdtot, charsize=chs, /normal, color=black, font=0
		yt-=dyt
	endif
	
	yt -= 0.5*dyt
	;xyouts, xt, yt, '  Peak abs:    R (m)     Z(m)    phi(deg)', charsize=chs, /normal, font=0, color=black
	;for jj=0, nsys-1 do begin
	;	phi_pk=toray_data[jj].phi_peak/!dtor 
	;	jjj=where(echsys.tank.gyroname eq toray_data[jj].gyroname)
	;	if jjj[0] lt 0 then begin
	;		print, '  !!! No gyrotron by name of ', toray_data[0].gyro_name
	;		goto, time_to_go
	;	endif
	;	phi_ant=float(strmid(ECHSys.Tank[jjj].antport, 0, $
	;		strpos(ECHSys.Tank[jjj].antport, ' ')))
	;	phi_peak= phi_ant - phi_pk
	;	yt -= dyt
	;xyouts, xt, yt, stg(ECHSys.Tank[jjj].gyroname, '(a12)', 10) + ':    ' +$
	;	stg(toray_data[jj].r_peak, '(f6.3)', 5) + '    ' + $
	;	stg(toray_data[jj].z_peak, '(f6.3)', 5) + '    ' + $
	;	stg(phi_peak, '(f8.3)', 7), charsize=chs, /normal, font=0, color=black
	;endfor

	dest = 1
	thk=2.5
	yl=1.1
	chs=0.62
	dyt=0.013
	set_plot, 'ps'
	device, /inches, yoffset=1.0, ysize=9.5, xsize=7.0, /color, /COURIER, $
		filename=PrgDat.print_file, /portrait
	!x.thick=1.0
	!y.thick=1.0
	black=1
	goto, plot_it
endelse
;*****************************************************************
time_to_go:
set_plot, 'x'
return
endif ; end plotting part from start_plot:

;if window not yet set up
wBasePT=widget_base(/column, title='TORAY Plot Window', group_leader=WidIDs.Base, $
	xoffset=200, yoffset=50)

; graphic widget
wBaseDrawPT=widget_draw(wBasePT, uvalue='DrawPT', retain=2, $
	xsize=700, ysize=600)

; button widgets
wBaseButtonsPT=Widget_Base(wBasePT, /row)
wPower=cw_bgroup(wBaseButtonsPT, /exclusive, ['1 MW', 'Actual'], /row, $
	label_left='Inc. powers:', button_uvalue=[0,1], $
	uvalue='powers', set_value=1)
;wPower=widget_button(wBaseButtonsPT, value='Inc. Powers/1 MW nominal', uvalue='powers')
wExpandPlot=widget_button(wBaseButtonsPT, value=' Expand/Full rho ', uvalue='expandPT')

wPwinter=cw_field(wBaseButtonsPT, title='  Printer:', xsize=12, value=PrgDat.printer_name, $
	uvalue='setptr', /return_events, /string)
	
wPrint=widget_button(wBaseButtonsPT, value=' Print ', uvalue='printit')
wClosePT=widget_button(wBaseButtonsPT, value=' Close ', uvalue='closePT')

widget_control, wBasePT, /realize
widget_control, wBaseDrawPT, get_value=toray_window
PrgDat.toray_window=toray_window
pt_widids={pt_powers:wPower}

xmanager, 'plot_toray', wBasePT
goto, start_plot
end
