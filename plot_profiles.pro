pro plot_profiles, directory, source
; directory is where to write the ps file

Common ECHCOM
Common WIDGETIDS, WidIDs

catch, an_error
if an_error ne 0 then begin
	if an_error eq -531 then prof_window=-1 else begin
		print, 'Error index: ', an_error
		print, 'Error message: ', !error_state.msg
	endelse
	catch, /cancel
endif

if xregistered('plot_kinetic_profiles_echres') then begin
start_plot:
old_window=!D.WINDOW
wset, PrgDat.kinetic_plot_window

; save the colors
tvlct, orig_red, orig_green, orig_blue, /get
c1=[1,0,0,0,1,1,0,1]*255
c2=[1,0,0,1,0,0,1,1]*255
c3=[1,0,1,0,0,1,1,0]*255
white=0&black=1&blue=2&green=3&red=4&purple=5&cyan=6&yellow=7
tvlct, c1, c2, c3

if PrgDat.Profiles.enein[0] lt 0.0001 then begin
	dummy=dialog_message('Choose Zip Toray or Full Toray (gaprofiles) first.')
	return
endif

; get eqdsk tree
if gstruct.error ne 0 then return
gtgs=tag_names(gstruct)
pos=where(gtgs eq 'TREE')
if pos ge 0 then gtree=gstruct.(pos) else gtree='file'

; if no window for toray output then open one
;if prof_window LT 0 then begin
;	window, /free, retain=2, title='DENSITY AND TEMPERATURE PROFILES', $
;		xsize=500, ysize=400
;	prof_window=!D.WINDOW
;endif else wset, prof_window

wset, PrgDat.kinetic_plot_window

ishot=PrgDat.shot
time=PrgDat.time
xrange = [0.,1.2]
avg_time=10.0
plotrefl=0
norawd=0		;;indicator for no raw data and normalization factor saved in zipfit01

; get the Thomson data
case source of 

	1: begin 
		print,'  *** Getting TS data from '+PrgDat.Profiles.Source	;'zipfits'
		widget_control, /hourglass
		mdsconnect,'atlas'
		ptree='zipfit01'
		mdsopen,ptree,ishot
		
		;;;get raw ne data
		ne_rawd=mdsvalue('\edensfit:rawdata',status=st0)
		ne_rawrho=mdsvalue('dim_of(\edensfit:rawdata,0)',status=st1)
		ne_err=mdsvalue('error_of(\edensfit:rawdata)',status=st2)
		
		;;;get raw te data
		te_rawd=mdsvalue('\etempfit:rawdata',status=st3)
		te_rawrho=mdsvalue('dim_of(\etempfit:rawdata,0)',status=st4)
		te_err=mdsvalue('error_of(\etempfit:rawdata)',status=st5)

		;;;get CO2 normalization factor, time, reduced chisquare	
		norm=mdsvalue('\use_norm',status=st6)
		ne_chisq=mdsvalue('\edensfit:chisq_red',status=st7)
		t_ne=mdsvalue('dim_of(\edensfit:chisq_red)',status=st8)
		te_chisq=mdsvalue('\etempfit:chisq_red',status=st9)
		t_te=mdsvalue('dim_of(\etempfit:chisq_red)',status=st10)
		mdsclose
	
		;;;find the closest zipfit time point
		ptree='efit01'
		IF n_elements(ne_rawd) LE 1 THEN BEGIN
			norawd=1
		ENDIF ELSE BEGIN
			tmp=min(abs(t_ne-time),ind1)
			IF (min(norm) EQ max(norm)) THEN BEGIN
			    norawd=1
			    norm=1
		        ENDIF ELSE BEGIN
			    norm=norm[ind1]
			ENDELSE
			ne_rho=ne_rawrho[*,ind1]
			dens=ne_rawd[*,ind1]*norm
			dens_err=ne_err[*,ind1]
			tmp=min(abs(t_te-time),ind2)
			te_rho=te_rawrho[*,ind2]		
			temp=te_rawd[*,ind2]
			temp_err=te_err[*,ind2]		
		ENDELSE
		pt=t_ne[ind1]
		ne_chisq=ne_chisq[ind1]
		te_chisq=te_chisq[ind2]
	
	end
	
	2: begin
		print,'  *** Getting TS data from '+PrgDat.Profiles.Source	;'gaprofiles'
		;;;get raw ne data,redchisq, and CO2 normalization factor
		dnefile=PrgDat.prof_dir+'dne'+PrgDat.shottime
		if not file_test(dnefile) then begin
			dnefile=dialog_pickfile(title='Pick the SAME dne file:', filter='dne*', $
				path=PrgDat.prof_dir, get_path=dfile_path)
			PrgDat.prof_dir=dfile_path
		endif
		if dnefile EQ '' then begin
			message, 'Did not get dne file, no TS data will be plotted', /info
			source=3
		endif
		
		restore, dnefile
		pt=ne_str.ne_data.time
		ptree=ne_str.fit.erun
 		den_valid = ne_str.valid
  		den_rho = ne_str.ne_data.rho
  		den_dens = ne_str.ne_data.thom
  		den_err = ne_str.ne_data.thom_err
  		in_time_block = ne_str.ne_data.in_time_block
  		den_ntimes = ne_str.ne_data.ntimes
  		den_norm = ne_str.ne_data.norm
  		;; Apply CO2 and other normalization 
  		FOR j=0,den_ntimes-1 DO BEGIN
      		wh_block = WHERE(in_time_block EQ j,nwh_block)
      		IF nwh_block GT 0 THEN den_dens[wh_block]*=den_norm[j]
  		ENDFOR
  		whv1 = WHERE(den_valid EQ 1)
  		whv2 = WHERE(den_err GT 0.)
  		whv = setintersection(whv1,whv2)
  		ne_chisq=ne_str.fit.redchisq
		norm=den_norm 

		ne_rho=den_rho[whv]
		dens=den_dens[whv]
		dens_err=den_err[whv]

  		IF ne_str.ne_data.crefl.ierr EQ 0 THEN BEGIN 
			plotrefl=1
			crefl=ne_str.ne_data.crefl.rfldn*1.e-19
			creflrho=ne_str.ne_data.crefl.rflrho
  			nr=n_elements(whv)
			refl=fltarr(nr)
			reflrho=fltarr(nr)
			FOR i=0,nr-1 DO BEGIN
				ind=where(creflrho EQ ne_rho[i],tmp)
				reflrho[i]=creflrho[ind]
				refl[i]=crefl[ind]
			ENDFOR  
  		ENDIF

		;;;get raw te data, reduced chisquare
		dtefile=PrgDat.prof_dir+'dte'+PrgDat.shottime
		if not file_test(dtefile) then $
			dtefile=dialog_pickfile(title='Pick the SAME dte file: ', filter='dte*', $
					path=PrgDat.prof_dir, /must_exist)
		restore, dtefile		
		tem_valid = te_str.valid
		tem = te_str.te_data.temp
		tem_err = te_str.te_data.temp_err 
		tem_rho= te_str.te_data.rho         
		whv3 = WHERE(tem_valid EQ 1)
		whv4 = WHERE(tem_err GT 0.)
		whvt = setintersection(whv3,whv4)
		te_chisq=te_str.fit.redchisq
			  	
		te_rho=tem_rho[whvt]
		temp=tem[whvt]
		temp_err=tem_err[whvt]
		
	end
	
	4: begin
		print,'plotting from namelist, no TS data will be shown'
		pt=PrgDat.time
		ptree='namelist'
	end	
	
endcase		

fn= directory + 'profiles' + $
	strtrim(string(PrgDat.shot),2) + '.' + string(PrgDat.time, format='(i5.5)') + '.ps'

;Now plot Thomson density data points
dest=0
plotit:
!p.multi=[0,1,2]
!p.position=[0.1, 0.55, 0.9, 0.95]

IF ((source EQ 1) AND (norawd EQ 0)) or (source EQ 2) THEN BEGIN
	ymax =1.1*max(dens+dens_err)
	ymaxt=1.1*max(temp+temp_err)
	tit=strtrim(PrgDat.shot, 2)+'.'+string(pt,format='(i5.5)')+ ', ' 
ENDIF ELSE BEGIN
	ymax =1.15*max(PrgDat.Profiles.enein/1.e13)
	ymaxt=1.15*max(PrgDat.Profiles.tein)
	tit=''
ENDELSE

plot, [0,0], [0,0], charsize=1., $
	xrange=[0,1.2],yrange=[0, ymax], ytitle='n!De!N x 10!E19!Nm!E-3!N',/xs,/ys, $
	title=tit + ptree
IF PrgDat.Profiles.ShotCentralDensity EQ PrgDat.Profiles.CentralDensity THEN $
	oplot, PrgDat.Profiles.renein, PrgDat.Profiles.enein/1.e13, color=blue, thick=2. $
ELSE oplot, PrgDat.Profiles.renein, PrgDat.Profiles.enein/1.e13, color=red, thick=2.

IF (source EQ 1) or (source EQ 2) THEN BEGIN
	IF (norawd EQ 0) THEN oploterr, ne_rho, dens, dens_err,4
	xyouts,0.6,0.9,'Red_'+'!7v!X!U2!N='+string(ne_chisq,FORMAT='(F6.2)'),/normal,charsize=1.1
ENDIF

IF ((source EQ 1) AND (norawd EQ 0)) THEN xyouts,0.6,0.85,'norm_co2='+string(norm,FORMAT='(F5.2)'),/normal,charsize=1.1
IF ((source EQ 1) AND (norawd EQ 1)) THEN xyouts,0.6,0.85,'No CO2 normalization',/normal,charsize=1.1

IF (source EQ 2) AND plotrefl THEN oplot,reflrho,refl,psym=4,col=green
		
;Now plot Thomson temperature data
!p.position=[0.1, 0.1, 0.9, 0.5]
plot, [0,0], [0,0], charsize=1., $
	xran=[0,1.2],yrange=[0, ymaxt], xtitle='rho', ytitle='T!De!N  (keV)', /xs,/ys, $
	subtitle='g'+string(ishot,format='(i6.6)')+'.'+string(time,format='(i5.5)') + '; ' + gtree
IF PrgDat.Profiles.ShotCentralTe EQ PrgDat.Profiles.CentralTe THEN $
	oplot, PrgDat.Profiles.rtein, PrgDat.Profiles.tein, color=blue, thick=2. $
ELSE oplot, PrgDat.Profiles.rtein, PrgDat.Profiles.tein, color=red, thick=2.

IF (source EQ 1) or (source EQ 2) THEN BEGIN
	IF (norawd EQ 0) THEN oploterr,te_rho, temp, temp_err,4	
	xyouts,0.6,0.45,'Red_'+'!7v!X!U2!N='+string(te_chisq,FORMAT='(F6.2)'),/normal,charsize=1.1
ENDIF

; now write ps file
if dest eq 0 then begin
	dest=1
	xtk=!x.thick
	ytk=!y.thick
	ptk=!p.thick
	!x.thick=1.5
	!y.thick=1.5
	!p.thick=1.5
	set_plot, 'ps'
	device, /color, /inches, filename=fn, yoffset=1.0, ysize=9.0, xsize=7.0
	!p.position=[0.1, 0.55, 0.9, 0.95]
	col=!p.color
	!p.color=black
	goto, plotit
endif
!p.color=col

device, /close
print, '  *** plot_profiles: Wrote file '+fn
set_plot, 'x'
!x.thick=xtk
!y.thick=ytk
!p.thick=ptk
tvlct, orig_red, orig_green, orig_blue
!p.position=fltarr(4)
!P.MULTI=0
return
endif	; end plotting part from start_plot:

; gets here if window not yet set up
wBasePP=widget_base(/column, title='DENSITY AND TEMPERATURE PROFILES', $
	xoffset=200, yoffset=50)

; graphic widget	
wBaseDrawPP=widget_draw(wBasePP, uvalue='DrawPP', retain=2, $
	xsize=700, ysize=600)

widget_control, wBasePP, /realize
widget_control, wBaseDrawPP, get_value=kinetic_plot_window,/hourglass
PrgDat.kinetic_plot_window=kinetic_plot_window
xmanager, 'plot_kinetic_profiles_echres', wBasePP
goto, start_plot	
end
