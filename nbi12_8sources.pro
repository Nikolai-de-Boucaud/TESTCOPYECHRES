;************************************************************************
;+
; NAME:
;	NBI12_8SOURCES
;
; PURPOSE:
;	Prepare time history of beam power for ONETWO for all eight sources
;
; CATEGORY:
;
; CALLING SEQUENCE:
;	result = NBI12_ALL(shot,time,time_avg,center=center,twobeams=twobeams)
;
; INPUTS:
;	shot		shot number
;	time		array of times (ms)
;	time_avg	averaging time, default=80 ms
;	center		keyword for centered averaging (default is 
;			backwards averaging)
;	twobeams	keyword for old style of ONETWO input that was
;			limited to two beamlines
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;	This function returns structure 
;	{ ERR : error index, 0 - normal
;	  MSG : error message
;	  NBI : beam structure array
;		{ TIME:float, BEAMON:fltarr(8), BTIME:fltarr(8), NBEAMS:int, 
;                 NSOURCE:int, SFRAC1:fltarr(8), EBKEV:fltarr(8), 
;		  BPTOR:fltarr(8), ANGLEH:fltarr(8) }
;	}
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
;	The beam parameters returned by this routine are appropriate for
;	inserting into the INONE file of a snapshot ONETWO run. The 
;	parameters BEAMON, BTIME, NBEAMS, NSOURCE, SFRAC1, EBKEV, BPTOR,
;	and ANGLEH should be inserted into the second namelist of the INONE
;	file (the parameter TIME should equal TIME0 in the first namelist).
;	Even though ONETWO can handle only a single beam pulse, this routine
;	correctly handles multiple beam pulses by adjusting the BEAMON time
;	for each snapshot ONETWO run. A substantial amount of effort has
;	gone into mocking up the beam turn on and turn off to the best of
;	ONETWO's ability. To understand this, one first needs to understand
;	how ONETWO handles the beam turn on and turn off.
;
;	In ONETWO, the beam pulse starts at time BEAMON. Since ONETWO only
;	calculates the asymptotic (time independent) solution to the fast
;	ion slowing down equation, it imposes an ad hoc exponential rise
;	to the fast ion density on the fast ion slowing down time scale.
;	After the beams have been on for three slowing down times, ONETWO
;	deems the fast ion population to be fully formed. However, in
;	snapshot mode, after the beams are turned off ONETWO does not use
;	an exponential decay to the fast ion density. If the beams are off
;	at the time of the snapshot analysis, even if just for an instant,
;	then no beam calculation is performed and the beam density is zero.
;
;	NOTE: I HAVE DISCOVERED A BUG IN ONETWO WHEREBY IN SNAPSHOT MODE
;	BTIME APPEARS TO BE HARDWIRED TO INFINITY, SO THE BEAMS NEVER TURN
;	OFF. I HAVE INFORMED HOLGAR ST JOHN OF THIS BUG. NBI12_ALL WORKS
;	AROUND THIS BUG BY SETTING NBEAMS=0 WHENEVER THE BEAMS ARE NOT
;	TURNED ON.
;
;	This routine has special features to mock up the beam turn on and
;	turn off. First, when the snapshot analysis time occurs around the
;	BEAMON time, the parameter TIME_AVG is overridden so that times
;	before BEAMON are not included in determining the beam duty factor.
;	(This is most apparent when the keyword "center" is chosen.) This
;	preserves casuality and allows the ad hoc exponetial rise in the
;	ONETWO fast ion density to work as intended. This special feature
;	becomes moot after the beams have been on for TIME_AVG length of
;	time. Second, the beam turn off is approximated by linearly ramping
;	down the beam power over TIME_AVG length of time after the beams
;	have turned off. To cajole ONETWO into doing the beam calculation
;	after the beam turn off time, the parameter BTIME is artificially
;	increased by TIME_AVG length of time. Again, this special feature
;	becomes moot after the beams have been off for TIME_AVG length of
;	time. (Note that all of the above mentions of TIME_AVG should be
;	replaced by TIME_AVG/2 in the case of centered averaging.)
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; HISTORY:
; 		Created by Craig Petty starting from NBI12.PRO
;		5/1/2006 CCP Changed returned values for TIME, BEAMON and
;		BTIME from ms to s (to be compatible with ONETWO code)
;		
;		Modified by Ron Prater to allow vertically tilted beams and
;		sources. (1/24/2012) Also, the beam gas and energy fractions
;		are returned. The beam fractions use d3d_beam_species_mix
;		by Chuck Greenfield. The changes are denoted by ';RP'.
;
;************************************************************************
@$IDLSOURCE/general/evaluate
@$IDLSOURCE/data/get_nbi
@$IDLSOURCE/write_nbfiles/oanb_parms
@$IDLSOURCE/autotransp/public/ufiles/d3d_beam_species_mix

function nbi12_8sources,shot,time,time_avg,center=center,twobeams=twobeams
forward_function get_nbi
if keyword_set(center) then center=1 else center=0
if keyword_set(twobeams) then twobeams=1 else twobeams=0
on_error,2
beam = {err:0,msg:''}

; check for shot number
if (n_elements(shot) lt 1) then begin
	beam.err = 1
	beam.msg = 'Shot number missing'
	beam = create_struct(beam,'nbi','')
	return,beam
endif

; determine number of time slices
num_time = n_elements(time)
if (num_time lt 1) then begin
	beam.err = 1
	beam.msg = 'Input time array is empty'
	beam = create_struct(beam,'nbi','')
	return,beam
endif

; get nbi - note that it could hang at get_beam_param's call external 
; if the shot is not on disk.
setenv, 'SHARED4D = $VERSION4D/bin/hp/4dlib.sl'
nbi = get_nbi(shot,/cache)

; return if error
if (nbi.ierr ne 0) then begin
	beam.err = 1
	beam.msg = 'Failed retrieving beam power'
	beam = create_struct(beam,'nbi','')
	return,beam
endif

; convert V -> kV (corrected to MKS in MDSplus)
nbi.volts = nbi.volts / 1000.

; RP -- add data regarding tilted 150 deg beam; also beam species data
; Calculate rpivot, zpivot, anglev, and fbcur for sources 2 and 3
rpivot=replicate(286.56, 8) ; arbitrary value
zpivot=fltarr(8)
anglev=fltarr(8)
blenp=fltarr(8)
fbcur=fltarr(3,8)	; full, half, and third energy

op=oanb_parms(nbi.beamtilt[2], nbi.sourcetilt[2], nbi.sourcetilt[3], $
	shot, '150')
rpivot[2]=total(op[0:3].rpivot)/4.	; take average of 4 beamlets
zpivot[2]=total(op[0:3].zpivot)/4.
anglev[2]=total(op[0:3].anglev)/4.
blenp[2]=total(op[0:3].blenp)/4.
rpivot[3]=total(op[4:7].rpivot)/4.
zpivot[3]=total(op[4:7].zpivot)/4.
anglev[3]=total(op[4:7].anglev)/4.
blenp[3]=total(op[4:7].blenp)/4.

; get beam ion species
abeam=fltarr(8)
k=where(nbi.gas eq '', nk)
if nk gt 0 then begin
	; first, check that both sources have same gas
	; Note that gas is not written if source not used
	for j=0, nk-1 do begin
		if k[j] mod 2 eq 0 then $ ; it's even
			if nbi.gas[k[j]+1] ne '' then nbi.gas[k[j]]=nbi.gas[k[j]+1]
		if k[j] mod 2 eq 1 then $
			if nbi.gas[k[j]-1] ne '' then nbi.gas[k[j]]=nbi.gas[k[j]-1]
	endfor
	; D2 is default if data not written to MDSplus
	k=where(nbi.gas eq '', nk)
	if nk gt 0 then nbi.gas[k]='D2'
endif
k=where(nbi.gas ne 'D2' and nbi.gas ne 'H2', nk)
if nk gt 0 then begin
	print, ''
	print, '   !!! Converting unrecognized beams '+strtrim(k+1,2)+' to D2 so calculation can proceed.'
	nbi.gas[k]='D2'
endif
k=where(nbi.gas eq 'D2', nk)
if nk gt 0 then abeam[k]=2.
k=where(nbi.gas eq 'H2', nk)
if nk gt 0 then abeam[k]=1.
zbeam=replicate(1.,8)	; hydrogen isotopes only

; now get beam energy fractions
k=where(nbi.volts eq 0., nk)
if nk gt 0 then nbi.volts[k]=80.0	; beam not on, but still need nonzero V
for i=0, 7 do begin
	fb=d3d_beam_species_mix(nbi.volts[i], abeam[i], 1.)
	fbcur[*,i]=fb.before
endfor

; end RP

; check if averaging time was specified
if (n_elements(time_avg) lt 1) then begin
  time_avg = 80.
endif else begin
  time_avg = float( round( time_avg ) )
endelse
if center then time_int=time_avg/2. else time_int=time_avg

; round time_mds to nearest 0.1 ms
;nbi.time_mds = round( nbi.time_mds * 10 ) / 10.

; fill in missing beam voltages
;miss = where( nbi.volts eq 0., len)
;if (len gt 0) then begin
; for i=0,len-1 do nbi.volts[miss[i]] = 75.0
;endif
; fill in missing beam powers
;miss = where( nbi.power eq 0., len)
;if (len gt 0) then begin
; for i=0,len-1 do nbi.power[miss[i]] = 2.2e6
;endif

; check for errors with individual beams
miss = where( nbi.errors ne 0, len)
if (len gt 0) then begin
  for i=0,len-1 do begin
    if (nbi.volts[miss[i]] eq 0) then nbi.volts[miss[i]] = 75.0
    if (nbi.power[miss[i]] eq 0) then nbi.power[miss[i]] = 2.2e6
    print,'WARNING: Filled in missing data for ',nbi.names[miss[i]]
  end
endif

; search for beam start times and durations
; loop through each beam separately
beamon = fltarr(999,8)
btime = fltarr(999,8)
num_beamon = fltarr(8)
for i=0,7 do begin
  ton = where( nbi.ibeam[*,i] ne 0 , len1 )
  if (len1 gt 0) then begin
    delta_t = nbi.time_mds[ton[0]+1]-nbi.time_mds[ton[0]]
    delta_n = round( time_avg / delta_t )
    num_beamon[i] = 1
    beamon[0,i] = nbi.time_mds[ton[0]]
    LOOPAGAIN:
    toff = where( nbi.ibeam[ton[0]:ton[len1-1],i] eq 0, len2)
    if (len2 gt delta_n) then begin
      toff = toff + ton[0]
      for j=0L,len2-delta_n-1 do begin
        time2 = nbi.time_mds[toff[j+delta_n]] - nbi.time_mds[toff[j]]
        if (time2 eq time_avg) then begin
          beamoff = nbi.time_mds[toff[j]-1]
          btime[num_beamon[i]-1,i] = beamoff-beamon[num_beamon[i]-1,i]+time_int
          num_beamon[i] = num_beamon[i] + 1
          ton = where( nbi.ibeam[toff[j]+delta_n:ton[len1-1],i] ne 0, len1 )
          ton = ton + toff[j] + delta_n
          beamon[num_beamon[i]-1,i] = nbi.time_mds[ton[0]]
          goto, LOOPAGAIN
        endif
      end
      beamoff = nbi.time_mds[ton[len1-1]]
      btime[num_beamon[i]-1,i] = beamoff - beamon[num_beamon[i]-1,i] + time_int
    endif else begin
      beamoff = nbi.time_mds[ton[len1-1]]
      btime[num_beamon[i]-1,i] = beamoff - beamon[num_beamon[i]-1,i] + time_int
    endelse
  endif else begin
    num_beamon[i] = 0
  endelse
end

; Vertical divergence of DIII-D beam
; write_nbfiles reports divza as 0.0227 radians,
; whereas the default in ONETWO for NFREYA is bvdiv=1.3 degrees,
; which is 0.0227 radians, but apparently there is a discrepancy
; in how ONETWO and NUBEAM use that divergence (factor of sqrt(2))
; as reported by Bob Harvey.
; Therefore change the default for ONETWO input to be 1.839 (=1.3*sqrt(2)).
bvdiv = replicate(1.3*sqrt(2),8)

; create structure that contains time history of nbi power
inonenbi = create_struct( $
	'time',0.0,		$ time in ms
	'beamon',fltarr(8),	$ beam start time in ms
	'btime',fltarr(8),	$ beam on duration in ms
	'nbeams',8L,		$ number of beams
	'nsource',2L,		$ number of beam sources per beamline
	'sfrac1',fltarr(8),	$ fraction of power injected by right source
	'ebkev',fltarr(8),	$ beam voltage in keV
	'bptor',fltarr(8),	$ beam power in W
	'angleh',fltarr(8),	$ toroidal injection angle in degrees
	'anglev',anglev,    $ vertical injection angle in degrees
	'rpivot',rpivot,    $ radial value of pivot point (cm)
	'zpivot',zpivot,    $ vertical value of pivot point (cm)
	'blenp',blenp,      $ distance to collimator (cm)
	'fbcur',fbcur,      $ fractional beam power (full, half, third)
	'abeam',abeam,      $ beam species atomic number (H and D only)
	'zbeam',zbeam,      $ beam species charge number
  'bvdiv',bvdiv       $ beam vertical divergence
	)
inonenbi = replicate( inonenbi, num_time )

; loop through requested times to fill in inonenbi structure
duty = fltarr(8)
for k=0,num_time-1 do begin

  ; calculate beam timing
  inonenbi[k].time = time[k]
  time_avg1 = fltarr(8)

  ; loop through all 8 beams
  for h=0,7 do begin
    if (num_beamon[h] eq 0) then begin
      inonenbi[k].beamon[h] = 99000.0
      inonenbi[k].btime[h] = 1000.0
      time_avg1[h] = time_avg
    endif else begin
      inonenbi[k].beamon[h] = beamon[0,h]
      inonenbi[k].btime[h] = btime[0,h]
      for i=1,num_beamon[h]-1 do begin
        if (time[k] ge beamon[i,h]) then begin
          inonenbi[k].beamon[h] = beamon[i,h]
          inonenbi[k].btime[h] = btime[i,h]
        endif
      end
      if ( (time[k]-inonenbi[k].beamon[h]) lt time_int ) then begin
        time_avg1[h] = time[k] - inonenbi[k].beamon[h]
      endif else begin
        time_avg1[h] = time_avg
      endelse
    endelse

    ; calculate duty factors
    if (time_avg1[h] eq 0.0) then time_avg1[h] = delta_t
    if center then begin
      j = where(nbi.time_mds ge time[k] - time_avg1[h] / 2. and $
                nbi.time_mds lt time[k] + time_avg1[h] / 2. , num)
    endif else begin
      j = where(nbi.time_mds gt time[k] - time_avg1[h] and $
                nbi.time_mds le time[k] , num)
    endelse

    if (num lt 1) then begin
      num = 0
    endif else begin
      duty[h] = total( nbi.ibeam[j,h] ) / num
    endelse
  end
  onbeams = where( duty gt 0, num)

  ; calculate rest of beam parameters
  if ( shot gt 124000 ) then begin
    inonenbi[k].angleh = [19.5,19.5,19.5,19.5,-19.5,-19.5,19.5,19.5]
    inonenbi[k].sfrac1 = [1.,0.,1.,0.,0.,1.,1.,0.]
  endif else begin
    inonenbi[k].angleh = [19.5,19.5,19.5,19.5,19.5,19.5,19.5,19.5]
    inonenbi[k].sfrac1 = [1.,0.,1.,0.,1.,0.,1.,0.]
  endelse
  inonenbi[k].ebkev = nbi.volts
  inonenbi[k].bptor = nbi.power*duty
  if (num eq 0) then inonenbi[k].nbeams = 0
end

; convert from ms to s for parameters time, beamon, and btime
inonenbi.time = inonenbi.time / 1000.
inonenbi.beamon = inonenbi.beamon / 1000.
inonenbi.btime = inonenbi.btime / 1000.

for k=0,num_time-1 do begin
  timei = inonenbi[k].time
  ind = where(inonenbi[k].beamon le timei and $
              inonenbi[k].beamon+inonenbi[k].btime ge timei and $
              inonenbi[k].bptor ne 1, nbeams,complement=comp_ind)
  if nbeams gt 0 then begin
      if comp_ind[0] ne -1 then ind = [ind,comp_ind]
      inonenbi[k].nbeams = nbeams
      inonenbi[k].sfrac1[0:*] = inonenbi[k].sfrac1[ind]
      inonenbi[k].ebkev[0:*] = inonenbi[k].ebkev[ind]
      inonenbi[k].bptor[0:*] = inonenbi[k].bptor[ind]
      inonenbi[k].angleh[0:*] = inonenbi[k].angleh[ind]
      inonenbi[k].anglev[0:*] = inonenbi[k].anglev[ind]
      inonenbi[k].rpivot[0:*] = inonenbi[k].rpivot[ind]
      inonenbi[k].zpivot[0:*] = inonenbi[k].zpivot[ind]
      inonenbi[k].blenp[0:*] = inonenbi[k].blenp[ind]
      inonenbi[k].fbcur[0:2,0:*] = inonenbi[k].fbcur[0:2,ind]
      inonenbi[k].abeam[0:*] = inonenbi[k].abeam[ind]
      inonenbi[k].zbeam[0:*] = inonenbi[k].zbeam[ind]
      inonenbi[k].beamon[0:*] = inonenbi[k].beamon[ind]
      inonenbi[k].btime[0:*] = inonenbi[k].btime[ind]
  endif else inonenbi[k].nbeams = 0
endfor

; return nbi as a structure
beam = create_struct(beam,'nbi',inonenbi)
return,beam

end
