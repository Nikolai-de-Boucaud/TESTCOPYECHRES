; npar_spread
; Inputs: toray.nc structure
; Outputs: info about n_parallel spectrum
pro npar_spread, s, npar0, npar_min, npar_max, npar_mean, npar_dev
; s is toray.nc structure

; 96% of power absorbed between these n_par points for each ray
f1=exp(-4.)	; 98.2% extinction of total absorbed power
f2=1-f1		;  1.8% extinction of total absorbed power
npar=fltarr(3*s.nray)	; evaluate n_parallel at 3 points on ray

for i=0, s.nray-1 do begin
	j=3*i
	pwr=s.delpwr[*,i]/s.delpwr[0,i]
	lpwr=n_elements(pwr)-1
	dpwr=pwr[0]-pwr[lpwr]

	fi=max(where(pwr gt (pwr[0]-0.5*(pwr[0]-pwr[lpwr]))))
	if fi ge 0 then begin
		fid=fi+(0.5-pwr[fi])/(pwr[fi+1]-pwr[fi])
		npar[j]=interpolate(s.wnpar[*,i],fid)
	endif else npar[j]=0.

	fi=max(where(pwr gt (pwr[0]- f1*dpwr)))
	if fi ge 0 then begin
		if fi eq lpwr then npar[j]=s.wnpar[lpwr,i] else begin
			fid=fi+(pwr[0]-f1*dpwr-pwr[fi])/(pwr[fi+1]-pwr[fi])
			npar[j+1]=interpolate(s.wnpar[*,i],fid)
			endelse
	endif else npar[j+1]=npar[0]		

	fi=max(where(pwr gt (pwr[0]- f2*dpwr)))
	if fi ge 0 then begin
		fid=fi+(pwr[0]-f2*dpwr-pwr[fi])/(pwr[fi+1]-pwr[fi])
		npar[j+2]=interpolate(s.wnpar[*,i],fid)
	endif else npar[j+2]=npar[0]
endfor

npar0=npar[0]	; central ray at 0.5 power

st=moment(npar)
npar_mean=st[0]	; mean value of n_parallel
npar_dev=sqrt(st[1])	; std deviation from mean

np=npar[sort(npar)]
nnp=n_elements(npar)

; take n_parallel limits that include 90% of the power => 87% of power
if nnp eq 3 then begin	; one ray case
	n1=np[0]
	n2=np[2]
endif else begin
	n1=np[fix(nnp*0.05)]
	n2=np[fix(nnp*0.95)]
endelse

if n1 gt n2 then begin
	npar_max = n1
	npar_min = n2
endif else begin
	npar_max = n2
	npar_min = n1
endelse

done:
end

;s=readnc('toray.nc')
;npar_spread, s, npar0, npar_min, npar_max, npar_mean, npar_dev
;print, npar0, npar_min, npar_max, npar_mean, npar_dev
;end
