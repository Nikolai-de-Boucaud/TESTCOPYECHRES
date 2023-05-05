function index, mode_type, k, y, theta
; k is w_pe^2/w^2; y is W_ce/w; theta is angle in deg between B and k
; mode_type= number >0 for O-mode, <0 for X-mode
; or mode_type='x' or 'X' for x-mode, 'o' or 'O' for o-mode
; Returns the cold plasma index and polarization

sz=size(mode_type)
if sz[1] eq 7 then begin
	if strupcase(mode_type) eq 'X' then mode=-1 else $
		if strupcase(mode_type) eq 'O' then mode=1 else return, {success:0}
	endif 
if mode gt 0 then mode=1 else mode=-1 

if k lt 0.0005 then k=0.0005
if abs(90.-abs(theta)) lt 0.1 then theta=89.9
th=theta*!DTOR
cos=cos(th) & cos2=cos^2 & cos4=cos2^2
sin=sin(th) & sin2=sin^2 & sin4=sin2^2

; Stix formulation of index
PS=1-k
RS=(PS-y)/(1.-y)
LS=(PS+y)/(1.+y)
SS=(RS+LS)/2.
DS=(RS-LS)/2.

; Appleton-Hartree formulation of index
y2=y^2
rho=sqrt(sin4 + 4.*(1.-k)^2*cos2/y2)
n2x=1.-2.*k*(1.-k)/(2.*(1.-k)-y2*(sin2+rho))
n2o=1.-2.*k*(1.-k)/(2.*(1.-k)-y2*(sin2-rho))
if mode eq 1 then n2=n2o else n2=n2x
if n2 ge 0. then n=sqrt(n2) else n=0.

; from Stix Eq.28
g=DS/(n2-SS)	; second line
h=n2*cos*sin/(PS-n2*sin2)	; third line
ezr=sqrt(h^2/(1.+h^2+g^2))
exr=-ezr/h
eyi=g*exr

; from Stix Eq.57
er=sqrt((exr+eyi)^2/2.)
el=sqrt((exr-eyi)^2/2.)
ez=sqrt(ezr^2)

return, {success:1, $
	n2:n2, $
	n:sqrt(n2), $
	n_par:sqrt(n2*cos2), $
	n_perp:sqrt(n2*sin2), $
	er:er, el:el, ez:ez, $
	exr:exr, eyi:eyi, ezr:ezr}

done:
return, {success:0}
end

