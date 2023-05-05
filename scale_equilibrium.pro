; function scale_equilibrium
; Scales Bt and Ip by factor, 
;   keeping q constant but Ip with original sign.
;   Also scales pressure to keep beta constant

function scale_equilibrium, gin, factor
g=gin

; multiply Bt by factor
g.bcentr *= factor
g.fpol   *= factor	; F propto B
g.ffprim *= factor	; dF/dpsi indep of B

; multiply Ip by |factor|
absfactor=abs(factor)
g.cpasma *= absfactor 	; don't change sign Ip
g.ssimag *= absfactor	; keep positive
g.ssibry *= absfactor
g.psirz  *= absfactor

; multiply pressure by factor^2
g.pres   *= (absfactor*absfactor)
g.pprime *= absfactor	; P propto B^2, psi propto B

return, g

end

