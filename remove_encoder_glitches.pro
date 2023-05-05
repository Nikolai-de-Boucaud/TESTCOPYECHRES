; function to delete encoder count discontinuities
; t argument is time (msec)
; cts argument is counts(t) from the encoder signal like GYSMIR240L
; keyword threshold_counts is number of counts between two points that
;	could be considered a jump; default is 25 counts
; keyword threshold_dt is the time for which the jump is maintained; if more
; 	than threshold_dt it may not be a jump. Default is 5 msec. Typical jumps 
;	are about 2 msec long.



pro remove_encoder_glitches, t, cts, $
	threshold_count_jump=threshold_count_jump, threshold_dt=threshold_dt

if not keyword_set(threshold_count_jump) then threshold_count_jump=25
if not keyword_set(threshold_dt) then threshold_dt=5.0

dts=cts-shift(cts,1)
i=where(abs(dts[1:n_elements(dts)-2]) gt threshold_count_jump, ndcts)+1
if ndcts mod 2 eq 1 then begin	; it odd number jumps, delete the last one
	ndcts--
	i=i[0:ndcts-1]
endif
if ndcts ge 2 then for j=0, ndcts-1, 2 do $
	if (t[i[j+1]]-t[i[j]]) le threshold_dt then $	; test duration of jump
		cts[i[j]:i[j+1]]=round(interpp([t[i[j]-1],t[i[j+1]+1]], [cts[i[j]-1], $
			cts[i[j+1]]+1], t[i[j]:i[j+1]]))

return

end

; main, for testing
;gadat, time, counts, 'GYSMIR240L', 149708L
;counts_orig=counts
;remove_encoder_glitches, time, counts, threshold_count_jump=10, threshold_dt=5.
;plot, time, counts, /ynozero, xr=[4600,4700], psym=1
;oplot, time, counts_orig
;end
