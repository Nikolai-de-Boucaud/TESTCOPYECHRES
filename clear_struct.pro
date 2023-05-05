; pro clear_struct, input_struct
; recursively clears the input structure of values
; HOWEVER, does nothing if s is a substructure; that is,
;	works only on the top level structure

pro clear_struct, s, not_recursive=not_recursive

if not keyword_set(not_recursive) then not_recursive=0

if size(s, /type) ne 8 then return	; it's not a structure

tgs=tag_names(s)
for i=0, n_elements(tgs)-1 do begin
	tp=size(s.(i), /type)
	case tp of
		1: s.(i)[*]=byte(0)
		2: s.(i)[*]=fix(0)
		3: s.(i)[*]=long(0)
		4: s.(i)[*]=float(0)
		5: s.(i)[*]=double(0)
		6: s.(i)[*]=complex(0,0)
		7: s.(i)[*]=''
		8: begin
			if not not_recursive then begin
				dummy=s.(i)
				clear_struct, dummy
				s.(i)=dummy
			endif
		end
		9: s.(i)[*]=dcomplex(0,0)
		else:
	endcase
endfor

end
