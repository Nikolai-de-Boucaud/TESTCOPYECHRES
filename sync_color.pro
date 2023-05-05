FUNCTION sync_color,num
; 04-29-98 
; Synchronize the color among various plot pages of the efit_viewer.
; The color is decided by how many slices are requsted.
;
; 10-22-99 Q.Peng starts the second curve in color magenta and moves backwords
;		  in the color table for the subsequent curves.
;
; num - number of curves
;

  case num of
     0:color = color_index('Background')		   ; no curve
     1:color = color_index('Foreground')		   ; 1st curve	
  else:color = (!D.N_Colors + color_index('Magenta')-num+2) mod !D.N_Colors
; else:color = (color_index('Blue')+num-2) mod !D.N_Colors ; force 2nd be blue
  endcase

  return,color
END ; sync_color
