PRO plot_single_window, data, color
Rcent=172  		 ;D3D machine R0 in cm
z=data.z
R=data.R
;CALC. POLOIDAL ANGLE
theta=atan(z,R-Rcent)*180./!pi
IF color EQ 0 THEN BEGIN
	oplot,data.phi,theta
ENDIF ELSE BEGIN
	oplot,data.phi,theta, color=color
ENDELSE

END

PRO plot_d3d_wall
Common ECH_PORTS, RP1_240, RP1_255, RP1_270, RP1_285, RP2_300

;d3d ports/windows data in deg and cm
restore,'$ECHRES_PATH/d3dwall.dat'
;;;different colors for different EC sensitive windows
col=255    ;or set to -1 for white
col0=145  ;green - low risk
col1=195  ;yellow - medium risk or 200 orange color
col2=254  ;red - high risk
;it is decided to keep red only for ECH launcher port by ECPWG Dec 2021


;plot,findgen(10),findgen(10),/nodata, xr=[180,360], yr=[-100,100]
;plot,wall_phi,wall_th,col=30,psym=3
;;;R0 windows
;plot_single_window, R0_180, col
plot_single_window, R0_195, col
plot_single_window, R0_210, col
plot_single_window, R0_225, col
plot_single_window, R0_240, col
plot_single_window, R0_255, col
plot_single_window, R0_270, col
plot_single_window, R0_285, col
plot_single_window, R0_330, col
plot_single_window, R0_345, col
;plot_single_window, R0_360, col
;;;R-1 windows
;plot_single_window, RM1_180, col
plot_single_window, RM1_195, col
plot_single_window, RM1_210, col
plot_single_window, RM1_225, col
plot_single_window, RM1_240, col
plot_single_window, RM1_255, col
plot_single_window, RM1_270, col
plot_single_window, RM1_285, col
plot_single_window, RM1_300, col
plot_single_window, RM1_315, col
plot_single_window, RM1_330, col
;plot_single_window, RM1_360, col
;;;R+1 windows
;plot_single_window, RP1_180, col
plot_single_window, RP1_195, col
plot_single_window, RP1_210, col
plot_single_window, RP1_225, col
plot_single_window, RP1_240, col2
plot_single_window, RP1_255, col2
plot_single_window, RP1_270, col2
plot_single_window, RP1_285, col2
plot_single_window, RP1_300, col
plot_single_window, RP1_315, col
plot_single_window, RP1_330, col
;plot_single_window, RP1_360, col
;;;R-2 windows
plot_single_window, RM2_225, col
plot_single_window, RM2_240, col
plot_single_window, RM2_285, col
plot_single_window, RM2_315, col
;;;R+2 windows
plot_single_window, RP2_195, col
plot_single_window, RP2_225, col
plot_single_window, RP2_240, col
plot_single_window, RP2_270, col
plot_single_window, RP2_285, col
plot_single_window, RP2_300, col2; 300degree top launch
plot_single_window, RP2_360, col
;;;V+1 windows
plot_single_window, VP1_60, col
plot_single_window, VP1_120, col
plot_single_window, VP1_150, col
plot_single_window, VP1_195, col
plot_single_window, VP1_240, col
plot_single_window, VP1_285, col

END
