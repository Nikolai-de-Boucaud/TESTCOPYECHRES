; Procedure for getting polarizer settings that provide a target
; inclination and ellipticity

@$ECHRES_PATH/echcal
@$ECHRES_PATH/find_pol

gyroname=''

ta=55.0 ; antenna mirror tilt angle -
fa=0.	; antenna mirror facet angle - assume normal
shot=long(1000000)	; assume for NEW shots

start:
read, 'Enter gyrotron name (no quotes): ', gyroname
read, 'Enter target inclination and ellipticity (deg): ', alpha_x, beta_x

print, '      Pol1      Pol2      %purity    Sensitivity  Incl.   Ellipt.'

z=echcal(shot, gyroname)
a1=z.Gyro.PolInc*!dtor
a2=z.Gyro.PolEll*!dtor
a3=z.gm1
a4=z.gm2
a5=z.mir_type
a6=z.gamma*!dtor
a7=z.delay*!dtor
a8=alpha_x*!dtor
a9=beta_x*!dtor
a10=fa*!dtor
a11=ta*!dtor
a12=z.Antenna_data.pol_id*!dtor

GetPolSettings, a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,polvalues
print, format='(6F11.3)', polvalues
print, ''
goto, start
end
