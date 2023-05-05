;**************************************************************************
;
;  4dlib/EFITLIB/aplotnames.pro
;
;  CREATED:
;
;   19990622	C. Forest
;		Set labels and order of plotting for the A file stuff 
;		written on the efit plot.
;
;  MODIFIED:
;   20151124    SMF - Added tchimls and twagap.
;   20120725    SMF - Removed order.  It's implied by array readanames.   
;   19980424	Q.Peng - for units consistancy, replace e13 with e19 for nev* 
;			 set factor of V and A to 1 (no need to convert from
;			 cm to m, reada with MKS takes care of the unit.)
;   19980220 	Q.Peng - replace common block with arguments
;   19970712    J. Schachter -- replacing ORDER scheme with explicit 
;		specification of READA tag names
;   19960809    K. Greene -- Add use of function TEMPORARY
;
;
;**************************************************************************

pro aplotnames,s,factor,readaname

s = [	'chi**2', $
	'Rout(m)',$
	'Zout(m)', $
	'a(m)', $
	'elong', $
	'utri', $
	'ltri', $
	'indent']
factor = [1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0]
readaname=['CHISQ', 'RSURF', 'ZSURF', 'AMINOR', 'KAPPA', 'TRITOP', 'TRIBOT', 'INDENT']

s = [s,	'V (m**3)', $
	'A (m**2)', $
	'W (MJ)', $
	'betaT(%)', $
	'betaP', $
	'betaN', $
	'In']
factor = [factor,1.0,1.0,1.0e-6,1.0,1.0,1.0,1.0]
readaname=[readaname,'VOLUME', 'AREA', 'WMHD', 'BETAT', 'BETAP', 'BETAN', 'IN']

s = [s,	'Li', $
        'Li3', $
	'error(e-4)', $
	'q1', $
	'q95', $
	'dsep(m)', $
        'Rm(m)', $
	'Zm(m)', $
	'Rc(m)', $
	'Zc(m)']
factor = [factor,1.0,1.0,1.0e4,1.0,1.0,1.0,1.0,1.0,1.0,1.0]
readaname=[readaname, 'LI', 'LI3', 'EPS', 'QL', 'Q95', 'SEPLIM', 'RM', 'ZM', 'RCUR', 'ZCUR']

s = [s,	'betaPd', $
	'betaTd', $
	'Wdia(MJ)', $
	'Ipmeas(MA)', $
	'BT(0)(T)', $
	'Ipfit(MA)']
factor = [factor,1.0,1.0,1.0e-6,1.0e-6,1.0,1.0e-6]
readaname=[readaname, 'BETAPD', 'BETATD', 'WDIA', 'IPMEAS', 'BCENTR', 'IPMHD']

s = [s,	'Rmidin(m)',$
	'Rmidout(m)',$
	'gapin(m)', $
	'gapout(m)', $
	'gaptop(m)', $
	'gapbot(m)', $
	'Zts(m)']
factor = [factor,1.0,1.0,1.0,1.0,1.0,1.0,1.0]
readaname=[readaname, 'RMIDIN', 'RMIDOUT', 'GAPIN', 'GAPOUT', 'GAPTOP', 'GAPBOT', 'ZUPERTS']

s = [s,	'Rvsin(m)', $
	'Zvsin(m)', $
	'Rvsout(m)', $
	'Zvsout(m)',$
        'Rsep1(m)', $
	'Zsep1(m)', $
	'Rsep2(m)', $
	'Zsep2(m)']
factor=[factor,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0]
readaname=[readaname, 'RVSIN', 'ZVSIN', 'RVSOUT', 'ZVSOUT', 'RXPT1', 'ZXPT1', 'RXPT2', 'ZXPT2']

s = [s,	'psib(Vs/R)', $
	'elongm', $
	'qm', $
	'nev1(e19)', $
	'nev2(e19)', $
	'nev3(e19)',$
        'ner0(e19)', $
	'n/nc', $
	'dRsep', $
	'tflux' ]
factor = [factor,1.0,1.0,1.0,1.0e-19,1.0e-19,1.0e-19,1.0e-19,1.0,1.0,1.0]
readaname=[readaname,'PSIBDY', 'ELONGM', 'QM', 'CO2D1', 'DENSV2', 'CO2D3', 'CO2DR', 'XNNC', 'SSEP','TFLUX']

s = [s, 'tchimls', $
        'twagap(cm)' ]
factor = [factor,1.0,1.0]
readaname=[readaname,'TCHIMLS','TWAGAP']

s = [s, 'qmin', $
	'rhoqmin', $
	'sqfid', $
	'sqfiu', $
	'sqfod', $
	'sqfou', $
	'sqlid', $
	'sqliu', $
	'sqlod', $
	'sqlou' ]
factor = [factor,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0]
readaname=[readaname,'QMIN', 'RHOQMIN', 'SQFID', 'SQFIU', 'SQFOD', 'SQFOU', 'SQLID', 'SQLIU', 'SQLOD', 'SQLOU' ]

return
end
