restore,'efit_viewer.compile'
mdsconnect,'atlas'
anames=mds_efit_labels('a',-1,'efit01')
save,anames,filename='anames.sav'
exit
