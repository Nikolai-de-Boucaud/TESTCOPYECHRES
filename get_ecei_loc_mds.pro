function get_ecei_loc_mds,g, shot, plott=plott 

;shot=169129 ; 
;g=readg(shot,450)
;ECEI sightlines and frequencies are stored by shot number

if n_elements(plott) lt 1 then plott=0
 
hfsgood=0
error=0
lfsgood=0 ;if data are there for this shot, these will be set to one
 outs={r_hfs:[-1],z_hfs:[-1], r_lfs:[-1],z_lfs:[-1],$
 lfsgood:0, hfsgood:0, error:1}
 
;****************************************************

;****************************************************
;finding which configuration was used for the requested shot

gadat,t,hfs_freqs,'hfs_freqs',shot,tree='ECEI'
 
if n_elements(hfs_freqs) eq 1 then begin
   error=1
   print,'GET_ECEI_LOC_MDS: Shot '+strtrim(shot,2)+' outside of stored range'
   goto,jump1
endif
 

;Getting example HFS and LFS pointnames to check and confirm data are there for that shot
gadat,tec,ec0,'HFS0302',shot,xmin=0,xmax=100
gadat,tecl,ec0l,'LFS0302',shot, xmin=0,xmax=100


gadat,t,lfs_freqs,'lfs_freqs',shot,tree='ECEI'
gadat,t,ecei_slopes,'ecei_slopes',shot,tree='ECEI'
gadat,t,ecei_zint,'ecei_zint',shot,tree='ECEI'
 
 
 ;If data are there and the LO freq. is > 0 then these are good
if (n_elements(tec) gt 1) and (hfs_freqs(0) gt 0.) then hfsgood=1
if (n_elements(tecl) gt 1) and (lfs_freqs(0) gt 0.) then lfsgood=1

  
;Getting actual R,z values for HFS and LFS arrays
rz_hfs=get_ecei_loc(g,hfs_freqs,slopsin=ecei_slopes,zintin=ecei_zint,plott=0)
rz_lfs=get_ecei_loc(g,lfs_freqs,slopsin=ecei_slopes,zintin=ecei_zint,plott=0)

;Plotting the two arrays
if plott gt 0 then begin
plot, g.lim(0,*),g.lim(1,*), yrange=[-1.4,1.4], xrange=[1., 2.4],$
charsize=cz,xtitle='R (m)',font=0,ytitle='z (m)',/iso,thick=tt,ystyle=1,xstyle=1

contour,(g.psirz-min(g.psirz))/(g.ssibry-min(g.psirz)),g.r,g.z,levels=findgen(11)/10,/overplot
wg=where(g.bdry(0,*) gt 1.)
oplot,g.bdry(0,wg),g.bdry(1,wg)
 
if lfsgood gt 0 then begin  ;only plot it if data are there
oplot,rz_lfs.recei,rz_lfs.zecei,psym=5,symsize=.2
endif

if hfsgood gt 0 then begin  ;only plot it if data are there
oplot,rz_hfs.recei,rz_hfs.zecei,psym=5,symsize=.2,color=180
endif 
 endif
 
 outs={r_hfs:rz_hfs.recei,z_hfs:rz_hfs.zecei, r_lfs:rz_lfs.recei,z_lfs:rz_lfs.zecei,$
 lfsgood:lfsgood, hfsgood:hfsgood, error:error}
 
jump1:    ;ends up here if the shot was lower than stored in configuration file
 return,outs
 
 end
