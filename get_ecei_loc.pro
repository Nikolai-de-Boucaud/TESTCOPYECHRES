function sinterpol,v,x,u,sortt=sortt,_extra=_extra

   if n_elements(sortt) lt 1 then sortt=0
   if sortt then begin
      ind=sort(X)
   endif else begin
      ind=lindgen(n_elements(x))
   endelse
   return,interpol(v[ind],x[ind],u,_extra=_extra)

end


function interpolatexy,x,y,z,xu,yu,_extra=_extra

   ;takes in vectors of x and y coordinates for z(x,y) data set
   ;interpolates z onto (xu,yu) coordinates has same keywords as 
   ;interpolate
   indx=sinterpol(indgen(n_elements(x)),x,xu,sortt=1)
   indy=sinterpol(indgen(n_elements(y)),y,yu,sortt=1)
   int=interpolate(z,indx,indy,_extra=_extra)
   return,int

end


function get_ecei_loc,g,freqsin,slopsin=slopsin,zintin=zintin,$
                      plott=plott

   if n_elements(plott) lt 1 then plott=0

   ;z_ecei = slopesin*R_ecei + zintin
   ;assuming refraction small and sightlines satisfy some line always
   if n_elements(slopsin) le 1 then begin
      slopsin = fltarr(20)  ;input slope for nz sightlines
   endif

   if n_elements(zintin) le 1 then begin
      zintin=findgen(20)*0.01-0.1  ;input zint for nz sightlines (m)
   endif

   calculate_bfieldsg,bp,br,bt,bz,g
   befit=abs(br^2.+bt^2.+bz^2.)^.5
   fcefit=2.7992E6*befit*1.E4*2.*1.E-9

   nfreq=n_elements(freqsin)
   nz=n_elements(slopsin)
 
   Recei=fltarr(nfreq,nz)
   zecei=Recei 
 
   for j=0,nz-1 do begin
      drtemp=0.01
      ndrtemp=(2.4-0.9)/drtemp
      rtemp=findgen(ndrtemp)*drtemp+0.9
      ztemp=zintin(j)+slopsin(j)*rtemp
      fcefits=interpolatexy(g.r,g.z,fcefit,rtemp,ztemp)
      recei(*,j)=interpol(rtemp,fcefits,freqsin )
      zecei(*,j)=interpol(ztemp,fcefits,freqsin )
   endfor
 
   if plott gt 0 then begin
      plot, g.lim(0,*),g.lim(1,*), yrange=[-1.4,1.4], xrange=[1., 2.4],$
            charsize=cz,xtitle='R (m)',font=0,ytitle='z (m)',/iso,thick=tt,ystyle=1,xstyle=1
      contour,(g.psirz-min(g.psirz))/(g.ssibry-min(g.psirz)),g.r,g.z,levels=findgen(11)/10,/overplot
      wg=where(g.bdry(0,*) gt 1.)
      oplot,g.bdry(0,wg),g.bdry(1,wg)
      oplot,recei,zecei,psym=4,symsize=.2 
   endif
 
   outs={recei:recei,zecei:zecei}
 
   return,outs
 
 end
