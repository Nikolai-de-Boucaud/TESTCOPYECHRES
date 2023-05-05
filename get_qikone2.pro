; get_qikone2
; Same as get_qikone() but automatically selects qikone file if argument
; is absent, so it can run in background mode. Also gets the dimension of the
; radial grid rather than assuming 51.
; Modified to also return the total integrated driven current as
;  element a.totcurdrive

function get_qikone2, file, infile=infile
result=n_params()
if result eq 1 then filename=file else filename='qikone'
if keyword_set(infile) then filename=infile	; keyword overrides argument
a=''
totcurdrive=0.0
name  = findfile(filename, count=count)
if count lt 1 then return, {success:0}

openr, lun, filename, /get_lun
s=''
pos=-1
while ( (pos lt 0) and (not EOF(lun)) ) do begin
	readf, lun, s
	pos=strpos(s, 'kj =')
endwhile
kj = fix(strmid(s, pos+4))

; set up for file read
pslab = 'i        psi            rho            bp           curden           q            fcap           hcap'
pss = fltarr(8,kj)

caplab = 'i        rho           fcap           gcap           hcap       <R0**2/R**2>'
cap=fltarr(6,kj)

eeslab = 'j    r   r/a   1.5*dpe/dt  qconde      qconve      qdelt        qexch      qohm        qione       qrad         omegale'
ees = fltarr(12,kj)

eeslab2 = 'j    r   r/a      qbeame       qrfe         qe2d      qtfuse      qbfuse       qsawe     rhs-lhs      qmag'
ees2 = fltarr(11,kj)

peelab = 'j    r   r/a int 1.5*dpe/dt   pconde   pconve      pdelt       pexch       pohm        pione       prad        pomegale'
pee = fltarr(12,kj-1)

pee2lab = 'j    r   r/a      pbeame       prfe         pe2d      ptfuse      pbfuse       psawe     rhs-lhs     pmag'
pee2 = fltarr(11,kj-1)

ieslab = 'j    r   r/a   1.5*dpi/dt  qcondi      qconvi      qdelt        qexch      qioni         qcx      qomegapi      omegale'
ies = fltarr(12,kj)

ies2lab = 'j    r   r/a   qbeami      qrfi        qi2d       qtfusi      qbfusi       qsawi      rhs-lhs      qbcx'
ies2 = fltarr(11,kj)

pielab = 'j    r   r/a int 1.5*dpi/dt   pcondi   pconvi      pdelt        pexch      pioni       pcx         pomegapi     pomegale'
pie = fltarr(12,kj-1)

pie2lab = 'j    r   r/a     pbeami        prfi         pi2d      ptfusi      pbfusi       psawi     rhs-lhs       pbcx'
pie2 = fltarr(11,kj-1)

nb1lab = 'fast ion power source and fraction deposited in electrons and ions    (beam  1)'
nb1 = fltarr(12,kj)
beam1kv = fltarr(3)
string1 = ' '

nb2lab = 'fast ion power source and fraction deposited in electrons and ions    (beam  2)'
nb2 = fltarr(12,kj)
beam2kv = fltarr(3)
string2 = ' '

trlab = 'j         r       d(1)        chie        chii         xke         xki        xnuse      d -xnus      ftrap     eta'
trco = fltarr(11,kj-1)

diaglab = 'j         r       xkecar       xkeohk       xkepp        xkedom        xki/   shearp       slene        slte       chiwneo'
diag = fltarr(11,kj-1)

conflab = 'j    r   r/a      taue       tauetr     tauitr      tauer       tauir       taupe       tauelc      tres      tangmtmloc'
conf = fltarr(12,kj)

profslab =   'j    r   r/a      zeff       ene        enbeam      enalp      d  ions     c  ions     d  neuts       neuts'  
profs = fltarr(11,kj)

prlab = 'j    r   r/a      te       ti        we          wi        wbeam       walp'
pressure=fltarr(9,kj)

curlab ='j    r   r/a      q      etor       et+esaw     curden      curohm     curboot     curdrive       bpol        psir'
current = fltarr(12,kj)

;READ IN EACH OF THE MATRICES AND EXTRACT IMPORTANT PARAMETERS

while NOT EOF(lun) do begin

readf,lun,a

case  strtrim(a,2) of

   pslab: begin
 	  readf,lun,pss
	  end

   caplab: begin
 	   readf,lun,cap
	   end

   eeslab: begin
           readf,lun,a
           readf,lun,ees
	   end

   eeslab2: begin
            readf,lun,a
            readf,lun,ees2
	    end

   peelab: begin
           readf,lun,a
           readf,lun,pee
           end

   pee2lab: begin
            readf,lun,a
            readf,lun,pee2
            end

   ieslab: begin
	   readf,lun,a
	   readf,lun,ies
           end

   ies2lab: begin
	    readf,lun,a
	    readf,lun,ies2
	    end

   pielab: begin
           readf,lun,a
           readf,lun,pie
           end

   pie2lab: begin
            readf,lun,a
            readf,lun,pie2
            end

   nb1lab: begin
           readf,lun,a
           readf,lun,string1
           readf,lun,a
           readf,lun,nb1
           end

   nb2lab: begin
           readf,lun,a
           readf,lun,string2
           readf,lun,a
           readf,lun,nb2
           end

   trlab: begin
          readf,lun,a
          readf,lun,a
          readf,lun,trco
	  end

   diaglab: begin
            readf,lun,a
            readf,lun,diag
            end

   conflab: begin
            readf,lun,a
            readf,lun,conf
            end

   profslab: begin
             readf,lun,a
             readf,lun,profs
	     end

   prlab: begin
          readf,lun,a
          readf,lun,pressure
	     end

   curlab: begin
           readf,lun,a
           readf,lun,current
		   readf,lun,a
		   readf,lun,a
		   totcurdrive=float(strmid(a,strlen(a)-12,12))	; total current drive
	   end

   else:

endcase

endwhile

close,lun
free_lun, lun

psi     = transpose(pss(1,*))
rho     = transpose(pss(2,*))*100.
bp      = transpose(pss(3,*))
curdens = transpose(pss(4,*))
qsafe   = transpose(pss(5,*))

fcap  = transpose(cap(2,*))
gcap  = transpose(cap(3,*))
hcap  = transpose(cap(4,*))
r02r2 = transpose(cap(5,*))

qdpedt  = transpose(ees(3,*))
qconde  = transpose(ees(4,*))
qconve  = transpose(ees(5,*))
qdelte  = transpose(ees(6,*))
qexche  = transpose(ees(7,*))
qohm    = transpose(ees(8,*))
qione   = transpose(ees(9,*))
qrad    = transpose(ees(10,*))
omegale = transpose(ees(11,*))

qbeame = transpose(ees2(3,*))
qrfe   = transpose(ees2(4,*))
qe2d   = transpose(ees2(5,*))
qtfuse = transpose(ees2(6,*))
qbfuse = transpose(ees2(7,*))
qsawe  = transpose(ees2(8,*))
qmag   = transpose(ees2(10,*))

pdpedt   = transpose(pee(3,*))
pconde   = transpose(pee(4,*))
pconve   = transpose(pee(5,*))
pdelte   = transpose(pee(6,*))
pexche   = transpose(pee(7,*))
pohm     = transpose(pee(8,*))
pione    = transpose(pee(9,*))
prad     = transpose(pee(10,*))
pomegale = transpose(pee(11,*))

pbeame = transpose(pee2(3,*))
prfe   = transpose(pee2(4,*))
pe2d   = transpose(pee2(5,*))
ptfuse = transpose(pee2(6,*))
pbfuse = transpose(pee2(7,*))
psawe  = transpose(pee2(8,*))
pmag   = transpose(pee2(10,*))

qdpidt   = transpose(ies(3,*))
qcondi   = transpose(ies(4,*))
qconvi   = transpose(ies(5,*))
qdelti   = transpose(ies(6,*))
qexchi   = transpose(ies(7,*))
qioni    = transpose(ies(8,*))
qcx      = transpose(ies(9,*))
qomegapi = transpose(ies(10,*))

qbeami = transpose(ies2(3,*))
qrfi   = transpose(ies2(4,*))
qi2d   = transpose(ies2(5,*))
qtfusi = transpose(ies2(6,*))
qbfusi = transpose(ies2(7,*))
qsawi  = transpose(ies2(8,*))
qbcx   = transpose(ies2(10,*))

pdpidt   = transpose(pie(3,*))
pcondi   = transpose(pie(4,*))
pconvi   = transpose(pie(5,*))
pdelti   = transpose(pie(6,*))
pexchi   = transpose(pie(7,*))
pioni    = transpose(pie(8,*))
pcx      = transpose(pie(9,*))
pomegapi = transpose(pie(10,*))

pbeami = transpose(pie2(3,*))
prfi   = transpose(pie2(4,*))
pi2d   = transpose(pie2(5,*))
ptfusi = transpose(pie2(6,*))
pbfusi = transpose(pie2(7,*))
psawi  = transpose(pie2(8,*))
pbcx   = transpose(pie2(10,*))

beam1kv(0) = float(strmid(string1,29,7))
beam1kv(1) = float(strmid(string1,61,7))
beam1kv(2) = float(strmid(string1,93,7))
beam1full   = transpose(nb1(3,*))
beam1half   = transpose(nb1(6,*))
beam1third  = transpose(nb1(9,*))

beam2kv(0) = float(strmid(string2,29,7))
beam2kv(1) = float(strmid(string2,61,7))
beam2kv(2) = float(strmid(string2,93,7))
beam2full   = transpose(nb2(3,*))
beam2half   = transpose(nb2(6,*))
beam2third  = transpose(nb2(9,*))

rhohalf = transpose(trco(1,*))
d1      = transpose(trco(2,*))
chie    = transpose(trco(3,*))
chii    = transpose(trco(4,*))
xke     = transpose(trco(5,*))
xki     = transpose(trco(6,*))
xnuse   = transpose(trco(7,*))
dxnus   = transpose(trco(8,*))
ftrap   = transpose(trco(9,*))
eta     = transpose(trco(10,*))

xkecar    = transpose(diag(2,*))
xkeohk    = transpose(diag(3,*))
xkepp     = transpose(diag(4,*))
xkedom    = transpose(diag(5,*))
xkixkineo = transpose(diag(6,*))
shearp    = transpose(diag(7,*))
slene     = transpose(diag(8,*))
slte      = transpose(diag(9,*))
chiwneo   = transpose(diag(10,*))

taue       = transpose(conf(3,*))
tauetr     = transpose(conf(4,*))
tauitr     = transpose(conf(5,*))
tauer      = transpose(conf(6,*))
tauir      = transpose(conf(7,*))
taupe      = transpose(conf(8,*))
tauelc     = transpose(conf(9,*))
tres       = transpose(conf(10,*))
tangmtmloc = transpose(conf(11,*))

zeff   = transpose(profs(3,*))
ene    = transpose(profs(4,*))
enbeam = transpose(profs(5,*))
enalp  = transpose(profs(6,*))
dions  = transpose(profs(7,*))
cions  = transpose(profs(8,*))
dneuts = transpose(profs(9,*))

te      = transpose(pressure(3,*))
ti      = transpose(pressure(4,*))
we      = transpose(pressure(5,*))
wi      = transpose(pressure(6,*))
wbeam   = transpose(pressure(7,*))
walp    = transpose(pressure(8,*))

q        = transpose(current(3,*))
etor     = transpose(current(4,*))
etsaw    = transpose(current(5,*))
curden   = transpose(current(6,*))
curohm   = transpose(current(7,*))
curboot  = transpose(current(8,*))
curdrive = transpose(current(9,*))
bpol     = transpose(current(10,*))
psir     = transpose(current(11,*))

a = {success:1, grid_size:kj, psi:psi, rho:rho, bp:bp, curdens:curdens, qsafe:qsafe,$
fcap:fcap, gcap:gcap, hcap:hcap, r02r2:r02r2,$
qdpedt:qdpedt, qconde:qconde, qconve:qconve, qdelte:qdelte, qexche:qexche,$
qohm:qohm, qione:qione, qrad:qrad, omegale:omegale, qbeame:qbeame,$
qrfe:qrfe, qe2d:qe2d, qtfuse:qtfuse, qbfuse:qbfuse, qsawe:qsawe, qmag:qmag,$
pdpedt:pdpedt, pconde:pconde, pconve:pconve, pdelte:pdelte, pexche:pexche,$
pohm:pohm, pione:pione, prad:prad, pomegale:pomegale, pbeame:pbeame,$ 
prfe:prfe, pe2d:pe2d, ptfuse:ptfuse, pbfuse:pbfuse, psawe:psawe, pmag:pmag,$
qdpidt:qdpidt, qcondi:qcondi, qconvi:qconvi, qdelti:qdelti, qexchi:qexchi,$
qioni:qioni, qcx:qcx, qomegapi:qomegapi, qbeami:qbeami, qrfi:qrfi, qi2d:qi2d,$
qtfusi:qtfusi, qbfusi:qbfusi, qsawi:qsawi, qbcx:qbcx,$
pdpidt:pdpidt, pcondi:pcondi, pconvi:pconvi, pdelti:pdelti, pexchi:pexchi,$ 
pioni:pioni, pcx:pcx, pomegapi:pomegapi, pbeami:pbeami, prfi:prfi,$
pi2d:pi2d, ptfusi:ptfusi, pbfusi:pbfusi, psawi:psawi, pbcx:pbcx,$
beam1kv:beam1kv, beam1full:beam1full, beam1half:beam1half,$
beam1third:beam1third, beam2kv:beam2kv, beam2full:beam2full,$
beam2half:beam2half, beam2third:beam2third,$
rhohalf:rhohalf, d:d1, chie:chie, chii:chii, xke:xke, xki:xki,$
xnuse:xnuse, dxnus:dxnus, ftrap:ftrap, eta:eta,$
xkecar:xkecar, xkeohk:xkeohk, xkepp:xkepp, xkedom:xkedom,$
xkixkineo:xkixkineo, shearp:shearp, slene:slene, slte:slte, chiwneo:chiwneo,$
taue:taue, tauetr:tauetr, tauitr:tauitr, tauer:tauer, tauir:tauir,$
taupe:taupe, tauelc:tauelc, tres:tres, tangmtmloc:tangmtmloc, zeff:zeff,$
ene:ene, enbeam:enbeam, enalp:enalp, dions:dions, cions:cions, dneuts:dneuts,$
te:te, ti:ti, we:we, wi:wi, wbeam:wbeam, walp:walp,$
q:q, etor:etor, etsaw:etsaw, curden:curden, curohm:curohm,$
curboot:curboot, curdrive:curdrive, bpol:bpol, psir:psir, totcurdrive:totcurdrive, ierr: 0}

return, a

end




