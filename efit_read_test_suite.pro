cd,'~/tests/efit_tests'

;;;;;;; compiles first (so don't clutter output messages)
@efit_read_modules

;;;;;;; FILE TESTS:

print,'****** should get 96021, 3600 from file:'
d = reada(96021,3600,verbose=verbose,debug=debug) 
print,'reada: ',d.shot,d.time,d.error,d.source
d = readg(96021,3600,verbose=verbose,debug=debug) 
print,'readg: ',d.shot,d.time,d.error,d.source
d = readm(96021,3600,verbose=verbose,debug=debug) 
print,'readm: ',d.shot,d.time,d.error,d.source
print,''

print,'****** Asking for 3601, should get 3600 from 96021: '
d = reada(96021,3601,verbose=verbose,debug=debug) 
print,'reada: ',d.shot,d.time,d.error,d.source
d = readg(96021,3601,verbose=verbose,debug=debug)
print,'readg: ',d.shot,d.time,d.error,d.source
d = readm(96021,3601,verbose=verbose,debug=debug)
print,'readm: ',d.shot,d.time,d.error,d.source
print,''

print,'****** Asking for 3601, should not get anything'
d = reada(96021,3601,/exact_time,verbose=verbose,debug=debug) 
print,'a.error: ',d.error,d.source
d = readg(96021,3601,/exact_time,verbose=verbose,debug=debug) 
print,'g.error: ',d.error,d.source
d = readm(96021,3601,/exact_time,verbose=verbose,debug=debug) 
print,'m.error: ',d.error,d.source
print,''

print,'****** Asking for 3601, should get 3650 from 96021: '
d = reada(96021,3601,verbose=verbose,debug=debug,time_range=[0,100]) 
print,'reada: ',d.shot,d.time,d.error,d.source
d = readg(96021,3601,verbose=verbose,debug=debug,time_range=[0,100]) 
print,'readg: ',d.shot,d.time,d.error,d.source
d = readm(96021,3601,verbose=verbose,debug=debug,time_range=[0,100]) 
print,'readm: ',d.shot,d.time,d.error,d.source
print,''


print,'****** Asking for 95021, mode file.  Should not get anything'
d = reada(95021,3600,mode='FILE',verbose=verbose,debug=debug)
print,'a.error: ',d.error
d = readg(95021,3600,mode='FILE',verbose=verbose,debug=debug)
print,'g.error: ',d.error
d = readm(95021,3600,mode='FILE',verbose=verbose,debug=debug)
print,'m.error: ',d.error
print,''

print,'****** Testing units.  First should be in MKS, second in CGS'
a = reada(96021,3600,/MKS,verbose=verbose,debug=debug)
print,'Rmagx: ',a.d.rm
a = reada(96021,3600,verbose=verbose,debug=debug)
print,'Rmagx: ',a.d.rm
print,''

print,'****** Testing explicit filename'
d = reada('/u2/schacht/tests/efit_tests/a096021.03600',verbose=verbose,debug=debug)
print,'reada: ',d.shot,d.time,d.error,d.source
d = readg('/u2/schacht/tests/efit_tests/g094000.02100',verbose=verbose,debug=debug)
print,'readg: ',d.shot,d.time,d.error,d.source
d = readm('/u2/schacht/tests/efit_tests/m096021.03600',verbose=verbose,debug=debug)
print,'readm: ',d.shot,d.time,d.error,d.source
print,''

print,'****** Testing explicit filename, time different - should not get anything'
d = reada('/u2/schacht/tests/efit_tests/a096021.03601',verbose=verbose,debug=debug)
print,'a.error: ',d.error
d = readg('/u2/schacht/tests/efit_tests/g094000.02101',verbose=verbose,debug=debug)
print,'g.error: ',d.error
d = readm('/u2/schacht/tests/efit_tests/m096021.03601',verbose=verbose,debug=debug)
print,'m.error: ',d.error
print,''

print,'****** Testing Unformatted file reads'
d = reada('/u/schaffer/eqdsks/shot087522/a087522.01437',verbose=verbose,debug=debug)
print,'reada: ',d.shot,d.time,d.error,d.source
d = readg('/u/schaffer/eqdsks/shot087522/g087522.01437',verbose=verbose,debug=debug)
print,'readg: ',d.shot,d.time,d.error,d.source
print,''

;;;;;;; MDSPLUS TESTS:

print,'****** Should get 95021, 3600 from MDSplus - EFIT01'
d = reada(95021,3600,verbose=verbose,debug=debug) 
print,'reada: ',d.shot,d.time,d.error,d.source
d = readg(95021,3600,verbose=verbose,debug=debug) 
print,'readg: ',d.shot,d.time,d.error,d.source
d = readm(95021,3600,verbose=verbose,debug=debug) 
print,'readm: ',d.shot,d.time,d.error,d.source
print,''

print,'****** Should get 96021, 3600 from MDSplus - EFIT01'
d = reada(96021,3600,mode='MDSPLUS',verbose=verbose,debug=debug) 
print,'reada: ',d.shot,d.time,d.error,d.source
d = readg(96021,3600,mode='MDSPLUS',verbose=verbose,debug=debug) 
print,'readg: ',d.shot,d.time,d.error,d.source
d = readm(96021,3600,mode='MDSPLUS',verbose=verbose,debug=debug) 
print,'readm: ',d.shot,d.time,d.error,d.source
print,''

print,'****** Asking for 3601.  Should get 95021, 3600 from MDSplus - EFIT01'
d = reada(95021,3601,verbose=verbose,debug=debug) 
print,'reada: ',d.shot,d.time,d.error,d.source
d = readg(95021,3601,verbose=verbose,debug=debug) 
print,'readg: ',d.shot,d.time,d.error,d.source
d = readm(95021,3601,verbose=verbose,debug=debug) 
print,'readm: ',d.shot,d.time,d.error,d.source
print,''

print,'****** Asking for 3601.  Should get nothing (exact_time set)'
d = reada(95021,3601,/EXACT_TIME,verbose=verbose,debug=debug)
print,'a.error: ',d.error
d = readg(95021,3601,/EXACT_TIME,verbose=verbose,debug=debug)
print,'g.error: ',d.error
d = readm(95021,3601,/EXACT_TIME,verbose=verbose,debug=debug)
print,'m.error: ',d.error
print,''

print,'****** Testing UNITS... first in MKS, then in CGS'
a = reada(95021,3600,/MKS,verbose=verbose,debug=debug)
print,'Rmagx: ',a.d.rm
a = reada(95021,3600,verbose=verbose,debug=debug)
print,'Rmagx: ',a.d.rm
print,''

print,'****** Should get nothing - asking for 111111 time 11111'
d = reada(111111,1111,verbose=verbose,debug=debug)
print,'a.error: ',d.error
d = readg(111111,1111,verbose=verbose,debug=debug)
print,'g.error: ',d.error
d = readm(111111,1111,verbose=verbose,debug=debug)
print,'m.error: ',d.error
print,''

print,'****** Should get 95021, 3600 from MDSplus - EFIT02'
d = reada(95021,3600,verbose=verbose,debug=debug,runid='EFIT02') 
print,'reada: ',d.shot,d.time,d.error,d.source
d = readg(95021,3600,verbose=verbose,debug=debug,runid='EFIT02') 
print,'readg: ',d.shot,d.time,d.error,d.source
d = readm(95021,3600,verbose=verbose,debug=debug,runid='EFIT02') 
print,'readm: ',d.shot,d.time,d.error,d.source
print,''

