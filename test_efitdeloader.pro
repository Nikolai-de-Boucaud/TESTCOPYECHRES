filename='/p/d2/disrupt/hyatt/asdexshape/mahdavi_cases/cases_slice_files/a122492.01333'
.comp ../data/is_numeric.pro
.comp efitdeloader.pro
.comp efitde_read.pro
.comp efitde_write.pro
.comp efitde_read_r.pro
.comp mdsconnect.pro
.comp mdsopen.pro
.comp mdsclose.pro
print,'TEST 1: loading A-Files...'
efitdeloader, filename, /setup, /debug
print,'TEST 2: loading G-Files...'
filename='/p/d2/disrupt/hyatt/asdexshape/mahdavi_cases/cases_slice_files/g996740.03400'
efitdeloader, filename, /setup, /debug
