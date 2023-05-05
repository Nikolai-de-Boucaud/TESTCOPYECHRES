file1='/p/d2/disrupt/hyatt/asdexshape/mahdavi_cases/cases_slice_files/a122492.01333'
file2='/p/d2/disrupt/hyatt/asdexshape/mahdavi_cases/cases_slice_files/g122492.01333'
.comp ../data/is_numeric.pro
.comp efitde_read.pro
.comp efitde_write.pro
.comp efitde_read_r.pro
.comp efitde__define.pro
.comp mdsconnect.pro
.comp mdsopen.pro
.comp mdsclose.pro
print,'TEST 1'
o = obj_new('efitde',/debug)
o->setFile, file1
o->setFile, file2
o->setComment, 'testing efitde__define.pro'
o->load
file1='/p/d2/disrupt/hyatt/asdexshape/mahdavi_cases/cases_slice_files/a122492.01311'
file2='/p/d2/disrupt/hyatt/asdexshape/mahdavi_cases/cases_slice_files/g122492.01311'
obj_destroy,o
print,'TEST 2'
o = obj_new('efitde',file1,file2,/debug)
o->setComment, 'testing efitde__define.pro'
o->load
