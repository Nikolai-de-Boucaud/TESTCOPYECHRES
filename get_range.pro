function get_range, lower_lim, upper_lim, npoints
npts=long(npoints)
a=float(lower_lim)
b=float(upper_lim)
if npts le 1 then return, a
np=float(npts)
return, (findgen(npts)/(np-1.))*(b - a) + a
end
