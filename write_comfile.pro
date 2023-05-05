; write_comfile.pro
; Writes the command file for running toray on a remote platform

function write_comfile, sys_num, srcdir, username, host_machine, $
	cqldat=cqldat

openw, unit, 'comfile', /get_lun

printf, unit, 'cd ~/echres'
printf, unit, '~/bin/toray > logf'
printf, unit, 'scp -B toray.nc ' + username + '@' + host_machine + $
	':' + srcdir + 'toray.nc_sys' + strtrim(string(sys_num),2) + ' >& /dev/null'
printf, unit, 'scp -B logf ' + username + '@' + host_machine + $
	':' + srcdir + 'logf_sys' + strtrim(string(sys_num),2) + ' >& /dev/null'
if keyword_set(cqldat) then if $
	((strpos(cqldat, 'true') ge 0) or (strpos(cqldat, 'TRUE') ge 0)) then $
	printf, unit, 'scp -B rayec001 ' + username + '@' + host_machine + $
	':' + srcdir + 'rayech_sys' + strtrim(string(sys_num),2) + ' >& /dev/null'
;printf, unit, 'rm gafit.in >& /dev/null'
;printf, unit, 'rm toray.nc echin mhddat toray.in >& /dev/null'
printf, unit, 'mv mhddat mhddat_old >& /dev/null'
printf, unit, 'mv echin echin_old >& /dev/null'
printf, unit, 'mv gafit.in gafit.in_old >& /dev/null'

close, unit
free_lun, unit
return, {success:1}
end
