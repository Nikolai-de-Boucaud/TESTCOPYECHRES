#! /bin/csh -f

# do not want to be affected by user's IDL startup
unsetenv IDL_STARTUP

# backup old version (embedding date in filename)
mv cw_efitpick.compile cw_efitpick.compile_`date | tr ' ' '_'`

# set environment variables automatically based on current directory
pushd .. >&/dev/null
#setenv IDLSOURCE `pwd`
popd >&/dev/null

# complain if MDSROOT is not defined
if (! $?MDSROOT) then
  echo Please ensure that the MDSROOT environment variable is set to the
  echo top level directory of your MDSplus installation.
  echo For example, /usr/local/mdsplus
  exit
endif

# run cw_efitpick_INSTALL.pro
idl cw_efitpick_INSTALL

chmod 664 cw_efitpick.compile

exit
