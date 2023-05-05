#!/bin/tcsh
#
# this script will copy the input aiming file from echres to the
# the d3share area for timcon setup files of the user running echres.

if($1 == "") then
	echo "usage: "$0" filename"
	exit
endif


if(! -e $1) then
	echo "Could not open input aiming file: "$1
	exit
endif

if(! -e /fusion/d3d/d3share/timcon/setup_files/$user) then
	echo "Error sending aiming file to timcon."
	echo $user" is not an authorized timcon user."
	exit
endif

echo "Sending aiming file: "$1" to timcon for user: "$user

#set fname = `basename $1`
#echo $fname

cp -f $1 /fusion/d3d/d3share/timcon/setup_files/$user/

echo "Send to timcon complete."
