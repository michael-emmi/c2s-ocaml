#!/bin/bash

notice=NOTICE
src=$1

function ml-header () {
	echo "(***"
	cat $notice | sed "s/^/ * /"
	echo " *)"
}

function c-header () {
	echo "/***"
	cat $notice | sed "s/^/ * /"
	echo " */"
}

if [[ ! -f $src ]]
then
	echo "Cannot find file '$src'"
	exit -1
fi

case $src in
	*.ml | *.mli | *.mll ) ml-header ;;
	*.mly ) c-header ;;
	* ) ;;
esac
echo
cat $src

