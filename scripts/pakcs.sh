#!/bin/sh
#
# Main script to run the components of PAKCS

# Define the main directory where PAKCS is installed:
PAKCSHOME=`echo PAKCSHOME must be defined here!`
export PAKCSHOME

# start the Curry->Prolog compiler:
# use readline wrapper rlwrap if it is installed and we have tty as stdin:
USERLWRAP=no
if tty -s ; then
  RLWRAP=`which rlwrap`
  if [ -x "$RLWRAP" ] ; then
    USERLWRAP=yes
  fi
fi

for i in $* ; do
  if [ $i = "--noreadline" ] ; then
    USERLWRAP=no
  fi
done

if [ $USERLWRAP = yes ] ; then
  exec rlwrap -a -c -f "$PAKCSHOME/tools/rlwrap" "$PAKCSHOME/curry2prolog/c2p.state" ${1+"$@"}
else
  exec "$PAKCSHOME/curry2prolog/c2p.state" ${1+"$@"}
fi
