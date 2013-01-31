#!/bin/sh
#
# Start interactive read-eval-print loop of PAKCS

# Define the main directory where PAKCS is installed:
PAKCSHOME=`echo PAKCSHOME must be defined here!`
export PAKCSHOME

REPL="$PAKCSHOME/curry2prolog/pakcs"
if [ ! -x "$REPL" ] ; then
  echo "ERROR: executable '$REPL' not found!" >&2
  echo "Run: cd $PAKCSHOME && make" >&2
  exit 1
fi

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
  exec rlwrap -c -f "$PAKCSHOME/tools/rlwrap" "$REPL" ${1+"$@"}
else
  exec "$REPL" ${1+"$@"}
fi
