#!/bin/sh
#
# Start interactive read-eval-print loop of PAKCS

PAKCSBUILDDIR=`echo PAKCSBUILDDIR must be defined here!`
PAKCSINSTALLDIR=
# Define the main directory where PAKCS is installed:
if [ -d "$PAKCSINSTALLDIR" ] ; then
  PAKCSHOME=$PAKCSINSTALLDIR
else
  PAKCSHOME=$PAKCSBUILDDIR
fi
export PAKCSHOME

# check whether first argument is a tool and, if yes, exec the tool
PAKCSTOOL="$PAKCSHOME/bin/pakcs-"$1
if [ -x "$PAKCSTOOL" ] ; then
  shift
  exec "$PAKCSTOOL" ${1+"$@"}
fi

# Add PAKCS bin directory to path so that currypp can be found:
PATH=$PATH:$PAKCSHOME/bin
export PATH

REPL="$PAKCSHOME/curry2prolog/pakcs"
if [ ! -x "$REPL" ] ; then
  echo "ERROR: executable '$REPL' not found!" >&2
  echo "Run: cd $PAKCSHOME && make" >&2
  exit 1
fi

# use readline wrapper rlwrap for SICStus-Prolog back end
# if rlwrap exists, we have tty as stdin, and we have a home directory to
# store rlwrap's history:
USERLWRAP=no
if tty -s ; then
  RLWRAP=`which rlwrap`
  if [ -f "$PAKCSHOME/bin/sicstusprolog" -a -x "$RLWRAP" -a -d "$HOME" ] ; then
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
