#!/bin/sh
#
# Main script to run the components of PAKCS

# Define the main directory where PAKCS is installed:
PAKCSHOME=`echo PAKCSHOME must be defined here!`
export PAKCSHOME

# Load the definition of SICSTUS and others specifying auxiliary tools:
. "$PAKCSHOME/bin/.pakcs_variables"

# Define load path for standard libraries provided by PAKCS:
# (used by the various programming tools to search for modules if they
# are not found elsewhere)
PAKCSLIBPATH=$PAKCSHOME/lib:$PAKCSHOME/lib/meta
export PAKCSLIBPATH

# The name of the currently called program:
progname=`basename "$0"`

# add $SICSTUSDIR/bin to path so that command "sicstus..." becomes executable:
if [ -n "$SICSTUSDIR" ] ; then
  PATH=$SICSTUSDIR/bin:$PATH
  export PATH
fi

if [ $progname = pakcs -o $progname = curry2prolog ] ; then
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

elif [ $progname = parsecurry ] ; then
  # start the Curry front end:
  exec "$PAKCSHOME/bin/.parsecurry" ${1+"$@"}

elif [ $progname = sicstusprolog -o $progname = sicstus ] ; then
  # call current version of SICStus-Prolog
  if [ -z "$SICSTUSDIR" ] ; then
    echo "ERROR: Variable SICSTUSDIR undefined in PAKCS, can't execute 'sicstusprolog'!" >&2
    exit 1
  else
    exec "$SICSTUSDIR/bin/sicstus" ${1+"$@"}
  fi

elif [ $progname = swiprolog ] ; then
  # call current version of SWI-Prolog
  if [ -z "$SWIPROLOG" ] ; then
    echo "ERROR: Variable SWIPROLOG undefined in PAKCS, can't execute 'swiprolog'!" >&2
    exit 1
  else
    exec "$SWIPROLOG" ${1+"$@"}
  fi

else
  echo "Error: unknown program '$progname'"
  exit 1
fi

