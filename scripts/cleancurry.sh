#!/bin/sh
# delete all auxiliary files of all or selected Curry programs in a directory
#
# Assumptions:
# - The actual Curry system (REPL) is available at `../bin/curry`
# - The call `../bin/curry --compiler-name --numeric-version`
#   returns the name N and version V of the Curry compiler so that
#   auxiliary files are stored in the subdirectory `.curry/N-V/`

SCRIPT=$(readlink -f "$0")
CURRYHOME=$(dirname $(dirname "$SCRIPT"))

HELP=no
VERBOSE=no
case $1 in
  --help   | -h | -\? ) HELP=yes ; shift ;;
  --verbose| -v       ) VERBOSE=yes ; shift ;;
  -*                  ) echo "Unknown option: $1" ; HELP=yes ;;
esac

if [ $HELP = yes ] ; then
  echo "Usage: `basename $SCRIPT` [-h|-v] [<prog> <prog> ...]" >&2
  echo "-h, --help   : show this information and quit" >&2
  echo "-v, --verbose: show executed remove commands" >&2
  echo "<prog>: remove only auxiliary files for Curry program <prog>" >&2
  exit 1
fi

# remove a given directory if it is empty:
remove_empty_dir() {
  DIR="$1"
  if [ -d "$DIR" ] ; then
    DIRFILES=`ls -A $DIR`
    if [ -z "$DIRFILES" ] ; then
      rmdir $DIR
    fi
  fi
}

exec_rm_command() {
  if [ $VERBOSE = yes ] ; then
    echo REMOVE: $*
  fi
  /bin/rm -rf $*
}
  
CURRYCMD="$CURRYHOME/bin/curry --compiler-name --numeric-version"
CURRYOUT=`$CURRYCMD`
if [ $? != 0 ] ; then
  echo "ERROR: cannot determine compiler name/version by command:" >&2
  echo "$CURRYCMD" >&2
  exit 1
fi
COMPILERVERSION=`echo $CURRYOUT | tr ' ' '-'`
# echo COMPILERVERSION: $COMPILERVERSION

# remove all auxiliary Curry files in the current directory:
if [ $# = 0 ] ; then
  exec_rm_command .curry/$COMPILERVERSION
  remove_empty_dir .curry
  exit
fi

# remove auxiliary files of given Curry programs:
for F in $* ; do
  if [ "$F" != "*.curry" -a "$F" != "*.lcurry" ] ; then
    F=`expr $F : '\(.*\)\.lcurry' \| $F`
    F=`expr $F : '\(.*\)\.curry' \| $F`
    FDIR=`dirname $F`
    FBASE=`basename $F | tr '.' '/'`
    CURRYDIR=$FDIR/.curry
    COMPILERDIR="$CURRYDIR/$COMPILERVERSION"
    if [ -d $COMPILERDIR ] ; then
      CURRYF=$COMPILERDIR/$FBASE
      exec_rm_command $CURRYF.*
      remove_empty_dir $COMPILERDIR
    fi
    remove_empty_dir $CURRYDIR
  fi
done
