#!/bin/sh

# This is the shell script to invoke the FlatCurry preprocessor
# which applies various transformations on a FlatCurry program

PAKCSBUILDDIR=`echo PAKCSBUILDDIR must be defined here!`
PAKCSINSTALLDIR=
# Define the main directory where PAKCS is installed:
if [ -d "$PAKCSINSTALLDIR" ] ; then
  PAKCSHOME=$PAKCSINSTALLDIR
else
  PAKCSHOME=$PAKCSBUILDDIR
fi
export PAKCSHOME

QUIET=no

while [ $# -gt 1 ] ; do
  case $1 in
    -q | --quiet ) QUIET=yes ;;
    --fcypp      ) shift ; FCYPP="$FCYPP $1 " ;;
    --fpopt | --compact | --compactexport | --compactmain=* ) FCYPP="$FCYPP $1 " ;;
    * ) echo ERROR: Illegal option: "$@" >&2 ; exit 1 ;;
  esac
  shift
done

if [ $# != 1 ] ; then
  echo >&2
  echo "Usage: pakcs-fcypp [<Options>] <progname>" >&2
  echo >&2
  echo "Apply FlatCurry preprocessor transformations on Curry program stored in" >&2
  echo "<progname>.[l]curry" >&2
  echo >&2
  echo "<Options> is a sequence of:"  >&2
  echo "-q|--quiet       : work silently" >&2
  echo "--fpopt          : apply function pattern optimization after parsing" >&2
  echo "--compact        : apply code compactification" >&2
  echo "--compactexport  : apply code compactification w.r.t. exports" >&2
  echo "--compactmain=f  : apply code compactification w.r.t. main function 'f'" >&2
  echo "--fcypp <c>      : apply command <c> to <progname>" >&2
  exit 1
fi
PROG=$1
PROGDIR=`dirname $PROG`
MODNAME=`basename $PROG`

# check existence of source program:
if [ -f "$PROGDIR/$MODNAME.lcurry" ] ; then
  SOURCESUFFIX=.lcurry
elif [ -f "$PROGDIR/$MODNAME.curry" ] ; then
  SOURCESUFFIX=.curry
else
  echo "ERROR: Source program '$PROG.[l]curry' not found!" >&2
  exit 1
fi

# find options in Curry source file:
OPTIONLINE=`grep '^{-#.*PAKCS_OPTION_FCYPP' "$PROGDIR/$MODNAME$SOURCESUFFIX" | head -1`
OPTIONS=`expr "$OPTIONLINE" : '{-#.*PAKCS_OPTION_FCYPP*\(.*\)#-}.*'`
# add source file options to current preprocessing options:
if [ -n "$OPTIONS" ] ; then
  FCYPP="$FCYPP $OPTIONS"
fi

if [ $QUIET = no -a -n "$FCYPP" ] ; then
  echo "Executing FlatCurry preprocessing options: $FCYPP"
fi
for T in $FCYPP ; do
  case $T in
    --fpopt         ) TCMD="$PAKCSHOME/tools/optimize/NonStrictOpt" ;;
    --compact       ) TCMD="$PAKCSHOME/tools/optimize/CompactFlat" ;;
    --compactexport ) TCMD="$PAKCSHOME/tools/optimize/CompactFlat -export" ;;
    --compactmain=* ) TCMD="$PAKCSHOME/tools/optimize/CompactFlat -main `expr $T : '--compactmain=\(.*\)'`" ;;
    *              ) TCMD=$T ;;
  esac
  $TCMD "$PROGDIR/$MODNAME"
  EXITCODE=$?
  if [ $EXITCODE -gt 0 ] ; then
     # just to be sure that there is no buggy file:
    rm -f "$PROGDIR/$MODNAME.fcy"
    exit $EXITCODE
  fi
done
