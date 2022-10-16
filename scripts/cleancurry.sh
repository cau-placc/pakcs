#!/bin/sh
# delete all auxiliary files of all Curry programs in a directory

COMPILERVERSION=`echo COMPILERVERSION must be defined here!`

RM="/bin/rm"

RECURSIVE=no
if [ "xx$1" = "xx-r" ] ; then
  RECURSIVE=yes
  shift
fi

PROG=
if [ $# = 1 ] ; then
  # remove possible suffix:
  PROG=`expr "$1" : '\(.*\)\.lcurry' \| "$1"`
  PROG=`expr "$PROG" : '\(.*\)\.curry' \| "$PROG"`
  shift
fi

if [ $# != 0 ]
then
  echo "Usage: $0 [-r] [-a] [<prog>]" >&2
  echo "-r: apply this command recursively to all subdirectories" >&2
  echo "<prog>: remove only auxiliary Curry files for program <prog>" >&2
  exit 1
fi

if [ -z "$PROG" ] ; then
  CURRYFILES="*.curry *.lcurry"
  LCURRYFILES="*.lcurry"
else
  CURRYFILES="$PROG.curry $PROG.lcurry"
  if [ -f $PROG.lcurry ] ; then
    LCURRYFILES="$PROG.lcurry"
  else
    LCURRYFILES=""
  fi
fi

for F in $CURRYFILES
do
  if [ "$F" != "*.curry" -a "$F" != "*.lcurry" ] ; then
    F=`expr $F : '\(.*\)\.lcurry' \| $F`
    F=`expr $F : '\(.*\)\.curry' \| $F`
    FDIR=`dirname $F`
    FBASE=`basename $F`
    CURRYDIR="$FDIR/.curry"
    COMPILERDIR="$CURRYDIR/$COMPILERVERSION"
    if [ -d "$COMPILERDIR" ] ; then
      "$RM" -f "$COMPILERDIR/$FBASE.pl" "$COMPILERDIR/$FBASE.po"
      CURRYF="$COMPILERDIR/$F"
      "$RM" -f "$CURRYF.cy" "$CURRYF.acy" "$CURRYF.uacy" "$CURRYF.fcy" "$CURRYF.fint" "$CURRYF.icurry" "$CURRYF.tokens" "$CURRYF.ast" "$CURRYF.sast"
      FDIRFILES=`ls -A "$COMPILERDIR"`
      if [ -z "$FDIRFILES" ] ; then # .curry/... directory is empty
        rmdir "$COMPILERDIR"
      fi
    fi
    if [ -d "$CURRYDIR" ] ; then
      CDIRFILES=`ls -A "$CURRYDIR"`
      if [ -z "$CDIRFILES" ] ; then # .curry directory is empty
        rmdir "$CURRYDIR"
      fi
    fi
    "$RM" -f -r COOSYLOGS
  fi
done

if [ $RECURSIVE = yes ]
then
  # delete .curry directory:
  "$RM" -rf ".curry/$COMPILERVERSION"
  PATHNAME=`(cd "\`dirname \"$0\"\`" > /dev/null ; pwd)`
  for i in *
  do
    if test -d "$i"
    then
      (cd "$i" ; "$PATHNAME/`basename \"$0\"`" -r)
    fi
  done
fi
