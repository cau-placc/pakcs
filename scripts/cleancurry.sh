#!/bin/sh
# delete all auxiliary files of all Curry programs in a directory

RM=/bin/rm

RECURSIVE=no
if [ "xx$1" = "xx-r" ] ; then
  RECURSIVE=yes
  shift
fi

ALL=no
if [ "xx$1" = "xx-a" ] ; then
  ALL=yes
  shift
fi

PROG=
if [ $# = 1 ] ; then
  # remove possible suffix:
  PROG=`expr $1 : '\(.*\)\.lcurry' \| $1`
  PROG=`expr $PROG : '\(.*\)\.curry' \| $PROG`
  shift
fi

if [ $# != 0 ]
then
  echo "Usage: $0 [-r] [-a] [<prog>]" >&2
  echo "-r: apply this command recursively to all subdirectories" >&2
  echo "-a: remove all auxiliary Curry files (even those without a source file)" >&2
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
    $RM -f $F.ast $F.cint $F.fl $F.def $F.pizza $F.profile "$F"_flat.xml
    FDIR=`dirname $F`
    FBASE=`basename $F`
    PAKCSDIR=$FDIR/.curry/pakcs
    if [ -d $PAKCSDIR ] ; then
      $RM -f $PAKCSDIR/$FBASE.pl $PAKCSDIR/$FBASE.po
      FDIRFILES=`ls -A $PAKCSDIR`
      if [ -z "$FDIRFILES" ] ; then # pakcs directory is empty
        rmdir $PAKCSDIR
      fi
    fi
    CURRYDIR=$FDIR/.curry
    if [ -d $CURRYDIR ] ; then
      CURRYF=.curry/$F
      $RM -f $CURRYF.cy $CURRYF.acy $CURRYF.uacy $CURRYF.fcy $CURRYF.fint $CURRYF.icurry $CURRYF.tokens
      FDIRFILES=`ls -A $CURRYDIR`
      if [ -z "$FDIRFILES" ] ; then # .curry directory is empty
        rmdir $CURRYDIR
      fi
    fi
    $RM -f -r COOSYLOGS
    $RM -f -r $F.classes
  fi
done
$RM -f prelude.pizza

if [ $ALL = yes ] ; then
  for i in *.fcy # look for fcy files not deleted in the first step
  do
    F=`expr $i : '\(.*\).fcy'`
    if [ "$F" != "*" ] ; then
      $RM -f $F.fcy $F.pl $F.po $F.pl.main $F.state $F.profile "$F"_flat.xml
      FDIR=`dirname $F`
      FBASE=`basename $F`
      $RM -f $FDIR/.curry/pakcs/$FBASE.pl $FDIR/.curry/pakcs/$FBASE.po
    fi
  done
fi
 
# delete also .curry files if there is a corresponding .lcurry file:
for F in $LCURRYFILES
do
  if [ "$F" != "*.lcurry" ] ; then
    F=`expr $F : '\(.*\)\.lcurry' \| $F`
    $RM -f $F.curry
  fi
done

if [ $RECURSIVE = yes ]
then
  # delete .curry directory:
  $RM -rf .curry  
  PATHNAME=`(cd "\`dirname \"$0\"\`" > /dev/null ; pwd)`
  for i in *
  do
    if test -d "$i"
    then
      (cd "$i" ; "$PATHNAME/`basename \"$0\"`" -r)
    fi
  done
fi
