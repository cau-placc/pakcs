#!/bin/sh
# delete all auxiliary files of all Curry programs in a directory

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
    rm -f $F.ast $F.cint $F.fl $F.def $F.pizza $F.acy $F.uacy $F.flc $F.fint $F.fcy $F.pl $F.po $F.pl.main $F.state $F.profile "$F"_flat.xml $F.icurry
    if [ -h "$F" ] ; then
      rm "$F"
    fi
    FDIR=`dirname $F`
    FBASE=`basename $F`
    PAKCSDIR=$FDIR/.curry/pakcs
    if [ -d $PAKCSDIR ] ; then
      rm -f $PAKCSDIR/$FBASE.pl $PAKCSDIR/$FBASE.po
      FDIRFILES=`ls -A $PAKCSDIR`
      if [ -z "$FDIRFILES" ] ; then # pakcs directory is empty
        rmdir $PAKCSDIR
      fi
    fi
    # and delete .pakcs directory of version 1.9.1
    OLDPAKCSDIR=$FDIR/.pakcs
    if [ -d $OLDPAKCSDIR ] ; then
      rm -rf $OLDPAKCSDIR
    fi
    OLDPAKCSDIR=$FDIR/.curry/.pakcs
    if [ -d $OLDPAKCSDIR ] ; then
      rm -rf $OLDPAKCSDIR
    fi
    CURRYDIR=$FDIR/.curry
    if [ -d $CURRYDIR ] ; then
      rm -f $CURRYDIR/$FBASE.fcy $CURRYDIR/$FBASE.fint $CURRYDIR/$FBASE.icurry $CURRYDIR/$FBASE.acy $CURRYDIR/$FBASE.uacy
      FDIRFILES=`ls -A $CURRYDIR`
      if [ -z "$FDIRFILES" ] ; then # .curry directory is empty
        rmdir $CURRYDIR
      fi
    fi
    rm -f -r COOSYLOGS
    rm -f -r $F.classes
  fi
done
rm -f prelude.pizza

if [ $ALL = yes ] ; then
  for i in *.fcy # look for fcy files not deleted in the first step
  do
    F=`expr $i : '\(.*\).fcy'`
    if [ "$F" != "*" ] ; then
      rm -f $F.fcy $F.pl $F.po $F.pl.main $F.state $F.profile "$F"_flat.xml
      FDIR=`dirname $F`
      FBASE=`basename $F`
      rm -f $FDIR/.curry/pakcs/$FBASE.pl $FDIR/.curry/pakcs/$FBASE.po
    fi
  done
fi
 
# delete also .curry files if there is a corresponding .lcurry file:
for F in $LCURRYFILES
do
  if [ "$F" != "*.lcurry" ] ; then
    F=`expr $F : '\(.*\)\.lcurry' \| $F`
    rm -f $F.curry
  fi
done

if [ $RECURSIVE = yes ]
then
  PATHNAME=`(cd "\`dirname \"$0\"\`" > /dev/null ; pwd)`
  for i in *
  do
    if test -d "$i"
    then
      (cd "$i" ; "$PATHNAME/`basename \"$0\"`" -r)
    fi
  done
fi
