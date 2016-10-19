#!/bin/sh
# run a Curry program without explicitly invoking the interactive environment

# Define the main directory where PAKCS is installed:
PAKCSHOME=`echo PAKCSHOME must be defined here!`
export PAKCSHOME

# usage function:
show_usage () {
    echo "Usage:"
    echo
    echo "As a shell command:"
    echo "> runcurry [Curry system options] <Curry program name> <run-time arguments>"
    echo
    echo "As a shell script: start script with"
    echo "#!/usr/bin/env runcurry"
    echo "...your Curry program defining operation 'main'..."
    echo
    echo "In interactive mode:"
    echo "> runcurry"
    echo "...type your Curry program until end-of-file..."
}

# define a new Curry file name:
NEWMOD=RUNCURRY$$
while [ -f $NEWMOD.curry ] ; do
    NEWMOD=$NEWMOD_0
done
NEWPROG=$NEWMOD.curry

CURRYARGS=
PROG=
DELPROG=no
while [ $# -gt 0 -a -z "$PROG" ] ; do
    if [ "$1" = "-h" -o "$1" = "--help" -o "$1" = "-?" ] ; then
	show_usage ; exit
    fi
    ARG=$1
    shift
    ARGwolcurry=`expr $ARG : '\(.*\)\.lcurry'`
    ARGwocurry=`expr $ARG : '\(.*\)\.curry'`
    # check whether runcurry is called in script mode, i.e., the argument
    # is not a Curry program but an existing file:
    if [ "$ARGwolcurry" != "" -o "$ARGwocurry" != "" ] ; then
	# argument is a Curry program:
	PROG=$ARG
    elif [ -x $ARG ] ; then
	# argument is not a Curry file but is executable, hence a script:
	# store ARG in a Curry program, but remove lines starting with '#':
	sed 's|^#.*$||' < $ARG > $NEWPROG
	DELPROG=yes
	PROG=$NEWPROG
    else
	# argument is a Curry system argument:
	CURRYSYSARGS="$CURRYSYSARGS $ARG"
    fi
done

if [ -z "$PROG" ] ; then
    # no program argument provided, use remaining input as program:
    PROG=$NEWPROG
    cat > $PROG
    DELPROG=yes
fi

$PAKCSHOME/bin/curry :set v0 :set parser -Wnone $CURRYSYSARGS :load "$PROG" :set args ${1+"$@"} :eval main :quit

if [ $DELPROG = yes ] ; then
    cleancurry "$PROG"
    rm "$PROG"
fi
