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
    echo "> runcurry [Curry system options] <Curry program> <run-time arguments>"
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

DELPROG=no
if [ $# = 0 ] ; then
    # no arguments provided, use remaining input as program:
    PROG=$NEWPROG
    cat > $PROG
    DELPROG=yes
elif [ $# = 1 ] ; then
    if [ "$1" = "-h" -o "$1" = "--help" -o "$1" = "-?" ] ; then
	show_usage ; exit
    fi
    PROG=$1
    shift
    # check whether runcurry is called in script mode, i.e., the argument
    # is not a Curry program but an existing file:
    PROGwolcurry=`expr $PROG : '\(.*\)\.lcurry'`
    PROGwocurry=`expr $PROG : '\(.*\)\.curry'`
    if [ -z "$PROGwolcurry" -a -z "$PROGwocurry" -a -f $PROG ] ; then
	# store PROG in a Curry program, but remove lines starting with '#':
	sed 's|^#.*$||' < $PROG > $NEWPROG
	DELPROG=yes
	PROG=$NEWPROG
    fi
else
    # split arguments into system arguments and Curry program:
    CURRYSYSARGS=
    PROG=
    while [ -z "$PROG" ] ; do
	# check whether argument is the name of a Curry program:
	PROGwolcurry=`expr $1 : '\(.*\)\.lcurry'`
	PROGwocurry=`expr $1 : '\(.*\)\.curry'`
	if [ -z "$PROGwolcurry" -a -z "$PROGwocurry" ] ; then
	    CURRYSYSARGS="$CURRYSYSARGS $1"
	else
	    PROG=$1
	fi
	shift
	if [ $# = 0 ] ; then
	    echo "ERROR: No Curry program name provided as argument!" >&2
	    exit 1
	fi
    done
fi


$PAKCSHOME/bin/curry :set v0 :set parser -Wnone $CURRYSYSARGS :load "$PROG" :set args ${1+"$@"} :eval main :quit

if [ $DELPROG = yes ] ; then
    cleancurry "$PROG"
    rm "$PROG"
fi
