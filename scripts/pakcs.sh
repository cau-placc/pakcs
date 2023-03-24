#!/bin/sh
#
# Start interactive read-eval-print loop of PAKCS

PAKCSBUILDDIR=`echo PAKCSBUILDDIR must be defined here!`
PAKCSINSTALLDIR=
# Define the main directory where PAKCS is installed:
if [ -d "$PAKCSINSTALLDIR" ] ; then
  PAKCSHOME="$PAKCSINSTALLDIR"
else
  PAKCSHOME="$PAKCSBUILDDIR"
fi
export PAKCSHOME

# The bin directory of PAKCS:
PAKCSBIN="$PAKCSHOME/bin"
# The directory where CPM installs the binaries:
CPMBIN="$HOME/.cpm/bin"

# check whether a requested tool is installed.
# If yes, execute it, otherwise exit with error.
check_and_exec_tool() {
  TOOLNAME=$1
  TOOLBIN="$PAKCSBIN/pakcs-$TOOLNAME"
  if [ -x "$TOOLBIN" ] ; then
    shift
    if [ "$TOOLNAME" = cypm ] ; then
      TOOLOPTS="-d curry_bin=\"$PAKCSBIN/pakcs\""
    else
      TOOLOPTS=
    fi
    #echo "Executing:" "$TOOLBIN" $TOOLOPTS ${1+"$@"}
    exec "$TOOLBIN" $TOOLOPTS ${1+"$@"}
  else
    echo "Incomplete installation: '$TOOLBIN' not installed!"
    echo "Please run: cd \"$PAKCSHOME\" && make" >&2
    exit 1
  fi
}

# check whether a tool of the distribution should be executed
case $1 in
  cypm | frontend ) check_and_exec_tool ${1+"$@"} ;;
esac

NOCOLOR=

# use readline wrapper rlwrap if rlwrap exists,
# we have tty as stdin, and we have a home directory to
# store rlwrap's history:
USERLWRAP=no
if tty -s ; then
  RLWRAP=`command -v rlwrap`
  if [ -x "$RLWRAP" -a -d "$HOME" ] ; then
    USERLWRAP=yes
  fi
else
  NOCOLOR="--nocolor"
fi

QUIET=no    # quiet, i.e., no messages from this script?
USECPM=yes  # should we call CPM to compute the correct load path?

# check and remove arguments that should not be passed to the REPL:
for arg do
  shift
  case $arg in
    --nocypm | -n ) USECPM=no    ;;
    --noreadline  ) USERLWRAP=no ;;
    *             ) set -- "$@" "$arg" ;;
  esac
done
#echo "ARGUMENTS PASSED TO REPL:"
#printf '%s\n' "$@"

# check REPL arguments that are relevant for this shell script:
for i in $* ; do
  case $i in
    --help | -h | -\? ) USECPM=no ;;
    --version | -V    ) USECPM=no ;;
    --numeric-version | --compiler-name | --base-version ) USECPM=no ;;
    --quiet  | -q     ) QUIET=yes ;;
  esac
done

CYPMBIN=
# if USECPM=yes, set variable CYPMBIN to the binary of CPM
if [ $USECPM = yes ] ; then
  if [ ! -d "$HOME" ] ; then   # do not use CPM without a home directory
    CYPMBIN=
  elif [ -x "$PAKCSBIN/cypm" ] ; then
    CYPMBIN="$PAKCSBIN/cypm"  # use local binary of CPM
  elif [ -x "$CPMBIN/cypm" ] ; then
    CYPMBIN="$CPMBIN/cypm"    # use ~/.cpm/bin/cypm
  else
    WHICHCPM=`command -v cypm`  
    if [ -x "$WHICHCPM" ] ; then
      CYPMBIN="$WHICHCPM"     # use another binary of CPM in the load path
    fi
  fi
fi

# Title/version of CPM passed to PAKCS:
CPMVERSION=

if [ -n "$CYPMBIN" ] ; then
  # set CURRYPATH with 'deps' command of CPM
  if [ $QUIET = no ] ; then
    echo "Compute CURRYPATH with '$CYPMBIN'..."
  fi
  # set CURRYPATH with 'deps' command of CPM
  CPMPATH=`"$CYPMBIN" -v quiet -d CURRYBIN="$PAKCSBIN/pakcs" deps -p`
  if [ $? -gt 0 ] ; then
    echo "$CPMPATH"
    exit 1
  fi
  if [ -n "$CURRYPATH" ] ; then
    CURRYPATH="$CURRYPATH:$CPMPATH" # keep existing CURRYPATH setting
  else
    CURRYPATH="$CPMPATH"
  fi
  export CURRYPATH
  # set version string of CPM
  CPMVERSION=`"$CYPMBIN" -V`
  if [ $? -gt 0 ] ; then
    CPMVERSION=
  fi
fi

REPL="$PAKCSHOME/src/pakcs"
if [ ! -x "$REPL" ] ; then
  echo "ERROR: executable '$REPL' not found!" >&2
  echo "Run: cd \"$PAKCSHOME\" && make" >&2
  exit 1
fi

# do not use rlwrap inside emacs:
if [ "$TERM" = dumb ] ; then
  USERLWRAP=no
fi

if [ $USERLWRAP = yes ] ; then
  exec rlwrap -c -f "$PAKCSHOME/tools/rlwrap-completions" "$REPL" --cpm-version "$CPMVERSION" $NOCOLOR ${1+"$@"}
else
  exec "$REPL" --cpm-version "$CPMVERSION" $NOCOLOR ${1+"$@"}
fi
