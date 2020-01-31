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

# The bin directory of PAKCS:
PAKCSBIN=$PAKCSHOME/bin
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
      TOOLOPTS="-d curry_bin=$PAKCSBIN/pakcs"
    else
      TOOLOPTS=
    fi
    #echo "Executing:" "$TOOLBIN" $TOOLOPTS ${1+"$@"}
    exec "$TOOLBIN" $TOOLOPTS ${1+"$@"}
  else
    echo "Incomplete installation: '$TOOLBIN' not installed!"
    echo "Please run: cd $PAKCSHOME && make" >&2
    exit 1
  fi
}

# check whether a tool of the distribution should be executed
case $1 in
  cypm | frontend ) check_and_exec_tool ${1+"$@"} ;;
esac

# check whether we should call CPM to compute the correct load path:
if [ ! -d "$HOME" ] ; then
  USECPM=no   # do not use CPM without a home directory
elif [ -x $PAKCSBIN/cypm ] ; then
  CYPMBIN=$PAKCSBIN/cypm
  USECPM=yes
elif [ -x $CPMBIN/cypm ] ; then
  CYPMBIN=$CPMBIN/cypm
  USECPM=yes
else
  USECPM=no
fi

# check arguments for appropriate settings:
for i in $* ; do
  case $i in
    --help | -h | -\? ) USECPM=no ;;
    --version | -V    ) USECPM=no ;;
    --numeric-version | --compiler-name | --base-version ) USECPM=no ;;
    --nocypm ) USECPM=no ;;
    --noreadline ) USERLWRAP=no
  esac
done

if [ $USECPM = yes ] ; then
  # set CURRYPATH with 'deps' command of CPM
  CPMPATH=`"$CYPMBIN" -v quiet -d CURRYBIN="$PAKCSBIN/pakcs" deps -p`
  if [ $? -gt 0 ] ; then
    echo $CPMPATH
    exit 1
  fi
  if [ -n "$CURRYPATH" ] ; then
    CURRYPATH=$CURRYPATH:$CPMPATH # keep existing CURRYPATH setting
  else
    CURRYPATH=$CPMPATH
  fi
  export CURRYPATH
fi

REPL="$PAKCSHOME/src/pakcs"
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
  if [ -f "$PAKCSBIN/sicstusprolog" -a -x "$RLWRAP" -a -d "$HOME" ] ; then
    USERLWRAP=yes
  fi
fi

# do not use rlwrap inside emacs:
if [ "$TERM" = dumb ] ; then
  USERLWRAP=no
fi

if [ $USERLWRAP = yes ] ; then
  exec rlwrap -c -f "$PAKCSHOME/tools/rlwrap" "$REPL" ${1+"$@"}
else
  exec "$REPL" ${1+"$@"}
fi
