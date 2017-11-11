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

# Add PAKCS bin directory to path so that currypp can be found:
PATH=$PATH:$PAKCSHOME/bin
export PATH

# The directory where CPM installs the binaries:
CPMBIN="$HOME/.cpm/bin"

# check whether some tool is installed by CPM. If yes, execute it,
# otherwise inform the user to install it
check_and_call_tool() {
  TOOLPACKAGE=$1
  shift
  TOOLNAME=$1
  TOOLBIN="$CPMBIN"/$TOOLNAME
  shift
  shift
  if [ -x "$TOOLBIN" ] ; then
    if [ "$TOOLNAME" = cypm ] ; then
      TOOLOPTS="-d curry_bin=$PAKCSHOME/bin/pakcs"
    else
      TOOLOPTS=
    fi
    echo "Executing CPM installed tool:" "$TOOLBIN" $TOOLOPTS ${1+"$@"}
    # Extend CURRYPATH with system libraries of this installation:
    if [ -n "$CURRYPATH" ] ; then
      CURRYPATH=$CURRYPATH:$PAKCSHOME/lib
    else
      CURRYPATH=$PAKCSHOME/lib
    fi
    export CURRYPATH
    exec "$TOOLBIN" $TOOLOPTS ${1+"$@"}
  else
    echo "Curry tool '$TOOLNAME' is not installed!"
    echo "Please install it with the Curry Package Manager by:"
    echo "> cypm update && cypm install $TOOLPACKAGE"
    exit 1
  fi
}

# check whether the real program name and the first argument is a tool
# in the distribution and, if yes, exec the tool
DISTTOOL=`readlink -f $0`-$1
if [ -x "$DISTTOOL" ] ; then
  shift
  exec "$DISTTOOL" ${1+"$@"}
fi

# check whether the first argument is a tool packaged with CPM and, if yes,
# exec this tool or require its installation (for backward compatibility):
case $1 in
  addtypes  ) check_and_call_tool addtypes    curry-addtypes ${1+"$@"} ;;
  analyze   ) check_and_call_tool cass        cass           ${1+"$@"} ;;
  browse    ) check_and_call_tool currybrowse curry-browse   ${1+"$@"} ;;
  check     ) check_and_call_tool currycheck  curry-check    ${1+"$@"} ;;
  cypm      ) check_and_call_tool cpm         cypm           ${1+"$@"} ;;
  data2xml  ) check_and_call_tool xmldata     curry-data2xml ${1+"$@"} ;;
  doc       ) check_and_call_tool currydoc    curry-doc      ${1+"$@"} ;;
  erd2curry ) check_and_call_tool ertools     erd2curry      ${1+"$@"} ;;
  genmake   ) check_and_call_tool makefile    curry-genmake  ${1+"$@"} ;;
  pp        ) check_and_call_tool currypp     currypp        ${1+"$@"} ;;
  run       ) check_and_call_tool runcurry    runcurry       ${1+"$@"} ;;
  spiceup   ) check_and_call_tool spicey      curry-spiceup  ${1+"$@"} ;;
  style     ) check_and_call_tool casc        curry-style    ${1+"$@"} ;;
  verify    ) check_and_call_tool verify      curry-verify   ${1+"$@"} ;;
esac

# check whether we are inside a package by searching for `package.json`
# in some parent directory
CURDIR=`pwd`
PKGFOUND=no
while [ "$CURDIR" != "/" -a $PKGFOUND = no ] ; do
    if [ -f "$CURDIR/package.json" ] ; then
        PKGFOUND=yes
    else
        CURDIR=`dirname "$CURDIR"`
    fi
done

# check whether we do not need to call CPM:
USECPM=yes
for i in $* ; do
  case $i in
    --help | -h | -\? ) USECPM=no ;;
    --version | -V    ) USECPM=no ;;
    --nocypm | --numeric-version | --compiler-name | --base-version ) USECPM=no ;;
  esac
done

if [ $PKGFOUND = yes -a $USECPM = yes ] ; then
  # set CURRYPATH with 'deps' command of CPM
  CURRYPATH=`"$0" cypm -v quiet -d CURRYBIN="$0" deps -p`
  if [ $? -gt 0 ] ; then
    echo $CURRYPATH
    exit 1
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
  if [ -f "$PAKCSHOME/bin/sicstusprolog" -a -x "$RLWRAP" -a -d "$HOME" ] ; then
    USERLWRAP=yes
  fi
fi

for i in $* ; do
  if [ $i = "--noreadline" ] ; then
    USERLWRAP=no
  fi
done

if [ $USERLWRAP = yes ] ; then
  exec rlwrap -c -f "$PAKCSHOME/tools/rlwrap" "$REPL" ${1+"$@"}
else
  exec "$REPL" ${1+"$@"}
fi
