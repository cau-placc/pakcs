#!/bin/sh

# Transform an existing saved state of a Curry program into
# a saved state that is executable independent of the
# generation environment, i.e.,
# - add path information to the saved state
# - add locale information (LC_ALL) to the saved state

##############################################################################
# Global settings:

# The value of the locale environment variable LC_ALL when PAKCS is
# configured. It must be a UTF-8 encoding and will be set in generated
# executables (see below).
LCALL=

# bin directory of the SICStus-Prolog installation, i.e.,
# $SICSTUSBINDIR/sicstus should be the name of the interpreter executable:
SICSTUSBINDIR=

# Executable of SWI-Prolog
SWIPROLOG=

# Settings for memory limits for a SWI-Prolog saved state
# (compare the SWI-Prolog manual for supported values).
# The default is unlimited usage. Actually, SWI-Prolog seems to
# put a limit of 1GB on every memory area in this case (on 64bit machines).
# Thus, if there is a local stack overflow in your application (e.g.,
# you see a message like "ERROR: local"), you should increase the stack sizes
# by redefining the definition of SWILIMITS

if [ -x "$SWIPROLOG" ] ; then
  # determin major version of SWI-Prolog:
  SWIVERSION=`"$SWIPROLOG" --version`
  SWI_MAJOR_VERSION=`expr "$SWIVERSION" : '.*version \([0-9]*\).*'`
  # set limits according to SWI-Prolog version:
  case "$SWI_MAJOR_VERSION" in
    # SWI-Prolog 7.*: use 4GB for the local stack
    7 ) SWILIMITS="-L4G -G0 -T0" ;;
    # SWI-Prolog 8.*: use 8GB for all stacks
    8 | 9 ) SWILIMITS="--stack_limit=8g" ;;
    # no default for other versions:
    * ) SWILIMITS="" ;;
  esac
fi

##############################################################################
# Process arguments:

STANDALONE=no
if [ "$1" = "--standalone" ] ; then
  STANDALONE=yes
  shift
fi
if [ "$1" = "-standalone" ] ; then # for backward compatibility
  STANDALONE=yes
  shift
fi

if [ $# = 1 ] ; then
  STATE="$1"
  TARGET="$1"
elif [ $# = 2 ] ; then
  STATE="$1"
  TARGET="$2"
else
  echo "Usage: $0 [-standalone] <saved_state_file> [<target_file>]"
  echo "--standalone      : transform saved state into stand-alone executable"
  echo "<saved_state_file>: existing file with the saved state"
  echo "<target_file>     : target file with transformed state (if different)"
  exit 1
fi

# add $SICSTUSBINDIR to path so that command "sicstus..." becomes executable:
if [ -n "$SICSTUSBINDIR" ] ; then
  PATH="$SICSTUSBINDIR:$PATH"
  export PATH
fi

if test ! -f "$STATE" ; then
  echo "ERROR: saved state '$STATE' does not exist!"
  exit 1
fi

if [ $STANDALONE = yes ] ; then
  if [ -n "$SICSTUSBINDIR" ] ; then
    SPLD="$SICSTUSBINDIR/spld"
    if [ ! -x "$SPLD" ] ; then
      echo "ERROR: SICStus Prolog application builder '$SPLD' not found!" >&2
      exit 1
    fi
    mv "$STATE" main.sav
    "$SPLD" --static --main=restore --resources-from-sav --resources=main.sav=`pwd`/main.sav --output=$TARGET
    rm -f main.sav
    chmod 755 "$TARGET"
    echo "Stand-alone executable '$TARGET' generated."
    exit 0
  else
    # we have SWI-Prolog and nothing must be done
    # (everything was done in saveprog)
    if [ "$STATE" != "$TARGET" ] ; then
      mv "$STATE" "$TARGET"
    fi
    exit 0
  fi
fi

# Patch the Sicstus saved state to suppress startup infos like version# 
# and add current local and PATH information:
if [ "$STATE" = "$TARGET" ] ; then
  TMPSTATE="$STATE$$"
  mv "$STATE" "$TMPSTATE"
else
  TMPSTATE="$STATE"
fi

if [ -z "$SICSTUSBINDIR" ] ; then
  # patch SWI-Prolog saved state with optimization and stack limit options:
  SWIOPTIONS="$SWILIMITS -O"
  sed "3s/-x/$SWIOPTIONS -x/" < "$TMPSTATE" > "$TMPSTATE$$"
  mv "$TMPSTATE$$" "$TMPSTATE"
fi

TMPFILE="TMPSAVEDSTATE$$"
echo "#!/bin/sh" > "$TMPFILE"
# Set LC_ALL in saved states to the installation value of LC_ALL.
# When PAKCS is installed, LC_ALL should be UTF-8 encoding to ensure
# the correct reading of source programs with UTF-8 encoding.
echo "LC_ALL=$LCALL" >> "$TMPFILE"
echo "export LC_ALL" >> "$TMPFILE"

if [ -n "$SICSTUSBINDIR" ] ; then
  # add SICSTUSBINDIR to path so that SICStus can find its binary:
  echo "PATH=\"\$PATH:$SICSTUSBINDIR\"" >> "$TMPFILE"
  echo "export PATH" >> "$TMPFILE"
  # check whether --noinfo parameter is accepted (e.g., not for SICStus 3.7):
  "$SICSTUSBINDIR/sicstus" --noinfo --help > /dev/null 2>&1
  if [ $? -eq 0 ] ; then
    # suppress "restoring" message of SICStus-Prolog:
    echo "exec \"$SICSTUSBINDIR/sicstus\" --noinfo -r \"\$0\" -a \"\$@\"" >> "$TMPFILE"
  fi
fi
cat "$TMPFILE" "$TMPSTATE" > "$TARGET"
rm "$TMPFILE" "$TMPSTATE"
chmod 755 "$TARGET"
