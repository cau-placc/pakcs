#!/bin/sh

# Transform an existing saved state of a Curry program into
# a saved state that is executable independent of the
# generation environment, i.e.,
# - add path information to the saved state
# - add locale information (LANG, LC_ALL) to the saved state

# Directory of the SICStus-Prolog installation, i.e.,
# $SICSTUSDIR/bin/sicstus should be the name of the interpreter executable:
SICSTUSDIR=

STANDALONE=no
if [ "$1" = "-standalone" ] ; then
  STANDALONE=yes
  shift
fi

if [ "$1" = "-error" ] ; then
  echo "WARNING: option -noerror no longer supported!"
  shift
fi

if [ $# = 1 ] ; then
  STATE=$1
  TARGET=$1
elif [ $# = 2 ] ; then
  STATE=$1
  TARGET=$2
else
  echo "Usage: $0 [-standalone] <saved_state_file> [<target_file>]"
  echo "-standalone: transform saved state into stand-alone executable"
  echo "saved_state: existing file with the saved state"
  echo "target_file: target file with transformed state (if different)"
  exit 1
fi

# add $SICSTUSDIR/bin to path so that command "sicstus..." becomes executable:
if [ -n "$SICSTUSDIR" ] ; then
  PATH=$SICSTUSDIR/bin:$PATH
  export PATH
fi

if test ! -f "$STATE" ; then
  echo "ERROR: saved state '$STATE' does not exist!"
  exit 1
fi

if [ $STANDALONE = yes ] ; then
  if [ -n "$SICSTUSDIR" ] ; then
    SPLD=$SICSTUSDIR/bin/spld
    if [ ! -x "$SPLD" ] ; then
      echo "ERROR: SICStus Prolog application builder '$SPLD' not found!" >&2
      exit 1
    fi
    mv "$STATE" main.sav
    "$SPLD" --static --main=restore --resources-from-sav --resources=main.sav=`pwd`/main.sav --output=$STATE
    rm -f main.sav
    chmod 755 "$STATE"
    echo "Stand-alone executable '$STATE' generated."
    exit 0
  else
    # we have SWI-Prolog and nothing must be done (everything was done in saveprog)
    exit 0
  fi
fi

# Patch the Sicstus saved state to suppress startup infos like version# 
# and add current local and PATH information:
if [ "$STATE" = "$TARGET" ] ; then
  TMPSTATE=$STATE$$
  mv $STATE $TMPSTATE
else
  TMPSTATE=$STATE
fi

if [ -z "$SICSTURDIR" ] ; then
  # patch SWI-Prolog saved state with unlimited memory option
  # so that the generated binaries have the same behavior as PAKCS:
  sed "3s/-x/-L0 -G0 -T0 -x/" < $TMPSTATE > $TMPSTATE$$
  mv $TMPSTATE$$ $TMPSTATE
fi

TMPFILE=TMPSAVEDSTATE$$
echo "#!/bin/sh" > $TMPFILE
if test -n "$LANG" ; then
  echo "LANG=$LANG" >> $TMPFILE
  echo "export LANG" >> $TMPFILE
fi
if test -n "$LC_ALL" ; then
  echo "LC_ALL=$LC_ALL" >> $TMPFILE
  echo "export LC_ALL" >> $TMPFILE
fi

if [ -n "$SICSTUSDIR" ] ; then
  # add SICSTUSDIR/bin to path so that SICStus can find its binary:
  echo "PATH=\"\$PATH:$SICSTUSDIR/bin\"" >> $TMPFILE
  echo "export PATH" >> $TMPFILE
  # check whether --noinfo parameter is accepted (e.g., not for SICStus 3.7):
  $SICSTUSDIR/bin/sicstus --noinfo --help > /dev/null 2>&1
  if [ $? -eq 0 ] ; then
    # suppress "restoring" message of SICStus-Prolog:
    echo "exec $SICSTUSDIR/bin/sicstus --noinfo -r \"\$0\" -a \"\$@\"" >> $TMPFILE
  fi
fi
cat $TMPFILE $TMPSTATE > $TARGET
rm $TMPFILE $TMPSTATE
chmod 755 $TARGET
