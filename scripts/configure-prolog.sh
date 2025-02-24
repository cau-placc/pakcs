#!/bin/sh
#
# Configure the Prolog back end used by PAKCS, i.e., 
# set the appropriate symbolic links
# PAKCSHOME/bin/sicstusprolog  (if SICStus-Prolog is available)
# PAKCSHOME/bin/swiprolog      (if SWI-Prolog is available)

# Compute home of PAKCS installation:
PAKCSHOME=`(cd "\`dirname \"$0\"\`" > /dev/null ; pwd)`/..

# Expand path if it is not the standard one to find Prolog executables:
PATH="$PATH:/bin:/usr/bin:/usr/local/bin"

# If the environment variables are undefined, set them to a bin/...prolog
# in order to keep a previous installation with a Prolog back end:
if [ -z "$SICSTUSPROLOG" ] ; then
  SICSTUSPROLOG="$PAKCSHOME/bin/sicstusprolog"
fi
if [ -z "$SWIPROLOG" ] ; then
  SWIPROLOG="$PAKCSHOME/bin/swiprolog"
fi

# if both SICSTUSPROLOG and SWIPROLOG are not executables, try to find one:
if [ ! -x "$SICSTUSPROLOG" -a ! -x "$SWIPROLOG" ] ; then
  echo "Try to find SICStus-Prolog..."
  SICSTUSPROLOG=`command -v sicstus 2> /dev/null`
fi
if [ -x "$SICSTUSPROLOG" ] ; then
  # try to get absolute path name:
  SICSTUSPROLOG=`readlink -f "$SICSTUSPROLOG" 2> /dev/null || echo "$SICSTUSPROLOG"`
  echo "halt." | "$SICSTUSPROLOG" > /tmp/sicstusout$$ 2>&1
  if [ $? -ne 0 ] ; then
    SICSTUSPROLOG=
  else
    SWIPROLOG=
  fi
  rm /tmp/sicstusout$$
else
  SICSTUSPROLOG=
fi

# try to define SWIPROLOG if SICSTUSPROLOG is undefined:
if [ -z "$SICSTUSPROLOG" ] ; then
  if [ ! -x "$SWIPROLOG" ] ; then
    echo "No SICStus-Prolog found, looking for SWI-Prolog..."
    SWIPROLOG=`command -v swipl 2> /dev/null`
  fi
  if [ -x "$SWIPROLOG" ] ; then
    # try to get absolute path name:
    SWIPROLOG=`readlink -f "$SWIPROLOG" 2> /dev/null || echo "$SWIPROLOG"`
    echo "halt." | "$SWIPROLOG" > /tmp/swiprologout$$ 2>&1
    if [ $? -ne 0 ] ; then
      SWIPROLOG=
    fi
    rm /tmp/swiprologout$$
  fi
fi

# if both SICSTUSPROLOG and SWIPROLOG are still undefined, give up:
if [ -z "$SICSTUSPROLOG" -a -z "$SWIPROLOG" ] ; then
  echo "Cannot find 'sicstus' or 'swipl' executable in path."
  echo "Please expand or provide a correct definition for"
  echo "SICSTUSPROLOG or SWIPROLOG as a parameter for 'make'!"
  exit 1
fi

# Create symbolic links in PAKCSHOME/bin and create scripts/makesavedstate:
rm -f bin/sicstusprolog bin/swiprolog # delete old Prolog definitions
if [ -n "$SICSTUSPROLOG" ] ; then
  ln -s "$SICSTUSPROLOG" bin/sicstusprolog
  SICSTUSBINDIR=`expr "$SICSTUSPROLOG" : '\(.*\)/sicstus'`
fi
if [ -n "$SWIPROLOG" ] ; then
  ln -s "$SWIPROLOG" bin/swiprolog
fi

# Report current values:
echo '======================================================================'
echo 'PAKCS Prolog back end configured with:'
echo "SICSTUSPROLOG=$SICSTUSPROLOG"
echo "SWIPROLOG=$SWIPROLOG"
echo '======================================================================'
