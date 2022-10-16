#!/bin/sh
#
# Configure the Prolog back end used by PAKCS, i.e., 
# set the appropriate symbolic links
# PAKCSHOME/bin/sicstusprolog  (if SICStus-Prolog is available)
# PAKCSHOME/bin/swiprolog      (if SWI-Prolog is available)

# Check LC_ALL, LC_CTYPE, or LANG for UTF-8 encoding:
if [ -n "$LC_ALL" ] ; then
  LCALL="$LC_ALL"
elif [ -n "$LC_CTYPE" ] ; then
  LCALL="$LC_CTYPE"
else
  LCALL="$LANG"
fi
case "$LCALL" in
  *UTF-8 | *UTF8 | *utf-8 | *utf8 ) ;;
  * ) echo "WARNING: locale/LC_ALL has no UTF-8 encoding but value '$LCALL'"
      echo "Since PAKCS might not work correctly with non-ASCII files,"
      echo "LC_ALL is set to 'C.UTF-8' in generated executables."
      echo "If this does not work, please set LC_ALL to another UTF-8 value."
      LCALL=C.UTF-8 ;;
esac

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
  SICSTUSPROLOG=`which sicstus 2> /dev/null`
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
    SWIPROLOG=`which swipl 2> /dev/null`
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

# Configure `scripts/makesavedstate` which is used to generate executables:
ORGMAKESTATE=scripts/pakcs-makesavedstate.sh
MAKESTATE=scripts/makesavedstate
# Create symbolic links in PAKCSHOME/bin and create scripts/makesavedstate:
rm -f bin/sicstusprolog bin/swiprolog "$MAKESTATE" # delete old definitions
if [ -n "$SICSTUSPROLOG" ] ; then
  ln -s "$SICSTUSPROLOG" bin/sicstusprolog
  SICSTUSBINDIR=`expr "$SICSTUSPROLOG" : '\(.*\)/sicstus'`
  # store the value of SICSTUSBINDIR in script scripts/makesavedstate :
  cat "$ORGMAKESTATE" |
    sed "s|^LCALL=.*$|LCALL=$LCALL|" |
    sed "s|^SICSTUSBINDIR=.*$|SICSTUSBINDIR=\"$SICSTUSBINDIR\"|" > "$MAKESTATE"
  chmod 755 "$MAKESTATE"
fi
if [ -n "$SWIPROLOG" ] ; then
  ln -s "$SWIPROLOG" bin/swiprolog
  # store the value of SWIPROLOG in script scripts/makesavedstate :
  cat "$ORGMAKESTATE" |
    sed "s|^LCALL=.*$|LCALL=$LCALL|" |
    sed "s|^SWIPROLOG=.*$|SWIPROLOG=\"$SWIPROLOG\"|" > "$MAKESTATE"
  chmod 755 "$MAKESTATE"
fi

# Report current values:
echo '======================================================================'
echo 'PAKCS Prolog back end configured with:'
echo "SICSTUSPROLOG=$SICSTUSPROLOG"
echo "SWIPROLOG=$SWIPROLOG"
echo '======================================================================'
