#!/bin/sh
#
# Configure the generation script for Prolog saved states:
# the shell script `pakcs-makesavedstate.sh` is copied into
# `makesavedstate` where the definition of `LCALL` is set to
# a UTF-8 value.

ORGMAKESTATE=pakcs-makesavedstate.sh
GENMAKESTATE=makesavedstate

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

cat "$ORGMAKESTATE" | sed "s|^LCALL=.*$|LCALL=$LCALL|" > "$GENMAKESTATE"
chmod 755 "$GENMAKESTATE"
