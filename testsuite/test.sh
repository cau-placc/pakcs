#!/bin/sh
# Shell script to test the current set of examples

CURRYHOME=`pwd`/..
CURRYBIN=$CURRYHOME/bin

# test for basic language features
test_lang()
{
  cd LanguageTests && $CURRYBIN/curry check Test*.curry && cd ..
}

# tests for type classes:
test_classes()
{
  cd TypeclassTests && $CURRYBIN/curry check Test*.curry && cd ..
}

# test for standard libraries:
test_libs()
{
  cd LibraryTests && $CURRYBIN/curry check Test*.curry && cd ..
}

if [ -x "$CURRYBIN/pakcs" ] ; then
    BACKEND=`$CURRYBIN/curry :set v0 :set -time :load Language.Curry.Distribution :eval "putStrLn (curryRuntime ++ show curryRuntimeMajorVersion)" :quit 2> /dev/null`
    # additional library tests for PAKCS with various Prolog back ends:
    TESTPAKCSBACKEND=
    case "$BACKEND" in
        sicstus3 ) TESTPAKCSBACKEND="TestIO " ;;
        sicstus4 ) TESTPAKCSBACKEND="" ;;
        swi5     ) TESTPAKCSBACKEND="TestIO " ;;
    esac
    TESTPAKCS="$TESTPAKCSBACKEND"
elif [ -x "$CURRYBIN/kics2" ] ; then
    TESTKICS2="TestPolySubExp TestUnification"
fi

# test features of specific Curry systems:
test_systems()
{
  if [ -n "$TESTPAKCS" -o -n "$TESTKICS2" ] ; then
    cd SpecialTests && $CURRYBIN/curry check $TESTPAKCS $TESTKICS2 && cd ..
  fi
}

# run all tests:
exec_all_tests()
{
  test_lang && test_classes && test_libs && test_systems
}

VERBOSE=no
if [ "$1" = "-v" ] ; then
  VERBOSE=yes
fi

# use the right Curry system for the tests:
PATH=$CURRYBIN:$PATH
export PATH

# clean up before
$CURRYBIN/cleancurry -r

LOGFILE=xxx$$

if [ $VERBOSE = yes ] ; then
  exec_all_tests
  if [ $? -gt 0 ] ; then exit 1 ; fi
else
  exec_all_tests > $LOGFILE 2>&1
  if [ $? -gt 0 ] ; then
    echo "ERROR in curry check:"
    cat $LOGFILE
    /bin/rm -f $LOGFILE
    exit 1
  fi
  /bin/rm -f $LOGFILE
fi
