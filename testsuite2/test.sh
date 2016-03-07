#!/bin/sh
# Shell script to test the current set of examples

CURRYHOME=..
CURRYBIN=$CURRYHOME/bin

# test for basic language features
TESTLANG="testFunctional testInfinite testHigher testNarrowing testNondet testCase testFCase testLetRec testRecords testNonLinearPattern testFuncPattern testAsPattern"

# test for standard libraries
TESTLIBS="testPrelude testAbstractCurry testArray testCombinatorial testDequeue testDirectory testFiniteMap testFlatCurryGoodies testFormat testGlobal testInteger testIOExts testKeyDatabaseSQLite testList testRedBlackTree testRegExp testSetFunctions testSort testSystem testTraversal"

if [ -x "$CURRYBIN/pakcs" ] ; then
    BACKEND=`$CURRYBIN/curry :set v0 :set -time :load Distribution :eval "putStrLn (curryRuntime ++ show curryRuntimeMajorVersion)" :quit 2> /dev/null`
    # additional library tests for PAKCS with various Prolog back ends:
    TESTPAKCSBACKEND=
    case "$BACKEND" in
        sicstus3 ) TESTPAKCSBACKEND="testIO testCLPB " ;;
        sicstus4 ) TESTPAKCSBACKEND="testCLPB " ;;
        swi5     ) TESTPAKCSBACKEND="testIO " ;;
    esac
    TESTPAKCS="testDatabase testFlatCurryXML testGlobalVariable $TESTPAKCSBACKEND"
elif [ -x "$CURRYBIN/kics2" ] ; then
    TESTKICS2="testExpTypeInference testPolySubExp testUnification testUnsafeSearchTree"
fi

ALLTESTS="$TESTLANG $TESTLIBS $TESTPAKCS $TESTKICS2"

VERBOSE=no
if [ "$1" = "-v" ] ; then
  VERBOSE=yes
fi

# use the right Curry system for the tests:
PATH=$CURRYBIN:$PATH
export PATH

# clean up before
$CURRYBIN/cleancurry

LOGFILE=xxx$$

if [ $VERBOSE = yes ] ; then
  $CURRYBIN/currycheck $ALLTESTS
  if [ $? -gt 0 ] ; then
    exit 1
  fi
else
  $CURRYBIN/currycheck $ALLTESTS > $LOGFILE 2>&1
  if [ $? -gt 0 ] ; then
    echo "ERROR in currycheck:"
    cat $LOGFILE
    /bin/rm -f $LOGFILE
    exit 1
  fi
  /bin/rm -f $LOGFILE
fi
