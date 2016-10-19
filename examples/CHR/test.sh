#!/bin/sh
# Shell script to test the current set of CHR(Curry) examples

CURRYHOME=../..
CURRYBIN=$CURRYHOME/bin

BACKEND=`$CURRYBIN/curry :set v0 :set -time :load Distribution :eval "putStrLn (curryRuntime ++ show curryRuntimeMajorVersion)" :quit 2> /dev/null`

VERBOSE=no
if [ "$1" = "-v" ] ; then
  VERBOSE=yes
fi

if [ "$BACKEND" != sicstus4 -a "$BACKEND" != swi6 -a "$BACKEND" != swi7 ] ; then
  echo "No appropriate Prolog back end, skip the CHR tests."
  exit
fi

LOGFILE=xxx$$
$CURRYBIN/cleancurry
cat << EOM | $CURRYBIN/curry -q :set -interactive :set v0 :set printdepth 0 :set -free :set +verbose :set -time > $LOGFILE
:load Leq
main10 x        where x free
main11 x y z    where x,y,z free
main12 x y z z' where x,y,z,z' free

:load Bool
main20 x y z    where x,y,z free
main21 a b s c  where a,b,s,c free
main22 a b s c  where a,b,s,c free

:load GCD
:add CHR
runGCD $ gcd 16 /\ gcd 28
runGCD $ gcd 206 /\ gcd 40
compileCHR "GCDCHR"   [gcda,gcd2]
:load GCDCHR
solveCHR $ gcdanswer x /\ gcd 206 /\ gcd 40  where x free

:load Fib
:add CHR
runCHR [dup,fib1,fibn,addrule] $ fib 7 x    where x free
compileCHR "FIBCHR"   [fibo1,fibo2,fibo3,addrule]
:load FIBCHR
solveCHR $ fib 20 x  where x free

:load FD
main50 x y      where x,y free
main51 x        where x free
main52 [x,y,z]  where x,y,z free
main53 xs       where xs free
main55 xs       where xs free

:load UnionFind
main60
main61 x        where x free
main62 x y      where x,y free
main63
main64 x y      where x,y free
main65 x y      where x,y free
:add CHR
compileCHR "UFCHR"    [makeI,unionI,findNode,findRoot,linkEq,linkTo]
:load UFCHR
solveCHR $ andCHR [make 1, make 2, make 3, make 4, make 5, union 1 2, union 3 4, union 5 3, find 2 x, find 4 y]  where x,y free

:load Primes
runPrime $ prime 20

:load Gauss
main80 x y      where x,y free
main81 x y      where x,y free
main82 x y      where x,y free
main85 i        where i free
main86 i        where i free
:add CHR
compileCHR "GAUSSCHR" [arithrule,emptyP,constM,eliminate,bindVar]
:load GAUSSCHR
:add Gauss
solveCHR $ 3.0:*:x GAUSSCHR.:=: 6.0 /\ 2.0:*:x :+: 6.0:*:y GAUSSCHR.:=: 10.0  where x,y free

EOM
# clean up:
for p in GCDCHR FIBCHR UFCHR GAUSSCHR ; do
    $CURRYBIN/cleancurry $p
    /bin/rm -f $p*
done
################ end of tests ####################
if [ $VERBOSE = yes ] ; then
    cat $LOGFILE
fi
# Check differences:
DIFF=diff$$
diff TESTRESULT $LOGFILE > $DIFF
if [ "`cat $DIFF`" = "" ] ; then
  echo
  echo "Regression test successfully executed!"
  /bin/rm -f $LOGFILE $DIFF
else
  echo
  echo "Differences in regression test occurred:"
  cat $DIFF
  /bin/rm -f $DIFF
  /bin/mv -f $LOGFILE LOGFILE
  echo "Test output saved in file 'LOGFILE'."
  exit 1
fi
