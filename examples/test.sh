#!/bin/sh
# Shell script to test some Curry examples

CURRYHOME=..
CURRYBIN=$CURRYHOME/bin

LOGFILE=xxx$$
$CURRYBIN/cleancurry -r
cat << EOM | $CURRYBIN/curry -q :set parser -W none :set -interactive :set -time :set +verbose :set v0 :set printdepth 0 | tee $LOGFILE
:l rev
append [1,2] [3,4] :: [Int]
rev [1,2,3,4,5,6,7,8,9,10] :: [Int]
:l higher
g1
g2
g3
g4
g5
:l quicksort
qsort [2,3,1,0] :: [Int]
:l qsortlet
qsort [2,3,1,0] :: [Int]
:l inflists
goal1
goal2
:l family_rel
goal1 child  where child free
grandfather g c  where g,c free
:l family_fun
father child =:= John  where child free
grandfather g c  where g,c free
:l horseman
horseman m h (int2nat 8) (int2nat 20)  where m,h free
horseman m h (S (S O)) f  where m,h,f free
:l first
goal1
goal2 x y  where x,y free
:l member
goal2 x    where x free
:l colormap
goal l1 l2 l3 l4   where l1,l2,l3,l4 free
:l account
goal1 b  where b free
goal2 b  where b free
goal3 s  where s free
:l maxtree
goal2
:l assembler
main
:l ralign
goal1
goal2
:l tctest
:t f
:t i
:t k
:l rectype
:t h
:l diamond
diamond 10
:cd listcomp
:l arithseq
l1
l2
l3
l4
:l multgen
goal1
goal2
goal3
:l psort
goal 6
:l Default
pabs 0
pabs 3
swap [3]
swap [3,4]
swap [3,4,5]
:cd ..
:l casetest
swap [1]
swap [1,2]
swap [1,2,3]
f [1,2]
g (Just "xyz")
g (Just "ab")
h [1,2]
h [1,3]
h [2,div 1 0]
:q
EOM
################ end of tests ####################
# The SICStus Prolog interpreter:
SICSTUS=`pwd`/../bin/sicstusprolog

# The SWI-Prolog interpreter:
SWI=`pwd`/../bin/swiprolog

# Check differences:
DIFF=diff$$
diff TESTRESULT.txt $LOGFILE > $DIFF
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
