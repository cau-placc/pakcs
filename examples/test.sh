#!/bin/sh
# Shell script to test some Curry examples

CURRYHOME=..
CURRYBIN=$CURRYHOME/bin

LOGFILE=xxx$$
/bin/rm -rf .curry

cat << EOM | $CURRYBIN/curry -q :set parser -W none :set -interactive :set -time :set v0 :set printdepth 0 | tee $LOGFILE
:!echo Loading program: rev
:l rev
append [1,2] [3,4] :: [Int]
rev [1,2,3,4,5,6,7,8,9,10] :: [Int]
:!echo Loading program: higher
:l higher
g1
g2
g3
g4
g5
:!echo Loading program: quicksort
:l quicksort
qsort [2,3,1,0] :: [Int]
:!echo Loading program: qsortlet
:l qsortlet
qsort [2,3,1,0] :: [Int]
:!echo Loading program: inflists
:l inflists
goal1
goal2
:!echo Loading program: family_rel
:l family_rel
goal1 child  where child free
grandfather g c  where g,c free
:!echo Loading program: family_fun
:l family_fun
father child =:= John  where child free
grandfather g c  where g,c free
:!echo Loading program: horseman
:l horseman
horseman m h (int2nat 8) (int2nat 20)  where m,h free
horseman m h (S (S O)) f  where m,h,f free
:!echo Loading program: first
:l first
goal1
goal2 x y  where x,y free
:!echo Loading program: member
:l member
goal2 x    where x free
:!echo Loading program: colormap
:l colormap
goal l1 l2 l3 l4   where l1,l2,l3,l4 free
:!echo Loading program: account
:l account
goal1 b  where b free
goal2 b  where b free
goal3 s  where s free
:!echo Loading program: maxtree
:l maxtree
goal2
:!echo Loading program: assembler
:l assembler
main
:!echo Loading program: ralign
:l ralign
goal1
goal2
:!echo Loading program: tctest
:l tctest
:t f
:t i
:t k
:!echo Loading program: rectype
:l rectype
:t h
:!echo Loading program: diamond
:l diamond
diamond 10
:cd listcomp
:!echo Loading program: arithseq
:l arithseq
l1
l2
l3
l4
:!echo Loading program: multgen
:l multgen
goal1
goal2
goal3
:!echo Loading program: psort
:l psort
goal 6
:!echo Loading program: Default
:l Default
pabs 0
pabs 3
swap [3]
swap [3,4]
swap [3,4,5]
:cd ..
:!echo Loading program: casetest
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
