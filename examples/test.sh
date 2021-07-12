#!/bin/sh
# Shell script to test some Curry examples

CURRYHOME=..
CURRYBIN=$CURRYHOME/bin

LOGFILE=xxx$$
/bin/rm -rf .curry

cat << EOM | $CURRYBIN/curry :set v0 :set parser -v0 -W none :set -interactive :set -time :set printdepth 0 :set +echo | tee $LOGFILE
:set v1
:load rev
append [1,2] [3,4] :: [Int]
rev [1,2,3,4,5,6,7,8,9,10] :: [Int]
:load higher
g1
g2
g3
g4
g5
:load quicksort
qsort [2,3,1,0] :: [Int]
:load qsortlet
qsort [2,3,1,0] :: [Int]
:load inflists
goal1
goal2
:load family_rel
goal1 child  where child free
grandfather g c  where g,c free
:load family_fun
father child =:= John  where child free
grandfather g c  where g,c free
:load horseman
horseman m h (int2nat 8) (int2nat 20)  where m,h free
horseman m h (S (S O)) f  where m,h,f free
:load first
goal1
goal2 x y  where x,y free
:load member
goal2 x    where x free
:load colormap
goal l1 l2 l3 l4   where l1,l2,l3,l4 free
:load account
goal1 b  where b free
goal2 b  where b free
goal3 s  where s free
:load maxtree
goal2
:load assembler
main
:load ralign
goal1
goal2
:load tctest
:t f
:t i
:t k
:load rectype
:t h
:load diamond
diamond 10
:cd listcomp
:load arithseq
l1
l2
l3
l4
:load multgen
goal1
goal2
goal3
:load psort
goal 6
:load Default
pabs 0
pabs 3
swap [3]
swap [3,4]
swap [3,4,5]
:cd ..
:load casetest
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
