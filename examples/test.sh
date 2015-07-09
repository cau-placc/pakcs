#!/bin/sh
# Shell script to test some Curry examples

CURRYHOME=..
CURRYBIN=$CURRYHOME/bin

LOGFILE=xxx$$
$CURRYBIN/cleancurry -r
cat << EOM | $CURRYBIN/curry -q :set -interactive :set -time :set +verbose :set v0 :set printdepth 0 | tee $LOGFILE
:l rev
append [1,2] [3,4]
rev [1,2,3,4,5,6,7,8,9,10]
:l higher
g1
g2
g3
g4
g5
:l quicksort
qsort [2,3,1,0]
:l qsortlet
qsort [2,3,1,0]
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
goal1 l1 l2 l3 l4   where l1,l2,l3,l4 free
goal2 l1 l2 l3 l4   where l1,l2,l3,l4 free
:l account
goal1 b  where b free
goal2 b  where b free
goal3 s  where s free
:l maxtree
goal2
:l assembler
goal
:l ralign
goal1
goal2
:l tctest
:t f
:t i
:t k
:l rectype
:t h
:l iodemo
dialog
michael
:l england
q1 x  where x free
q2 x y  where x,y free
q4l
q5l
q7 x  where x free
q10
:l queens
queens [1,2,3,4]
:l diamond
diamond 10
:l chords
run sounds
run nicolas
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
:l default
abs 0
abs 3
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
:cd parsing
:l expr_parser
expression val "(10+5*2)/4" =:= []  where val free
:l palindrome
pali "abaaba" =:= []
pali5
:cd ..
:cd CLP
:l mortgage
mortgage 100000.0 180.0 0.01 r 0.0  where r free
:l smm
smm l  where l free
:cd ..
:cd distcurry
:l accountport
goal1 b  where b free
goal2 b  where b free
:l nameserver
:fork serve
gn1
gn2
pn1
pn2
gn1
gn2
closeServer
:l addserver
:fork addServer
addClient "localhost" 3 4
stopServer "localhost"
:cd ..
:q
EOM
################ end of tests ####################
# The SICStus Prolog interpreter:
SICSTUS=`pwd`/../bin/sicstusprolog

# The SWI-Prolog interpreter:
SWI=`pwd`/../bin/swiprolog

# Check differences:
DIFF=diff$$
if [ -x "$SICSTUS" ] ; then
  diff TESTRESULT.sicstus $LOGFILE > $DIFF
elif [ -x "$SWI" ] ; then
  diff TESTRESULT.swi     $LOGFILE > $DIFF
fi
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
