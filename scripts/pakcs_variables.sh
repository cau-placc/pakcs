#####################################################################
# Set up the default variables according to the local installation:

# This variable must be defined only if the sources of the new front-end
# should be compiled.
# Executable of the Glasgow Haskell Compiler:
GHC=

######################################################################
# The following entries are only necessary for Curry->Prolog compiler,
# i.e., the main Curry programming environment of PAKCS:
# (undefined = do not install Curry2Prolog, TasteCurry etc)

# Directory of the SICStus-Prolog installation, i.e.,
# $SICSTUSDIR/bin/sicstus should be the name of the interpreter executable:
SICSTUSDIR=

# Executable of SWI-Prolog (if you don't have SICStus-Prolog)
# Note that the complete functionality of PAKCS is not available with
# SWI-Prolog and the efficiency is also slower than with SICStus-Prolog.
SWIPROLOG=

#####################################################################
