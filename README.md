PAKCS: The Portland Aachen Kiel Curry system
============================================

PAKCS is an implementation of the multi-paradigm declarative language
[Curry](http://www.curry-language.org) developed by RWTH Aachen,
University of Kiel and Portland State University
(email contact: pakcs@curry-language.org)

This directory contains:

`RELNOTES.txt`:
  Some information about the current release and changes w.r.t. previous
  releases of PAKCS.

`RESOURCES.html`:
  A description of various resources related to PAKCS and Curry.
  This is helpful for new users.

`INSTALL.txt`:
  Instructions how to install the system.

`GITINSTALL.txt`:
  Instructions how to install the system from the GIT repository
  (only intended for developers).

`bin`:
  A directory containing various executables
  to execute the components of PAKCS.

`src`:
  This directory contains the compiler from Curry into Prolog which
  is the basis of the main component of PAKCS: the interactive
  development environment.

`currytools`:
  This directory contains various tools for Curry
  (see the README there for a more detailed description).

`docs`:
  This directory contains some documentation (Curry Report, PAKCS User Manual,
  DTD for XML representation of FlatCurry).

`examples`:
  This directory contains a collection of example Curry programs
  and test files (prefixed by "test") to check the system using
  the currytest tool. All tests can be executed by the shell script
  `test.sh` in this directory.

`frontend`:
  This directory contains the Curry frontend, i.e., a parser for
  Curry programs. It is adapted from the parser originally developed for the
  [Muenster Curry Compiler](http://danae.uni-muenster.de/~lux/curry/).

`include`:
  This directory contains some resources which are included
  by various tools delivered with KiCS2.

`lib`:
  This directory contains a collection of standard libraries
  implemented in Curry (including the standard prelude).

`scripts`:
  This directory contains some templates for scripts used in PAKCS.

`testsuite`:
  This directory contains a collection of Curry programs
  implementing unit tests to check the functionality of the system using
  the currytest tool. All tests can be executed by the shell script
  `doTest` in this directory. Since these test suite is based on
  a test tool which is no longer supported, one should use the new
  tests contained in `testsuite2`.

`testsuite2`:
  This directory contains a collection of Curry programs
  implementing unit and property tests to check the functionality
  of the system using CurryCheck. All tests can be executed by the
  shell script `test.sh` in this directory.

`tools`:
  This directory contains various tools for PAKCS
  (see the README there for a more detailed description).
