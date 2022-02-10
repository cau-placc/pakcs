PAKCS: The Portland Aachen Kiel Curry system
============================================

PAKCS is an implementation of the multi-paradigm declarative language
[Curry](http://www.curry-lang.org) developed by Kiel University,
RWTH Aachen, and Portland State University
(email contact: pakcs@curry-lang.org)

This directory contains:

`RELNOTES.txt`:
  Some information about the current release and changes w.r.t. previous
  releases of PAKCS.

`RESOURCES.html`:
  A description of various resources related to PAKCS and Curry.
  This is helpful for new users.

`INSTALL.md`:
  Instructions how to install the system.

`GITINSTALL.md`:
  Instructions how to install the system from the GIT repository
  (only intended for developers).

`bin`:
  A directory containing various executables
  to execute the components of PAKCS.

`ci`:
  This directory contains some files supporting the CI build of gitlab.

`currytools`:
  This directory contains some base tools for Curry
  (see the README there for a more detailed description).

`docker`:
  This directory contains the files to generate the docker images of PAKCS.

`docs`:
  This directory contains some documentation (Curry Report, PAKCS User Manual,
  DTD for XML representation of FlatCurry).

`examples`:
  This directory contains a collection of example Curry programs
  and a shell script `test.sh` to test the basic functionality
  of the system.

`frontend`:
  This directory contains the Curry frontend, i.e., a parser for
  Curry programs. It is adapted from the parser originally developed for the
  [Muenster Curry Compiler](http://danae.uni-muenster.de/curry/).

`lib`:
  This directory contains the standard libraries of PAKCS
  (including the standard prelude).

`man`:
  This directory contains the pages for the `man` command in the
  Debian distribution of PAKCS.

`scripts`:
  This directory contains some templates for scripts used in PAKCS.

`src`:
  This directory contains the compiler from Curry into Prolog and
  the main component of PAKCS, the interactive development environment.

`testsuite`:
  This directory contains a collection of Curry programs
  implementing unit and property tests to check the functionality
  of the system using CurryCheck. All tests can be executed by the
  shell script `test.sh` in this directory.

`tools`:
  This directory contains various tools for PAKCS
  (see the README there for a more detailed description).
