PAKCS: The Portland Aachen Kiel Curry System
============================================

Installation Instructions
-------------------------

The current version of PAKCS runs on Unix-based platforms
and has been developed and tested under Linux (Debian/Ubuntu).
However, it should also run on similar platforms like Mac OS X or SunOS.
If you want to install and run the complete system
on your computer, you need the following software:

* [SICStus-Prolog](http://www.sics.se/sicstus.html)
  (Version 4.0 or higher) (recommended)
* [SWI-Prolog](http://www.swi-prolog.org)
  (if you do not have SICStus-Prolog, but then the execution is less
  efficient)


How to generate and install the PAKCS kernel:
---------------------------------------------

1. Go into the main directory of PAKCS (which contains
   this file). In the following, we assume that `pakcshome`
   is the name of this directory.

2. If you have the executables `sicstus` (for SICStus-Prolog) or
   `swipl` (for SWI-Prolog) in your path, execute `make` to install PAKCS.

3. Otherwise, values defining one of these executables can be passed
   as environment variables `SICSTUSPROLOG` or `SWIPROLOG` to `make`,
   e.g., by

       make SICSTUSPROLOG=/opt/sicstus/bin/sicstus

   or

       make SWIPROLOG=/usr/bin/swipl

   If both variables are undefined and the executables `sicstus`
   or `swipl` cannot be found, the PAKCS compiler system cannot
   be installed.

4. If you install PAKCS from the source code distribution, you need also the
   [Haskell Tool Stack](http://www.haskellstack.org/)
   (version 2.x or higher) to compile the front end of PAKCS,
   i.e., the executable `stack` must be in your path.

5. After the installation, add the directory `pakcshome/bin` to
   your path, e.g., by the command

       export PATH=pakcshome/bin:$PATH

   in the `bash`.

   If you do no want to have the specific version number of PAKCS
   included in your path, you can set a symbolic link like

       ln -s pakcs-<version> pakcs

   and put the directory `pakcs/bin` into your path.
   Now you can start the PAKCS compiler system via the command `pakcs`.

6. You can configure the behavior of PAKCS by various settings
   in a `pakcsrc` file. For doing so, copy the file
   `pakcshome/pakcsrc.default` as `.pakcsrc`
   (which will be automatically done when you start PAKCS for the first time)
   into you home directory and modify the settings in this file.

7. If you like to have support for _line editing or history functionality_
   in the PAKCS interactive environment (as supported by the readline
   library), you should have the Unix/Linux command `rlwrap` installed
   on your local machine. PAKCS uses `rlwrap` if it is invoked on a terminal
   without the parameter `--noreadline`.


Configuring the installation
----------------------------

If you want to install the system at a location different from
the build location, you have to specify the intended installation
location at built-time with the parameter `PAKCSINSTALLDIR`.
Furthermore, the final installation location must not exist
during built time. For instance, to built PAKCS at some local directory
and move it later to `/opt/pakcs`, you can do it by

    make PAKCSINSTALLDIR=/opt/pakcs
    ...
    mv /path/to/pakcs /opt/pakcs


Changing system constants:
--------------------------

The distribution of PAKCS is configured with a
**maximal tuple arity of 15**, i.e., Curry programs containing larger
tuple sizes cannot be compiled. If you want to increase this size
(usually, it is preferable to change your program), you have to change
(in a source distribution of PAKCS)
two system files and install your system as follows:

1. Change the definition of the constant `maxTupleArity` in the files
   `pakcshome/frontend/src/Generators/GenAnnotatedFlatCurry.hs`
   and `pakcshome/src/compiler.pl`
   according to your required maximal arity.
2. Delete all auxiliary files in the directory `pakcshome/lib/.curry`.
3. Re-install PAKCS by `make`.


Hints for specific platforms
----------------------------

### Ubuntu 7.10

If you install PAKCS from the binary Linux distribution,
the pre-compiled executable of the front-end might not find
the library `libgmp.so.3`. This problem can be fixed
by the following commands (executed as root):

    cd /usr/lib 
    ln -s libgmp.so.3.4.1 libgmp.so.3 

If you use SWI-Prolog, you should install the packages
`swi-prolog` as well as `swi-prolog-clib`
from the Ubuntu distribution.
The latter package is necessary if sockets, ports, and
other system-oriented features should be used in Curry.

(Thanks to Sergio Antoy and Steffen Mazanek for these hints.)

-------------------------------------------------------------

Contact: [Michael Hanus](http://www.informatik.uni-kiel.de/~mh/)
