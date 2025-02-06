PAKCS Repository
================

Installation Instructions for the GIT Repository
------------------------------------------------

If you want to install the up-to-date version of PAKCS
from the developer's repository, you have to clone the
[git repository](https://git.ps.informatik.uni-kiel.de/curry/pakcs),
e.g., by the shell command

    git clone https://git.ps.informatik.uni-kiel.de/curry/pakcs.git

This creates a new directory `pakcs` containing the current version.
Go into this directory by

    cd pakcs

and execute

    git submodule update --init

in order to obtain further files managed by git, i.e.,
the Curry system libraries shared by PAKCS and KiCS2.

Then, each future update can be obtained by the executing

    git pull
    git submodule update

Now you can install the complete system by the command

    make

similar to the standard
[PAKCS installation instructions](https://git.ps.informatik.uni-kiel.de/curry/pakcs/-/blob/main/INSTALL.md).

-------------------------------------------------------------

Contact: [Michael Hanus](http://www.informatik.uni-kiel.de/~mh/)
