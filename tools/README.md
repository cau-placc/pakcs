PAKCS Tools
===========

This directory contains various tools which are only available for PAKCS.

Currently it contains:

`emacs`:
   Emacs mode for editing Curry programs

`gedit`:
   Curry mode for gedit

`rlwrap-completions`:
   File with word completions for rlwrap/readline.


Note for rlwrap:
----------------

Version 0.43 of rlwrap contains a bug which erases the prompt:

https://github.com/hanslub42/rlwrap/issues/109

To avoid this bug in this version, put the following line into the
`~/.inputrc` file:

    set enable-bracketed-paste off
