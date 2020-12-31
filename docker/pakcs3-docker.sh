#!/bin/sh
# This is a shell script to run the Docker image caups/pakcs3
# with appropriate options in order to use PAKCS
# with local files and invoke tools contained in the image.
# Adapt this file according to further requirements, e.g.,
# if you run the docker daemon as a non-root user (rootless mode, see below).

# Set docker options:
# Run interactive and remove container after execution:
DOCKEROPTS="-it --rm"
# Mount current working directory and user's home directory:
DOCKEROPTS="$DOCKEROPTS -v `pwd`:`pwd` -w `pwd` -v $HOME:$HOME -e HOME=$HOME"
# Set docker user to host user:
# NOTE: if you use docker in rootless mode, remove the following line.
DOCKEROPTS="$DOCKEROPTS -u $(id -u):$(id -g)"

DOCKERTAG="caups/pakcs3"
ENTRYPOINT=""
HELP=no

case $1 in
  --help | -h | -\? ) HELP=yes ;;
esac

if [ $HELP = yes ] ; then
  echo "Usage: pakcs3-docker.sh [-h|-?|--help] [-t TAG] [options]"
  echo ""
  echo "with options:"
  echo ""
  echo "-h|-?|--help       : show this message and quit"
  echo "-t TAG             : use docker image with tag TAG (default: $DOCKERTAG)"
  echo "cypm <opts>        : invoke Curry Package Manager with <opts>"
  echo "curry-check <opts> : invoke CurryCheck with <opts>"
  echo "curry-doc   <opts> : invoke CurryDoc with <opts>"
  echo "pakcs <opts>       : invoke PAKCS with <opts>"
  echo "<opts>             : invoke PAKCS with <opts>"
  exit
fi

# check docker image tag:
if [ $# -gt 1 -a "$1" = "-t" ] ; then
  shift ; DOCKERTAG=$1 ; shift
fi

# check whether an installed tool should be invoked:
case $1 in
  pakcs       ) shift ;;
  cypm        ) shift ; ENTRYPOINT="/pakcs/pakcs/bin/cypm" ;;
  curry-check ) shift ; ENTRYPOINT="/pakcs/cpm/bin/curry-check" ;;
  curry-doc   ) shift ; ENTRYPOINT="/pakcs/cpm/bin/curry-doc" ;;
esac

if [ -n "$ENTRYPOINT" ] ; then
  DOCKEROPTS="$DOCKEROPTS --entrypoint=$ENTRYPOINT"
fi

docker run $DOCKEROPTS $DOCKERTAG ${1+"$@"}
