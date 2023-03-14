Docker image of PAKCS
=====================

This directory contains some files to create and run the
[Docker image of PAKCS](https://hub.docker.com/r/caups/pakcs).


Building a new docker image
---------------------------

If necessary, clean old image:

    > docker image rm pakcs
    > docker image prune

Then build new image:

    > docker build -t pakcs .

or

    > docker build -t pakcs -f Dockerfile-pakcs-... .


Uploading image to Docker Hub
-----------------------------

When the repository does not yet exist on Docker Hub:

1. Log in on https://hub.docker.com
2. Change to organization "caups"
3. Click on "Create Repository"
4. Choose a name ("pakcs") and click create

When the repository exists on Docker Hub:

Log into the Docker Hub from command line, tag and push the local image:

    > docker login --username mhanus42
    > docker tag pakcs caups/pakcs:<version>
    > docker push caups/pakcs:<version>

where <version> should be something like "3.5.2"
or "latest" to update the latest version.


Running the Docker image of PAKCS
---------------------------------

For convenient invocation of PAKCS and the tools contained in the
Docker image, one can use the shell script contained in this directory:

    > ./pakcs-docker.sh

invokes the interactive REPL of PAKCS. Use

    > ./pakcs-docker.sh --help

to see all options of this script.

