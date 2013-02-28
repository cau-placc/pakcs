# generate various tools for manipulating Curry programs:

# Required:
# - installed PAKCS

.PHONY: all
all:    dataToXml

# generate saved state for addtypes program:
dataToXml: dataToXml.curry AbstractGoodies.curry
	../../bin/pakcs -s dataToXml
	(cd ../../bin ; rm -f dataToXml ; \
	             ln -s ../tools/xmldata/dataToXml dataToXml)

clean: 
	../../bin/cleancurry
	(cd ../../bin ; rm -f dataToXml)

