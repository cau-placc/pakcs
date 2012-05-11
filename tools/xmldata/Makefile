# generate various tools for manipulating Curry programs:

# Required:
# - installed PAKCS

.PHONY: all
all:    dataToXml.state

# generate saved state for addtypes program:
dataToXml.state: dataToXml.curry AbstractGoodies.curry
	../../bin/pakcs -s dataToXml
	(cd ../../bin ; rm -f dataToXml ; \
	             ln -s ../tools/xmldata/dataToXml.state dataToXml)

clean: 
	../../bin/cleancurry
	(cd ../../bin ; rm -f dataToXml)

