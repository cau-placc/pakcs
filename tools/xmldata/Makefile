# Install tool to generate Curry programs for representing data in XML format
#
# Required:
# - installed Curry System (REPL)

# binaries
INSTALLBIN = $(ROOT)/bin/dataToXml

# Some modules required by the erd2curry implementation:
DEPS = dataToXml.curry AbstractGoodies.curry

.PHONY: install
install: dataToXml
	$(MAKE) uninstall
	ln -s $(CURDIR)/dataToXml $(INSTALLBIN)

.PHONY: uninstall
uninstall:
	rm -f $(INSTALLBIN)

# generate executable for dataToXml program:
dataToXml: $(DEPS)
	$(REPL) $(REPL_OPTS) :load dataToXml :save :q

.PHONY: clean
clean: uninstall
	$(CLEANCURRY)
	rm -f dataToXml
