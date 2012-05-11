# Translate programs for managing cgi server processes:

# Required:
# - installed curry2prolog system

.PHONY: all
all: submitform Registry.state

submitform: ../lib/HtmlCgi.curry ../lib/NamedSocket.curry ../lib/CPNS.curry
	cd ../lib && ../bin/pakcs -s HtmlCgi
	mv ../lib/HtmlCgi.state submitform

Registry.state: Registry.curry ../lib/HtmlCgi.curry
	../bin/pakcs -s Registry

.PHONY: cleanServers
cleanServers:
	./Registry.state clean

.PHONY: show
show:
	./Registry.state show

.PHONY: showload
showload:
	./Registry.state showload

.PHONY: sketch
sketch:
	./Registry.state sketch

.PHONY: showall
showall:
	./Registry.state showall

.PHONY: stop
stop:
	./Registry.state stop

.PHONY: kill
kill:
	./Registry.state kill

.PHONY: clean
clean: 
	rm -f submitform
	../bin/cleancurry


