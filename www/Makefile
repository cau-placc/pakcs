# Translate programs for managing cgi server processes:

.PHONY: all
all: submitform Registry.state

submitform: ../lib/HtmlCgi.curry ../lib/NamedSocket.curry ../lib/CPNS.curry
	../bin/pakcs -s SubmitForm
	mv SubmitForm.state submitform

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


