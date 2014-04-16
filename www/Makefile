# Translate programs for managing cgi server processes:

.PHONY: all
all: submitform Registry

submitform: SubmitForm.curry $(LIBDIR)/HtmlCgi.curry \
	    $(LIBDIR)/NamedSocket.curry $(LIBDIR)/CPNS.curry
	$(REPL) $(REPL_OPTS) :load SubmitForm :save :q
	mv SubmitForm submitform

Registry: Registry.curry $(LIBDIR)/HtmlCgi.curry
	$(REPL) $(REPL_OPTS) :load Registry :save :q

.PHONY: cleanServers
cleanServers:
	./Registry clean

.PHONY: show
show:
	./Registry show

.PHONY: showload
showload:
	./Registry showload

.PHONY: sketch
sketch:
	./Registry sketch

.PHONY: showall
showall:
	./Registry showall

.PHONY: stop
stop:
	./Registry stop

.PHONY: kill
kill:
	./Registry kill

.PHONY: clean
clean:
	rm -f submitform Registry
	$(CLEANCURRY)
