# Translate CPNS demon for fast startup:

# Required:
# - installed Curry system (REPL)

LOGFILE="/tmp/CurryPNSD.log"

CPNSD: ../lib/CPNS.curry CPNSD.curry
	$(REPL) $(REPL_OPTS) :load CPNSD :save :q

# Shows the currently registered ports:
.PHONY: show
show: CPNSD
	./CPNSD show

# Shows the log file of the demon:
.PHONY: showlog
showlog: 
	cat $(LOGFILE)

.PHONY: clean
clean: 
	./stop
	$(CLEANCURRY)
	rm -f CPNSD
