# Translate CPNS demon for fast startup:

# Required:
# - installed curry2prolog system

LOGFILE="/tmp/CurryPNSD.log"

CPNS.state: ../lib/CPNS.curry
	../bin/pakcs -s CPNS

# Shows the currently registered ports:
.PHONY: show
show: CPNS.state
	./CPNS.state show

# Shows the log file of the demon:
.PHONY: showlog
showlog: 
	cat ${LOGFILE}

.PHONY: clean
clean: 
	./stop
	../bin/cleancurry
	rm -f CPNS.state
