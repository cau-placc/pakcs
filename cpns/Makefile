# Translate CPNS demon for fast startup:

# Required:
# - installed PAKCS

LOGFILE="/tmp/CurryPNSD.log"

CPNS: ../lib/CPNS.curry
	../bin/pakcs -s CPNS

# Shows the currently registered ports:
.PHONY: show
show: CPNS
	./CPNS show

# Shows the log file of the demon:
.PHONY: showlog
showlog: 
	cat ${LOGFILE}

.PHONY: clean
clean: 
	./stop
	../bin/cleancurry
	rm -f CPNS
