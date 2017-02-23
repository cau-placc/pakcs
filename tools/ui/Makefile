# precompile all modules of the library
# to avoid recompilation when they are used:

CURRYEXEC=`pwd`/../../bin/curry

UILIBS = GUI.curry GUI2HTML.curry HTML.curry \
	 UI.curry UI2GUI.curry UI2HTML.curry \
	 Json.curry Parse.curry SpicyWeb.curry
	 # omitted since they require addition of type contexts:
	 #TypedUI2GUI.curry TypedUI2HTML.curry

LIB_PL    = `echo $(UILIBS:%.curry=.curry/pakcs/%.pl)`

.PHONY: all
all: pl

.NOTPARALLEL: pl
.PHONY: pl
pl:
	@$(MAKE) $(LIB_PL)

# generate all Prolog translations:
.curry/pakcs/%.pl: %.curry
	rm -f $@ && "$(CURRYEXEC)" --quiet :compile $* :quit

# clean all generated files
.PHONY: clean
clean:
	../../bin/cleancurry -r
