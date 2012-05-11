# precompile all modules of the library
# to avoid recompilation when they are used:

PAKCS=`pwd`/../../bin/pakcs

UILIBS = GUI.curry GUI2HTML.curry HTML.curry \
	 TypedUI2GUI.curry TypedUI2HTML.curry \
	 UI.curry UI2GUI.curry UI2HTML.curry Utils.curry \
	 Json.curry Parse.curry SpicyWeb.curry

LIB_PL    = `echo $(UILIBS:%.curry=.curry/pakcs/%.pl)`

.PHONY: all
all: pl

.PHONY: pl
pl:
	@${MAKE} $(LIB_PL)

# generate all Prolog translations:
.curry/pakcs/%.pl: %.curry
	rm -f $@ && "${PAKCS}" -q -c $*

# clean all generated files
.PHONY: clean
clean:
	../../bin/cleancurry -r
