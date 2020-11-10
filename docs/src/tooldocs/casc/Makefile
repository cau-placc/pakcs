default: curry-style-guide.html

# Generate an HTML version of the style guide.
curry-style-guide.html: curry-style-guide.md style.html
ifeq ($(shell which pandoc > /dev/null ; echo $$?),0)
	pandoc -s -N --toc -H style.html -o $@ $<
else ifeq ($(CI_BUILD), yes)
	$(error "pandoc not found, can't generate Curry style description")
else
	@echo "WARNING: pandoc not found, Curry style description not generated"
endif
