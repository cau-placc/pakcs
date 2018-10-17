# To format the user manual

PAKCS=../../bin/pakcs

# Executable of the markdown translator:
MD2PDF := $(shell which md2pdf)

.PHONY: pdf
pdf : manual.pdf

manual.pdf: $(wildcard *.tex lib/*.tex tooldocs/*/*.tex) \
	    markdown_syntax.tex
	pdflatex manual
	bibtex manual
	pdflatex manual
	makeindex manual.idx
	pdflatex manual

../Manual.pdf: manual.pdf
	rm -f $@
	cp $< $@

markdown_syntax.tex: markdown_syntax.txt
	$(MD2PDF) --include --tex -o "$@" "$<"

../markdown_syntax.html: markdown_syntax.txt
	$(MD2PDF) -t "Markdown Syntax" --html -o "$@" "$<"

# format installation description as HTML page
../INSTALL.html: ../../INSTALL.txt
	$(MD2PDF) -t "PAKCS Installation Instructions" --html -o "$@" "$<"

# format repository installation description as HTML page
../GITINSTALL.html: ../../GITINSTALL.txt
	$(MD2PDF) -t "PAKCS Repository" --html -o "$@" "$<"

# format release notes as HTML page
../RELNOTES.html: ../../RELNOTES.txt
	$(MD2PDF) -t "PAKCS Release Notes" --html -o "$@" "$<"

# format Curry style guide as HTML page
../CurryStyleGuide.html: tooldocs/casc/curry-style-guide.md
	cd tooldocs/casc && $(MAKE)
	@if [ -f "tooldocs/casc/curry-style-guide.html" ] ; then cp tooldocs/casc/curry-style-guide.html $@ ; fi

# generate documentation for all libraries
.PHONY: libdoc
libdoc:
	@cd ../../lib && $(MAKE) texdoc

.PHONY: install
install:
	$(MAKE) ../markdown_syntax.html
	$(MAKE) ../INSTALL.html
	$(MAKE) ../GITINSTALL.html
	$(MAKE) ../RELNOTES.html
	$(MAKE) ../CurryStyleGuide.html
	$(MAKE) libdoc
	$(MAKE) ../Manual.pdf

.PHONY: clean
clean:
	rm -f markdown_syntax.tex ../markdown_syntax.html \
	           ../INSTALL.html ../GITINSTALL.html ../RELNOTES.html
	rm -f *.aux *.bbl *.blg *.idx *.ilg *.ind *.log *.out *.pdf *.toc *.tpt
	rm -rf lib
	rm -f ../Manual.pdf ../Manual.ps.gz thumb* version.tex
