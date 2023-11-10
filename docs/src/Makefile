# To format the user manual

PAKCS=../../bin/pakcs

.PHONY: pdf
pdf : manual.pdf

manual.pdf: $(wildcard *.tex lib/*.tex tooldocs/*/*.tex)
	pdflatex manual
	bibtex manual
	pdflatex manual
	makeindex manual.idx
	pdflatex manual

../Manual.pdf: manual.pdf
	rm -f $@
	cp $< $@

# generate documentation for all libraries
.PHONY: libdoc
libdoc:
	@cd ../../lib && $(MAKE) texdoc

.PHONY: install
install:
	$(MAKE) libdoc
	$(MAKE) ../Manual.pdf

.PHONY: clean
clean:
	rm -f markdown_syntax.tex ../markdown_syntax.html \
	           ../INSTALL.html ../GITINSTALL.html ../RELNOTES.html
	rm -f *.aux *.bbl *.blg *.idx *.ilg *.ind *.log *.out *.pdf *.toc *.tpt
	rm -rf lib
	rm -f ../Manual.pdf ../Manual.ps.gz thumb* version.tex
