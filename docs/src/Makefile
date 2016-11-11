# To format the user manual

PAKCS=../../bin/pakcs

.PHONY: pdf
pdf : manual.pdf

manual.pdf: $(wildcard *.tex lib/*.tex ../../currytools/*/Docs/*.tex) \
	    markdown_syntax.tex
	pdflatex manual
	bibtex manual
	pdflatex manual
	makeindex manual.idx
	pdflatex manual

../Manual.pdf: manual.pdf
	thumbpdf manual
	pdflatex manual
	rm -f $@
	cp $< $@

markdown_syntax.tex: markdown_syntax.txt
	$(PAKCS) :l Markdown :eval 'readFile "$<" >>= writeFile "$@" . markdownText2LaTeX' :quit

../markdown_syntax.html: markdown_syntax.txt
	$(PAKCS) :l Markdown :eval 'readFile "$<" >>= writeFile "$@" . markdownText2CompleteHTML "Markdown Syntax"' :quit

# format installation description as HTML page
../INSTALL.html: ../../INSTALL.txt
	$(PAKCS) :l Markdown :eval 'readFile "$<" >>= writeFile "$@" . markdownText2CompleteHTML "PAKCS Installation Instructions"' :quit

# format repository installation description as HTML page
../GITINSTALL.html: ../../GITINSTALL.txt
	$(PAKCS) :l Markdown :eval 'readFile "$<" >>= writeFile "$@" . markdownText2CompleteHTML "PAKCS Repository"' :quit

# format release notes as HTML page
../RELNOTES.html: ../../RELNOTES.txt
	$(PAKCS) :l Markdown :eval 'readFile "$<" >>= writeFile "$@" . markdownText2CompleteHTML "PAKCS Release Notes"' :quit

# format Curry style guide as HTML page
../CurryStyleGuide.html: ../../currytools/casc/Docs/curry-style-guide.md
	cd ../../currytools/casc/Docs && $(MAKE)
	@if [ -f "../../currytools/casc/Docs/curry-style-guide.html" ] ; then cp ../../currytools/casc/Docs/curry-style-guide.html $@ ; fi

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
