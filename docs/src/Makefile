# To format the PAKCS User Manual

PAKCS=../../bin/pakcs

.PHONY: pdf
pdf : manual.pdf

manual.pdf: manual.tex version.tex currydoc.tex markdown_syntax.tex ../../lib/TEXDOC/LAST
	pdflatex manual
	bibtex manual
	pdflatex manual
	makeindex manual.idx
	pdflatex manual

../Manual.pdf: manual.pdf
	thumbpdf manual
	pdflatex manual
	rm -f ../Manual.pdf
	cp manual.pdf ../Manual.pdf

markdown_syntax.tex: markdown_syntax.txt
	$(PAKCS) -r Markdown -m 'readFile "markdown_syntax.txt" >>= writeFile "markdown_syntax.tex" . markdownText2LaTeX'

../markdown_syntax.html: markdown_syntax.txt
	$(PAKCS) -r Markdown -m 'readFile "markdown_syntax.txt" >>= writeFile "../markdown_syntax.html" . markdownText2CompleteHTML "Markdown Syntax"'

# format installation description as HTML page
../INSTALL.html: ../../INSTALL.txt
	$(PAKCS) -r Markdown -m 'readFile "../../INSTALL.txt" >>= writeFile "../INSTALL.html" . markdownText2CompleteHTML "PAKCS Installation Instructions"'

# format repository installation description as HTML page
../GITINSTALL.html: ../../GITINSTALL.txt
	$(PAKCS) -r Markdown -m 'readFile "../../GITINSTALL.txt" >>= writeFile "../GITINSTALL.html" . markdownText2CompleteHTML "PAKCS Repository"'

# format release notes as HTML page
../RELNOTES.html: ../../RELNOTES.txt
	$(PAKCS) -r Markdown -m 'readFile "../../RELNOTES.txt" >>= writeFile "../RELNOTES.html" . markdownText2CompleteHTML "PAKCS Release Notes"'

../../lib/TEXDOC/LAST:
	$(MAKE) libdoc

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
	$(MAKE) libdoc
	$(MAKE) ../Manual.pdf

.PHONY: clean
clean:
	/bin/rm -f markdown_syntax.tex ../markdown_syntax.html \
	           ../INSTALL.html ../GITINSTALL.html ../RELNOTES.html
	/bin/mv manual.tex manual.texx
	/bin/mv manual.bib manual.bibx
	/bin/rm -f manual.?? manual.??? version-defs.sty
	/bin/mv manual.texx manual.tex
	/bin/mv manual.bibx manual.bib
	/bin/rm -f ../Manual.pdf ../Manual.ps.gz thumb* version.tex
