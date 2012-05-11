# To format the PAKCS User Manual

.PHONY: pdf
pdf : manual.pdf

manual.pdf: manual.tex currydoc.tex markdown_syntax.tex ../../lib/TEXDOC/LAST
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
	../../bin/pakcs -r Markdown -m 'readFile "markdown_syntax.txt" >>= writeFile "markdown_syntax.tex" . markdownText2LaTeX'

../markdown_syntax.html: markdown_syntax.txt
	../../bin/pakcs -r Markdown -m 'readFile "markdown_syntax.txt" >>= writeFile "../markdown_syntax.html" . markdownText2CompleteHTML'

../../lib/TEXDOC/LAST:
	${MAKE} libdoc

# generate documentation for all libraries
.PHONY: libdoc
libdoc:
	@cd ../../lib && ${MAKE} texdoc
	@rm -f lib && ln -s ../../lib/TEXDOC lib

.PHONY: install
install:
	${MAKE} ../markdown_syntax.html
	${MAKE} libdoc
	${MAKE} ../Manual.pdf

.PHONY: clean
clean:
	/bin/rm -f markdown_syntax.tex ../markdown_syntax.html
	/bin/mv manual.tex manual.texx
	/bin/mv manual.bib manual.bibx
	/bin/rm -f manual.?? manual.??? version-defs.sty
	/bin/mv manual.texx manual.tex
	/bin/mv manual.bibx manual.bib
	/bin/rm -f ../Manual.pdf ../Manual.ps.gz thumb*


