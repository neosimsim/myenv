.DELETE_ON_ERROR:
.SUFFIXES: .pdf .tex
.PHONY: clean install

PREFIX=$(HOME)

FILES=banner.sty \
	lists.sty \
	brief.cls \
	colorsections.sty \
	neosimsim.sty \
	neosimsim-literate-programming.sty \
	neosimsim-a4paper.sty \
	neosimsim-a5paper.sty \

TEX=xelatex
EXAMPLES=banner-example.pdf \
		lists-examples.pdf \
		brief-example.pdf \
		colorsections-example.pdf \
		neosimsim-example.pdf \

pdf: $(EXAMPLES)

neosimsim-example.pdf: neosimsim.sty neosimsim-a4paper.sty

.tex.pdf:
	$(TEX) $<
	$(TEX) $<

clean:
	rm -f *.aux *.log *.pdf *.out *.toc

install:
	mkdir -p $(PREFIX)/texmf/tex/latex/custom
	cp $(FILES) $(PREFIX)/texmf/tex/latex/custom

uninstall:
	cd $(PREFIX)/texmf/tex/latex/custom && rm -f $(PREFIX)/texmf/tex/latex/custom
