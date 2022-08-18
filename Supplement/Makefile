all: supplement.pdf

supplement.pdf: supplement.tex ../references.bib
	xelatex supplement.tex
	bibtex supplement
	xelatex supplement.tex
	xelatex supplement.tex
	xelatex supplement.tex

clean:
	#rm -f *.pdf
	rm -f *.log *.dvi *.aux
	rm -f *.blg *.bbl
	rm -f *.log
	rm -f *.out
	rm -f *.synctex.gz
	rm -f *.bak
