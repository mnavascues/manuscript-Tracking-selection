all: tracking_selection_manuscript.pdf

tracking_selection_manuscript.pdf: tracking_selection_manuscript.tex references.bib
	pdflatex tracking_selection_manuscript.tex
	bibtex tracking_selection_manuscript
	pdflatex tracking_selection_manuscript.tex
	pdflatex tracking_selection_manuscript.tex

clean:
	rm -f *.pdf
	rm -f *.log *.dvi *.aux
	rm -f *.blg *.bbl
	#rm -f *.eps *.[1-9]
	rm -f src/*.mpx *.mpx
