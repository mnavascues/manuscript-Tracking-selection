all: tracking_selection_manuscript.pdf

tracking_selection_manuscript.pdf: tracking_selection_manuscript.tex references.bib
	xelatex tracking_selection_manuscript.tex
	bibtex tracking_selection_manuscript
	xelatex tracking_selection_manuscript.tex
	xelatex tracking_selection_manuscript.tex

clean:
	#rm -f *.pdf
	rm -f *.log *.dvi *.aux
	rm -f *.blg *.bbl
	rm -f *.log
	rm -f *.out
