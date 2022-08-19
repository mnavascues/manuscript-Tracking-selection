all: tracking_selection_manuscript.pdf diff.pdf

tracking_selection_manuscript.pdf: tracking_selection_manuscript.tex references.bib
	xelatex tracking_selection_manuscript.tex
	bibtex tracking_selection_manuscript
	xelatex tracking_selection_manuscript.tex
	xelatex tracking_selection_manuscript.tex
	
diff.tex: tracking_selection_manuscript.tex
	latexdiff previous_version.tex tracking_selection_manuscript.tex > diff.tex

diff.pdf: diff.tex references.bib
	xelatex diff.tex
	bibtex diff
	xelatex diff.tex
	xelatex diff.tex

clean:
	#rm -f *.pdf
	rm -f *.log *.dvi *.aux
	rm -f *.blg *.bbl
	rm -f *.log
	rm -f *.out
	rm -f *.bak
