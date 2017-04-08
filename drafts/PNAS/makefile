all: pdf

pdf:
	pandoc -H options.sty -s -S -V fontsize=11pt --bibliography=ms.bib --csl=nature.csl --latex-engine=pdflatex Chezik_PNAS_MS.md -o Chezik_PNAS_MS.docx
