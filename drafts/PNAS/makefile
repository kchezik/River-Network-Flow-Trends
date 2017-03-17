#LaTex Makefile

filename= Chezik_PNAS_MS
$(filename).pdf: $(filename).tex
	latexmk -pdf -quiet -f $(filename)

clean:
	rm -f $(filename).{aux,log,fls,fdb_latexmk,ps,dvi,blg,out,bbl}

cleanall:
	rm -f $(filename).{aux,log,fls,fdb_latexmk,ps,dvi,blg,out,bbl,pdf}
