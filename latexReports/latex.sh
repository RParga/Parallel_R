if [ "$1" != "" ] && [ -f $1.tex ]; then
    echo "Nombre de archivo " $1
	latex $1.tex
	if [ -f $1.aux ];
	then
	    bibtex $1.aux
	    latex $1.tex
	fi
	if [ -f $1.idx ];
	then
	    makeindex $1.idx
	    latex $1.tex
	fi
	dvips -o $1.ps $1.dvi
	ps2pdf $1.ps $1.pdf	
else
    echo "Sin Nombre de archivo"
fi

