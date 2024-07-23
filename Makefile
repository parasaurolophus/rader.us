all : cleanall build zip

zip : cleanzip
	cd ./book ;	find . -name \* | zip ../book.zip -@

build :
	mdbook build

clean :
	mdbook clean

cleanzip:
	if [ -f book.zip ]; then rm book.zip; fi

cleanall: cleanzip clean

test :
	mdbook serve --open

mermaid :
	curl https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js > mermaid.min.js
