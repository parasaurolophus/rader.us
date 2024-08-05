all:
	$(MAKE) cleanall
	$(MAKE) build
	$(MAKE) zip

clean:
	if [ -d .vitepress/dist ]; then rm -rf .vitepress/dist; fi
	if [ -d .vitepress/cache ]; then rm -rf .vitepress/cache; fi

cleanzip:
	if [ -f dist.zip ]; then rm dist.zip; fi

cleanall: clean cleanzip

build:
	npm run docs:build

dev:
	npm run docs:dev

preview:
	npm run docs:preview

zip: cleanzip
	cd ./.vitepress/dist ;	find . -name \* | zip ../../dist.zip -@
