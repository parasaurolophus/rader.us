all:
	$(MAKE) clean
	$(MAKE) build

clean:
	if [ -d .vitepress/dist ]; then rm -rf .vitepress/dist; fi
	if [ -d .vitepress/cache ]; then rm -rf .vitepress/cache; fi

build:
	npm run docs:build

dev:
	npm run docs:dev

preview:
	npm run docs:preview
