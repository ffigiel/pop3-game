.PHONY: clean dev
VERSION_ID := $(shell git describe --always --long --tags --dirty)

.DEFAULT_GOAL:= dist

clean:
	rm -rf dist

dist: clean
	npx vite build

dev:
	npx vite

deploy-pages: clean
	npx vite build --base /pop3-game/
	git checkout f2568b9
	git branch -D gh-pages
	git checkout -b gh-pages
	mv dist/* .
	git add index.html assets
	git commit -m "chore: release ${VERSION_ID}"
	git push -f origin gh-pages
	git checkout main
