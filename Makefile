.PHONY: clean dev
VERSION_ID := $(shell git describe --always --long --tags --dirty)

.DEFAULT_GOAL:= dist

clean:
	rm -rf dist

dist: clean
	mkdir dist
	elm make src/Main.elm --optimize --output dist/main.js
	cp style.css dist
	cp index.html dist

dev:
	npx elm-live src/Main.elm --host 0.0.0.0 --start-page index.html -- --output main.js

deploy-pages: dist
	git checkout f2568b9
	git branch -D gh-pages
	git checkout -b gh-pages
	mv dist/* .
	git add main.js style.css index.html
	git commit -m "chore: release ${VERSION_ID}"
	git push -f origin gh-pages
	git checkout main
