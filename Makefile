
default: build

build: build-js
	cabal install -fdev

build-js:
	psc -v -o psc-output "bower_components/*/src/**/*.purs" "purs/**/*.purs" -f "bower_components/*/src/**/*.js" -f "purs/**/*.js"
	psc-bundle "psc-output/**/*.js" -m 'Episodes' --main 'Episodes' | browserify - | uglifyjs - -c -m > static/js/episodes.js

run: build
	.cabal-sandbox/bin/episodes

