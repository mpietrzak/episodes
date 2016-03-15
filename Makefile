
default: pulp-prod

pulp-prod:
	pulp browserify -o purescript-output --src-path purs -m Episodes | uglifyjs - -c -m -o static/js/episodes.js

pulp-dev:
	pulp -w browserify -o purescript-output --src-path purs -m Episodes --to static/js/episodes.js

