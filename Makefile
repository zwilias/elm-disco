elm:
	@cd $(ROOT)
	@elm make --debug src/Disco.elm



watch:
	find src -name '*.elm' | entr -c make elm

elm-example:
	@cd $(ROOT)
	@elm make --debug example/Main.elm --output example/app.js

watch-example:
	find example -name '*.elm' | entr -c make elm-example