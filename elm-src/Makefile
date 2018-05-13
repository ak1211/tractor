ELM=elm-make --yes

static/main.js: Main.elm Model.elm Msg.elm View.elm Route.elm
		$(ELM) Main.elm --warn --output=public/main.js

