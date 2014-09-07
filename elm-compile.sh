#!/usr/bin/env zsh

elm --make --src-dir=static/elm --set-runtime=/static/js/elm-runtime.js -b . static/elm/Main.elm
