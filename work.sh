#!/bin/sh

#https://facebook.github.io/watchman/docs/watchman-make.html
watchman-make --make './make_web.sh' -p '**/*.md' '**/page.html' '**/GNUmakefile' -t all
