#SHELL := bash	
.ONESHELL:	
#.SHELLFLAGS := -eu -o pipefail -c	

ps-sources := $$(fd -epurs)	
ps-entrypoint := Seath.Main	
e2e-entrypoint := Seath.Test.E2E.Serve	
ps-bundle = spago bundle-module -m ${ps-entrypoint} --to output.js	

run-test:
	spago run --main Seath.Test.Main

run-dev:	
	@${ps-bundle} && BROWSER_RUNTIME=1 webpack-dev-server --progress	

e2e-serve:	
	spago bundle-module -m ${e2e-entrypoint} --to output.js	
	BROWSER_RUNTIME=1 webpack-dev-server --progress	

run-build:	
	@${ps-bundle} && BROWSER_RUNTIME=1 webpack --mode=production	

check-format:	check-explicit-exports-imports
	@purs-tidy check ${ps-sources}	

format:	
	@purs-tidy format-in-place ${ps-sources}	

check-explicit-exports-imports:
	@if grep -rn '(\.\.)' ${ps-sources}; then
		echo "Use explicit imports/exports ^"
		exit 1
	else
		echo "All imports/exports are explicit"
	fi
