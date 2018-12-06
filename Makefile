dev:
	ghcid --command "stack ghci --ghci-options -fdiagnostics-color=always" --test "Main.main"
.PHONY: dev

test-watch:
	stack test --fast --file-watch
.PHONY: test-watch

