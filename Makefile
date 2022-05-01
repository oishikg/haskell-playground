.PHONY: ide
ide:
	ghcid --command "stack ghci --ghci-options=-fno-code"

.PHONY: repl
repl:
	 stack ghci --no-build --ghc-options=-j4 --ghci-options=-fbyte-code
