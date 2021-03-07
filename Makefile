.PHONY: build
build:
	stack build --fast

.PHONY: ghci
ghci:
	stack ghci --ghci-options "-interactive-print=Text.Pretty.Simple.pPrint" --package pretty-simple

.PHONY: test
test:
	stack test --fast --test-arguments="--color --color"

.PHONY: test-parser
test-parser:
	stack test --fast --test-arguments="--match parser --color"

.PHONY: test-infer
test-infer:
	stack test --fast --test-arguments="--match infer --color"

.PHONY: clean
clean:
	stack clean && rm -r .stack-work

