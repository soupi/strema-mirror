.PHONY: ghci
ghci:
	stack ghci --ghci-options "-interactive-print=Text.Pretty.Simple.pPrint" --package pretty-simple

.PHONY: test-parser
test-parser:
	stack test --test-arguments="--match parser"

.PHONY: test-infer
test-infer:
	stack test --test-arguments="--match infer"
