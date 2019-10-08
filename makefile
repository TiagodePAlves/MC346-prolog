SWI ?= swipl -q


.PHONY: docs


docs: runpl-make_docs

runpl-%: %.pl
	$(SWI) -t $(patsubst %.pl,%,$<) $<
