SWI ?= swipl -q


.PHONY: doc


doc: runpl-make_doc

runpl-%: %.pl
	$(SWI) -t $(patsubst %.pl,%,$<) $<
