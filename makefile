SWI ?= swipl -q

DOCS ?= docs
SWIURL ?= www.swi-prolog.org


.PHONY: run clear doc


run: run.pl
	@$(SWI) -t topo $<

run_%: run.pl
	@$(SWI) -t $(patsubst run_%,%,$@) $<

test: tests/tests.pl
	@$(SWI) -t tests $<

plot: utils/plot.py
	@python $<

doc: clear $(DOCS) $(DOCS)/favicon.ico


$(DOCS): doc.pl
	$(SWI) -t run $<

%.ico:
	wget -q -P $(dir $@) $(SWIURL)/icons/$(notdir $@)

clear:
	rm -rf $(DOCS)
