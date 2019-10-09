SWI ?= swipl -q

DOCS ?= docs
SWIURL ?= www.swi-prolog.org


.PHONY: clear doc


doc: clear $(DOCS) $(DOCS)/favicon.ico


$(DOCS): doc.pl
	$(SWI) -t run $<

%.ico:
	wget -q -P $(dir $@) $(SWIURL)/icons/$(notdir $@)

clear:
	rm -rf $(DOCS)
