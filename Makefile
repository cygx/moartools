PROVE = prove -eperl6
NQP   = nqp
RM_RF = rm -rf

NQPBC    = MoarAS.moarvm
TINYBC   = lib/primitives.moarvm lib/arrays.moarvm moardis.moarvm
BYTECODE = $(NQPBC) $(TINYBC)
GARBAGE  = t/*.tmp t/*.moarvm

build: $(BYTECODE)

$(NQPBC): %.moarvm: %.nqp
	$(NQP) --target=mbc --output=$@ $<

$(TINYBC): %.moarvm: %.tiny MoarTL0.pm
	./moartl0 --compile $< $@

verbose: PROVE += -v
test verbose: $(BYTECODE)
	$(PROVE) t

realclean: GARBAGE += .precomp $(BYTECODE)
clean realclean:
	$(RM_RF) $(GARBAGE)
