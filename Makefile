PROVE = prove -eperl6
NQP   = nqp
RM_RF = rm -rf

BYTECODE = MoarAS.moarvm
GARBAGE  = t/*.tmp t/*.moarvm

build: $(BYTECODE)

MoarAS.moarvm: %.moarvm: %.nqp
	$(NQP) --target=mbc --output=$@ $<

verbose: PROVE += -v
test verbose: MoarAS.moarvm
	$(PROVE) t

realclean: GARBAGE += .precomp $(BYTECODE)
clean realclean:
	$(RM_RF) $(GARBAGE)
