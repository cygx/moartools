NAMES := $(patsubst %.asm,%,$(wildcard *.asm))
MBC   := $(NAMES:%=%.moarvm)

asm.moarvm: asm.nqp
	nqp --target=mbc --output=$@ $<

clean:
	rm -f *.moarvm

$(MBC): %.moarvm: %.asm asm.moarvm moaras
	./moaras $< $@

$(NAMES:%=eval-%): eval-%: %.asm
	./moaras $<

$(NAMES:%=dis-%): dis-%: %.moarvm
	moar --dump $<
