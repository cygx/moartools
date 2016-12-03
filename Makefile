PROVE = prove -eperl6
RM    = rm -f

test:
	$(PROVE) t

verbose:
	$(PROVE) -v t

clean:
	$(RM) t/*.tmp t/*.moarvm
