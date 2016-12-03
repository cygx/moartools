PROVE = prove -eperl6
RM_F  = rm -f
RM_RF = rm -rf

test:
	$(PROVE) t

verbose:
	$(PROVE) -v t

clean:
	$(RM_F) t/*.tmp t/*.moarvm

realclean: clean
	$(RM_RF) .precomp
