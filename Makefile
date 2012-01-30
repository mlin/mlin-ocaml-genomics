all: lib progs

.PHONY: lib progs clean

ARCH := `uname`.`uname -p`
export ARCH

progs: lib
	cd progs; $(MAKE) clean; $(MAKE) $(MFLAGS)
#	cp PhyloCSF/_build/PhyloCSF.native PhyloCSF.$(ARCH)

lib:
	cd lib/Genomics; $(MAKE) $(MFLAGS) reinstall

clean:
	cd lib/Genomics; $(MAKE) clean
	cd progs; $(MAKE) clean
#	rm -f PhyloCSF.*
