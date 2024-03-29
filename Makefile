GHCFLAGS=-Wall -XNoCPP -fno-warn-name-shadowing -XHaskell98 -O2
HLINTFLAGS=-XHaskell98 -XNoCPP -i 'Use camelCase' -i 'Use String' -i 'Use head' -i 'Use string literal' -i 'Use list comprehension' --utf8
VERSION=0.2

.PHONY: all clean doc install shell test debian

all: report.html doc dist/build/libHSpktree-$(VERSION).a dist/pktree-$(VERSION).tar.gz

Tests: Tests.hs
	ghc --make $(GHCFLAGS) $^ -o $@

test: Tests
	./Tests

install: dist/build/libHSpktree-$(VERSION).a
	cabal install

debian: debian/control

shell:
	ghci $(GHCFLAGS)

report.html: Data/PKTree.hs Tests.hs
	-hlint $(HLINTFLAGS) --report $^

doc: dist/doc/html/pktree/index.html README

README: pktree.cabal
	tail -n+$$(( `grep -n ^description: $^ | head -n1 | cut -d: -f1` + 1 )) $^ > .$@
	head -n+$$(( `grep -n ^$$ .$@ | head -n1 | cut -d: -f1` - 1 )) .$@ > $@
	-printf ',s/        //g\n,s/^.$$//g\nw\nq\n' | ed $@
	$(RM) .$@

dist/doc/html/pktree/index.html: dist/setup-config Data/PKTree.hs
	cabal haddock --hyperlink-source

dist/setup-config: pktree.cabal
	cabal configure

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) -r dist Tests

debian/control: pktree.cabal
	cabal-debian --update-debianization

dist/build/libHSpktree-$(VERSION).a: pktree.cabal dist/setup-config Data/PKTree.hs
	cabal build

dist/pktree-$(VERSION).tar.gz: pktree.cabal dist/setup-config Data/PKTree.hs README
	cabal check
	cabal sdist
