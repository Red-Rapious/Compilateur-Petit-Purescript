all: ppurs

ppurs:
	dune build ppurs.exe
	mv ppurs.exe ppurs

explain:
	menhir --base /tmp/parser --dump --explain parser.mly
	cat /tmp/parser.conflicts

clean:
	rm ppurs
	rm _build -d -r

test: ppurs
	./ppurs --type-only test.purs

tests:
	@echo "'make tests' n'est pas une commande valide. Essayez 'make tests1', 'make tests2', ou 'make tests3'."

tests1: ppurs
	cd tests-10-nov ; ./test.bash -1 ../ppurs

tests2: ppurs
	cd tests-10-nov ; ./test.bash -2 ../ppurs

tests3: ppurs
	cd tests-10-nov ; ./test.bash -3 ../ppurs

tests-v1: ppurs
	cd tests-10-nov ; ./test.bash -v1 ../ppurs

tests-v2: ppurs
	cd tests-10-nov ; ./test.bash -v2 ../ppurs

tests-v3: ppurs
	cd tests-10-nov ; ./test.bash -v3 ../ppurs

.PHONY: all clean explain ppurs