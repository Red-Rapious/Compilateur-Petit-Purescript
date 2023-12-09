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

tests1: ppurs
	cd tests-10-nov ; ./test.bash -1 ../ppurs

tests2: ppurs
	cd tests-10-nov ; ./test.bash -2 ../ppurs

tests3: ppurs
	cd tests-10-nov ; ./test.bash -2 ../ppurs

.PHONY: all clean explain ppurs