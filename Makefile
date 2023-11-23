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
	./ppurs --parse-only test.purs

tests: ppurs
	cd tests-10-nov ; ./test.bash -1 ../ppurs

.PHONY: all clean explain ppurs