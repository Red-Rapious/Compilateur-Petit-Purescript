all: ppurs
	dune exec ./ppurs

ppurs:
	dune build ppurs.exe
	mv ppurs.exe ppurs

explain:
	menhir --base /tmp/parser --dump --explain parser.mly
	cat /tmp/parser.conflicts

clean:
	dune clean

.PHONY: all clean explain ppurs