echo "======= SYNTAX ==========\n\n"
score=0
max=0

echo -n "== BONS ==\n"
for f in tests-10-nov/syntax/good/*.purs; do
    echo -n ".";
    max=`expr $max + 1`;
    ./ppurs --parse-only $f 2>/dev/null;
    case $? in
	"1")
	echo
	echo "ECHEC sur "$f" (devrait reussir)";;
	"0") score=`expr $score + 1`;;
	*)
	echo
	echo "ECHEC sur "$f" (pour une mauvaise raison)";;
    esac
done

echo

percent=`expr 100 \* $score / $max`;
echo -n "Parsing des bons : $score/$max : $percent%\n";

score=0
max=0

# les mauvais
echo -n "== MAUVAIS ==\n "
for f in tests-10-nov/syntax/bad/*.purs; do
    echo -n ".";
    max=`expr $max + 1`;
    ./ppurs --parse-only $f 2>/dev/null;
    case $? in
	"0")
	echo
	echo "ECHEC sur "$f" (devrait échouer)";;
	"1") score=`expr $score + 1`;;
	*)
	echo
	echo "ECHEC sur "$f" (pour une mauvaise raison)";;
    esac
done
echo


percent=`expr 100 \* $score / $max`;
echo -n "Parsing des mauvais : $score/$max : $percent%\n";

echo "\n\n======= TYPING ==========\n\n"
score=0
max=0

echo -n "== BONS ==\n"
for f in tests-10-nov/typing/good/*.purs; do
    echo -n ".";
    max=`expr $max + 1`;
    ./ppurs --parse-only $f 2>/dev/null;
    case $? in
	"1")
	echo
	echo "ECHEC sur "$f" (devrait reussir)";;
	"0") score=`expr $score + 1`;;
	*)
	echo
	echo "ECHEC sur "$f" (pour une mauvaise raison)";;
    esac
done

echo

percent=`expr 100 \* $score / $max`;
echo -n "Typing des bons : $score/$max : $percent%\n";

score=0
max=0

# les mauvais
echo -n "== MAUVAIS ==\n "
for f in tests-10-nov/typing/bad/*.purs; do
    echo -n ".";
    max=`expr $max + 1`;
    ./ppurs --parse-only $f 2>/dev/null;
    case $? in
	"0")
	echo
	echo "ECHEC sur "$f" (devrait échouer)";;
	"1") score=`expr $score + 1`;;
	*)
	echo
	echo "ECHEC sur "$f" (pour une mauvaise raison)";;
    esac
done
echo

percent=`expr 100 \* $score / $max`;
echo -n "Typing des mauvais : $score/$max : $percent%\n";