score=0
max=0

echo -n "bons "
for f in tests-10-nov/syntax/good/*.purs; do
    echo -n ".";
    max=`expr $max + 1`;
    ./ppurs --parse-only $f;
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

echo -n "Parsing: $score/$max : $percent%\n";