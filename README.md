# Compilateur Petit Purescript
Ceci est le compilateur Petit Purescript réalisé par Matthieu Boyer et Antoine Groudiev, pour le projet du cours "Langages de Programmation et Compilation" de 2023-2024.

## Exécution
- La commande `make` créé le compilateur, nommé `ppurs`, à la racine du projet.
- La commande `make test` créé et exécute le compilateur sur le fichier par défaut `test.purs`.
- La commande `make testc` créé et exécute le compilateur sur le fichier par défaut `test.purs`, et lance l'exécution avec `gcc`.
- La commande `make testd` est similaire à `make testd` mais active le mode `dbg`, qui *pretty print* l'AST.
- La commande `make tests1` exécute le compilateur sur les jeux de tests de la partie 1 (respectivement 2 et 3).
- La commande `make clean` supprime les fichiers créés par `make`

Une fois créé, le compilateur peut être exécuté en lançant une commande de la forme :
```
./ppurs [options] file.purs
```
Les options disponibles sont les suivantes :
```
  --parse-only   Réaliser uniquement la phase d'analyse syntaxique
  --type-only    Réaliser uniquement l'analyse syntaxique et sémantique
```

## Choix techniques
Le langage utilisé est OCaml. L'analyse lexicale est faite à l'aide de l'outil `ocamllex`, et l'analyse syntaxique avec Menhir.
### Description des fichiers source
Les fichiers sources du projet sont les suivants :
- `lexer.mll` contient un analyseur lexical traditionnel, dans le sens où il ne gère pas l'indentation significative.
- `indenter.ml` contient un second analyseur lexical. Son rôle est de traiter correctement l'indentation significative propre à PureScript, en générant à la volée les tokens factice `SEMICOLON` (`;`), `LBRACK` (`{`) et `RBRACK` (`}`). La fonction principale de ce fichier, `traiter_lexeme`, reproduit le comportement attendu d'un analyseur lexical. Elle a sensiblement le même type que `Lexer.token`, a l'exception du booléen précisant le type (fort ou faible) voulu.
- `parser.mly` contient l'analyseur syntaxique. Son comportement est standard et suit majoritairement la grammaire PureScript telle que détaillée dans le sujet. Une exception notable est la règle `tdecl`, qui nous a posé des difficultés à parser. La technique adoptée a été de lire la liste à la main, à l'aide d'une règle `type_list` qui implémente manuellement et plus finement la construction `list` de Menhir. Pour pouvoir localiser précisément les erreurs de typage, les types `expr`, `atom`, et `patarg` sont transformés par le parser en `loc_expr`, `loc_atom`, et `loc_patarg`, qui sont décorés des informations de localisation.
- `ast.ml` contient les arbres de syntaxe abstraite, qui sont au nombre de trois, générés respectivement lors de l'analyse lexicale, du typage, et de l'allocation des variables. La distinction entre `uident` et `lident` est abandonnée par le parser dans l'AST. À part cela, l'AST du parser est très proche de la grammaire du sujet. Le typage décore l'AST avec des types (`ttyp`) utilisés par la production de code, et supprime les informations de localisation. L'allocation décore l'AST avec les adresses allouées sur la pile. 
- `typing.ml` contient l'analyseur sémantique. L'algorithme employé est le suivant : on type une à une les déclarations du fichier, en maintenant dans des tables de hachage, qui contiennent les définitions de fonctions, de types, de classes et d'instances. On vérifie à chaque étape que la liste des définitions à l'intérieur d'une déclaration (`fdecl`, `clas`, `data`, `instance` dans le type `decl`) est bien regroupée par nom de fonction, et que celle-ci ne déborde pas de la déclaration. Dans chaque définition (`defn` dans le type `decl`), on vérifie récursivement le type de chaque morceau selon la construction de l'arbre de syntaxe. 
- `compile.ml` réalise l'allocation des variables sur la pile et la production du code assembleur. Certaines fonctions sont codées directement en assembleur : à la fois des fonctions appartenant à Purescript (`log`, `pure`, `not`, `mod`), et des fonctions utilitaires (affichage d'entier, de booléens et division).
- `ppurs.ml` est le fichier principal qui traite les arguments de la ligne de commande, appelle successivement les différents analyseurs, et gère les éventuelles erreurs advenant au cours de la compilation.
- `pretty.ml` contient un *pretty printer* destiné à l'affiche de l'AST après la phase d'allocation. Il affiche, en couleur, la hierarchie des constructions et les "adresses" des variables et autres données allouées. Il s'est avéré très utile lors des nombreuses phases de déboggage. 
- `utility.ml` contient des fonctions utilitaires servant notamment à extraire l'addresse ou le type d'une expression, d'un atome, etc.

### Limitations connues
- *Certains tests de typages ne renvoient pas encore le bon résultat.*
- *Certaines erreurs ne sont pas localisées, ce qui se voit à l'emploi temporaire de `placeholder_loc` dans les exceptions.*