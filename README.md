# Compilateur-Petit-Purescript

## Exécution
- La commande `make` créé le compilateur, nommé `ppurs`, à la racine du projet.
- La commande `make test` créé et exécute le compilateur sur le fichier par défaut `test.purs`.
- La commande `make tests` exécute le compilateur sur les jeux de tests.
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
- `ast.ml` contient l'arbre de syntaxe abstraite. La distinction entre `uident` et `lident` est abandonnée par le parser dans l'AST. À part cela, l'AST est très proche de la grammaire du sujet. 
- `typing.ml` contient l'analyseur sémantique. Dans l'état actuel du projet, l'AST typé n'est pas généré, et l'analyseur se contente de vérifier la correction sémantique du programme. Ceci sera évidemment modifié pour la partie de production de code.
- `ppurs.ml` est le fichier principal qui traite les arguments de la ligne de commande, appelle successivement les différents analyseurs, et gère les éventuelles erreurs advenant au cours de la compilation.