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
  -help          Display this list of options
  --help         Display this list of options
```