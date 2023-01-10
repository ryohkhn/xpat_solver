# Rapport
 ## Identifiants
| Nom | Prénom | Identifiant GitLab | Numéro étudiant |
|:----------|:------|:----------|:---------| 
| VYSHKA | Tedi | @vyshka | 22011242 |
| RODRIGUEZ | Lucas | @rodrigul | 22002335 |


 ## Fonctionnalités 

Nous avons effectué le sujet minimal sans extensions.
La fonctionnalité check pour vérifier qu’un fichier .sol est valide.
La fonctionnalité solve pour rechercher des solutions pour un mode et une seed spécifique.

La recherche de solution pour le mode de jeu ‘Seahaven’ détecte seulement les parties qui ne sont pas solubles.
Certaines recherches de solutions avec les seed du mode ‘Bakers Dozens’ sont très longues, mais la plupart fonctionnent.

## Compilation et exécution

Compiler le projet avec 'dune build'
Vérifier une solution: './run -check <in_file> <game>.<seed>' 
Chercher une solution: './run -search <out_file> <game>.<seed>' 
Tester: 'dune runtest tests/<test-directory>' 
Résumé des tests: './test-summary' 

## Découpage modulaire 

XpatRandom.ml contient les fonctions qui créent la permutation initiale.
State.ml contient les fonctions de traitement des états.
Check.ml pour la vérification des solutions.
Solve.ml pour la recherche de solutions.

## Organisation

Nous avons réparti le travail de la manière suivante:
Partie 1:
1.1 et 1.2 -> VYSHKA Tedi
1.2 et 1.3 -> RODRIGUEZ Lucas
Partie 2:
Calcul des coups, compare_state -> VYSHKA Tedi
Algorithme de recherche -> RODRIGUEZ Lucas

## Misc

Le type state représente un état du jeu qui peut être modifié, avec ses colonnes, registres, dépôt et historique.
Nous avons aussi implémenté l’heuristique permettant de supprimer de la recherche les états dont le score est loin du meilleur score trouvé. Cette valeur d’écart va supprimer de la liste des états atteignables tous les états qui ont un score plus petit que le score maximum atteint moins l’écart. Cette heuristique réduit énormément l’exhaustivité de la recherche, notamment avec des petites valeurs.
Cette valeur d’écart est à entrer dans la ligne de commande avec l’option `-diff <valeur>`.
