La solution en quelques mots
----------------------------

En gros le principe c'était juste :
* choisir des points au pif sur la carte
* dispatcher les voitures le plus vite possible vers là-bas (à coups de A\*)
* faire localement avancer les voitures par marche aléatoire en prenant des rues pas encore visitées
* une fois qu'une voiture était bloquée : A\* pour rejoindre la rue non visitée la plus proche, et c'est reparti

Il y a une bonne partie du processus de génération de la solution qui
est invisible sur le code, du genre une fois qu'on avait un choix
random de paramètre qui faisait un truc bien on s'en servait pour les
réinjecter dans les itérations suivantes afin d'essayer d'atteindre le
max local en étant chanceux.

En gros c'est tout. On a eu quelques surprises, comme observer que
utiliser un A\* au lieu d'un BFS pour débloquer les voitures
améliorait le résultat. On a aussi essayé d'avoir les voitures qui
tournaient en spirale mais on s'est gouré sur le calcul de déterminant
(*ab-cd* au lieu de *ad-bc*) donc ça faisait n'importe quoi. Ce bug a
été trouvé après la fin de l'épreuve.

Le code de chaque variante tient en 100 lignes, c'est du Haskell dense
donc pas très lisible mais en connaissant l'intention ça se déchiffre
pas trop mal. La solution finale est probablement solution-astar.hs,
je ne m'en souviens plus.

