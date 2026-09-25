# Brick Breaker

Un petit jeu d'arcade réalisé en OCaml qui reproduit le gameplay classique du casse-briques : déplacer une raquette, lancer une balle, détruire des briques et survivre le plus longtemps possible.

Ce projet est conçu comme un exercice de conception logicielle modulaire. Il met en œuvre la programmation fonctionnelle, les mises à jour d'état en temps réel, la gestion des collisions et une structure de données optimisée pour les requêtes spatiales.

## À propos du projet

- Programmation fonctionnelle en OCaml avec transitions d'état explicites
- Physique de jeu et collisions en temps réel
- Optimisation spatiale avec un quadtree pour les objets proches
- Architecture modulaire avec logique de jeu, logique de collision et point d'entrée
- Tests unitaires sur les mécanismes principaux

## Ce que fait le projet

Le joueur contrôle une raquette en bas de l'écran et doit garder une balle en jeu tout en détruisant une grille de briques. Chaque rebond modifie la trajectoire de la balle, le nombre de vies est suivi par le jeu et l'état du programme évolue à travers une boucle de mise à jour structurée.

Le dépôt est organisé selon une structure claire :

- `source/lib/` contient la logique principale du jeu, les collisions et la structure quadtree
- `source/bin/` contient le point d'entrée exécutable du jeu
- `source/dune-project` et `source/dune-workspace` définissent la configuration de compilation OCaml

## Fonctionnalités

- Gameplay classique de casse-briques
- Déplacement de la raquette avec la souris
- Physique de balle avec collisions mur/raquette/brique
- Destruction des briques et évolution de l'état du jeu
- Requêtes spatiales basées sur un quadtree
- Tests intégrés pour la logique de base
- Build léger et portable basé sur Dune

## Démarrage rapide

### Prérequis

Avant de lancer le projet, vérifiez que vous avez installé :

- OCaml
- Dune
- La bibliothèque `graphics` disponible pour l'exécution OCaml

Exemple de configuration sur un environnement Unix :

```bash
opam install dune
opam install graphics
```

### Cloner le dépôt

```bash
git clone https://github.com/tarteeg/brick_breaker.git
cd brick_breaker/source
```

### Compiler le projet

```bash
dune build
```

### Lancer le jeu

```bash
dune exec ./bin/newtonoid.exe
```

Si votre environnement expose directement le binaire, vous pouvez aussi utiliser :

```bash
dune exec newtonoid
```

## Contrôles

- Déplacez la raquette avec la souris
- Cliquez pour lancer ou relancer la balle
- Gardez la balle en jeu pour détruire toutes les briques

## Structure du dépôt

```text
brick_breaker/
├── README.md
├── main.pdf
├── notes_projet
└── source/
    ├── dune-project
    ├── dune-workspace
    ├── LISEZMOI.txt
    ├── bin/
    │   ├── dune
    │   └── newtonoid.ml
    └── lib/
        ├── debug.ml
        ├── dune
        ├── game.ml
        ├── iterator.ml
        ├── quadtree.ml
        ├── quadtree.mli
        └── types.ml
```

## Architecture du jeu

Le code est volontairement structuré pour séparer les responsabilités :

- `types.ml` définit les structures de données du jeu
- `game.ml` contient la boucle de mise à jour, les règles de collision et la logique de gameplay
- `quadtree.ml` implémente une partition spatiale pour rendre la recherche des objets proches plus efficace
- `newtonoid.ml` est le point d'entrée exécutable qui initialise et lance le jeu

Cette organisation rend le projet plus facile à tester.

## Tests et qualité logicielle

La logique du jeu contient des tests intégrés (`ppx_inline_test`) pour les comportements principaux, notamment :

- mise à jour du mouvement de la balle
- calcul du déplacement de la raquette
- gestion des collisions avec les murs
- comportement des collisions avec la raquette
- détection de contact avec les briques

Ce projet constitue donc un exemple concret de conception logicielle réaliste, testée et adaptée à un langage fonctionnel.

Aucun fichier de licence explicite n'a été trouvé dans les éléments examinés pour ce dépôt. Si vous envisagez de réutiliser ou de distribuer le projet publiquement, il convient de confirmer s'il existe une licence prévue avant toute diffusion.
