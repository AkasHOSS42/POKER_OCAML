type rang = Deux | Trois | Quatre | Cinq | Six | Sept
            | Huit | Neuf | Dix | Valet | Dame | Roi | As;;

type couleur = Pique | Trefle | Carreau | Coeur;;

type carte = couleur * rang;;

type donne = carte * carte;;

type table = carte * carte * carte * ((carte * (carte option)) option);;

(* J'introduis le type potentiel, qui représente un ensemble de cartes
 * où on peut facilement voir les combinaisons possibles avec ces cartes.
 * La première liste représente les rangs qui sont rang d'exactement 2
 * cartes de l'ensemble (une paire), la seconde exactement 3 cartes,
 * la troisième 4. La quatrième liste représente les rangs qui apparaissent
 * au moins 1 fois dans l'ensemble, et la dernière composante partitionne
 * l'ensemble selon la couleur de ses éléments. Les listes du potentiel
 * sont toutes triées dans l'ordre décroissant. On s'en assure quand on ajoute une
 * carte à un potentiel. *)
type potentiel = (rang list) * (rang list) * (rang list) * (rang list)
                 * (couleur -> (rang list));;

val potentielVide : potentiel;;

val toInt : rang -> int;;

(* Un jeu de poker complet de 52 cartes. *)
val pioche : carte list;;

(* compare r1 r2 renvoie vrai quand r1 < r2. *)
val compare : rang -> rang -> bool;;

val delete : 'a -> 'a list -> 'a list;;

(* Ajoute une carte a un potentiel. *)
val addPotentiel : carte -> potentiel -> potentiel;;

