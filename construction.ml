type rang = Deux | Trois | Quatre | Cinq | Six | Sept
            | Huit | Neuf | Dix | Valet | Dame | Roi | As;;

type couleur = Pique | Trefle | Carreau | Coeur;;

type carte = couleur * rang;;

type donne = carte * carte;;

type table = carte * carte * carte * ((carte * (carte option)) option);;

(* J'introduis le type potentiel, qui represente un ensemble de cartes
 * ou on peut facilement voir les combinaisons possibles avec ces cartes.
 * La premiere liste represente les rangs qui sont rang d'exactement 2
 * cartes de l'ensemble (une paire), la seconde exactement 3 cartes,
 * la troisième 4. La quatrieme liste represente les rangs qui apparaissent
 * au moins 1 fois dans l'ensemble, et la dernière composante partitionne
 * l'ensemble selon la couleur de ses elements. Les listes du potentiel
 * sont toutes triees dans l'ordre decroissant. On s'en assure quand on ajoute une
 * carte à un potentiel. *)
type potentiel = (rang list) * (rang list) * (rang list) * (rang list)
                 * (couleur -> (rang list));;

let potentielVide = ([], [], [], [], fun x-> []);;

let toInt = function
    Deux -> 2
   |Trois -> 3
   |Quatre -> 4
   |Cinq -> 5
   |Six -> 6
   |Sept -> 7
   |Huit -> 8
   |Neuf -> 9
   |Dix -> 10
   |Valet -> 11
   |Dame -> 12
   |Roi -> 13
   |As -> 14;;

(* Fonction auxiliaire pour construire la pioche. *)
let toRang n =
  match n with
    2 -> Deux
   |3 -> Trois
   |4 -> Quatre
   |5 -> Cinq
   |6 -> Six
   |7 -> Sept
   |8 -> Huit
   |9 -> Neuf
   |10 -> Dix
   |11 -> Valet
   |12 -> Dame
   |13 -> Roi
   |14 -> As
   |_ -> failwith "Probleme";;

(* Un jeu de poker complet de 52 cartes. *)
let pioche =
  let rec aux list = match list with
      [] -> aux [(Trefle, Deux)]
     |(col, rg) :: _ -> if (toInt rg) < 14
                        then aux ((col, toRang ((toInt rg) + 1)) :: list)
                        else let newCol = match col with
                                 Trefle -> Pique
                                |Pique -> Coeur
                                |Coeur -> Carreau
                                |Carreau -> Trefle
                             in if newCol = Trefle
                                then list
                                else aux ((newCol, Deux) :: list)
  in aux [];;

let compare rg1 rg2 = (toInt rg1) < (toInt rg2);;

(* List est triee. ordre a b = true ssi a < b, si on veut une liste decroissante. *)
let rec insert ordre elt list =
  match list with
    [] -> [elt]
   |e :: l -> if ordre e elt
              then elt :: list
              else if e = elt
              then list
              else e :: (insert ordre elt l);;

let rec delete elt list =
  match list with
    [] -> list
   |e :: l -> if e = elt
              then l
              else e :: delete elt l;;

(* Meme chose, list est triee decroissante et ordre est comme < . *)
let rec deleteSorted ordre elt list =
  match list with
    [] -> []
   |e :: l -> if e = elt
              then l
              else if ordre e elt
              then list
              else e :: (deleteSorted ordre elt l);;

(* Ajoute une carte à un potentiel. *)
let addPotentiel (col, rg) = function
    (paires, triplets, quadruplets, suites, couleurs) ->
    let add = insert compare rg
    and remove = deleteSorted compare rg
    and contains = List.mem rg
    in let newCol c = if c = col
                      then add (couleurs c)
                      else couleurs c
       and newS = add suites
       and (newP, newT, newQ) =
         if contains paires (* Le nombre d'occurences de rg passe de 2 a 3. *)
         then (remove paires, add triplets, quadruplets)
         else if contains triplets (* Pareil, de 3 a 4. *)
         then (paires, remove triplets, add quadruplets)
         else if contains suites (* Pareil, de 1 a 2. *)
         then (add paires, triplets, quadruplets)
         else (paires, triplets, quadruplets)
       in (newP, newT, newQ, newS, newCol);;


