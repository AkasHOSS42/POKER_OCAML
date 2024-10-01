#load "construction.cmo";;
#load "compute.cmo";;
#load "compare.cmo";;

(* On ecrit le code de la calculette a plusieurs joueurs. *)
(* Je recommande de lire extension.pdf d'abord. *)

open Construction;;
open Compare;;
open Array;;

let addAll = List.fold_right addPotentiel;;

(* On fait s'affronter les potentiels de t.
 * On renvoie la position du vainqueur strict,
 * si il existe. *)
let tournoi t length =
  let duel pos1 pos2 =
    let res = comparePot t.(pos1) t.(pos2)
    in if res > 0
       then Some pos1
       else if res < 0
       then Some pos2
       else None


  (* Tournoi dans le sous-tableau [first, last].
   * Le booleen renvoye en seconde composante sert a
   * dire si le vainqueur est strict. *)
  in let rec aux first last =
       if first = last
       then (first, true)
       else if first - last = 1
       then match duel first last with
              None -> (first, false)
             |Some pos -> (pos, true)
       else let left = aux first ((last + first) / 2)
            and right = aux (((last + first) / 2) + 1) last
            in match (left, right) with
                 ((pos1, best1), (pos2, best2)) ->
                  match duel pos1 pos2 with
                    None -> (pos1, false)
                   |Some pos ->
                     if pos = pos1
                     then (pos, best1)
                     else (pos, best2)


     in match aux 0 (length - 1) with
          (pos, best) ->
          if best
          then Some pos
          else None;;

(* Donne la pioche a parcourir,
 * en supprimant les cartes connues. *)
let getDeck donnes table =
  let rec aux deck = function
      [] -> deck
     |d :: l -> match d with
                  (c1, c2) -> aux (delete c1 (delete c2 deck)) l
  in let deck = aux pioche donnes
     in match table with
          (tc1, tc2, tc3, tcopt) ->
          (let deck = List.fold_right delete [tc1; tc2; tc3] deck
           in (match tcopt with
                 None -> deck
                |Some (tc4, tcopt) ->
                  let deck = delete tc4 deck
                  in (match tcopt with
                        None -> deck
                       |Some tc5 -> delete tc5 deck)));;

(* Calcule le produit des entiers de [[start, finish]] (en flottant). *)
let produitRange start finish =
  let rec aux ans i =
    if i = finish
    then ans *. finish
    else aux (ans *. i) (i +. 1.)
  in aux 1. start;;

(* Puissance de 2. *)
let rec pow n =
  if n = 0
  then 1
  else 2 * (pow (n - 1));;

(* Calcule le nombre da manieres
 * de remplir les donnes inconnues, pour une
 * table remplie. *)
let nbCas nbVar nbCST =
  if nbVar = 0
  then 1.
  else let nbVar = float_of_int nbVar
       and nbCST = float_of_int nbCST
       and p = pow nbVar
       in let nbCartes = 47. -. (2. *. nbCST)
          in (produitRange ((nbCartes -. (2. *. nbVar)) +. 1.) nbCartes)
             /. ((float_of_int p) *. (produitRange 1. nbVar));;

(* Approxime la taille maximale d'un entier. *)
let upperBound =
  let rec aux n =
    if (2 * n) > 0
    then aux (2 * n)
    else n
  in aux 1;;

(* Prends en argument :
 * n le nombre de joueurs.
 * donnes la liste des donnes connues.
 * table la table.
 * Renvoie la liste des probabilites
 * pour chaque joueur connu
 * de gagner strictement.
 * On utilise un peu de code imperatif :
 *
 ** L'ensemble des scores est un tableau
 ** car on veut pouvoir acceder a n'importe quel
 ** score instantanement.
 *
 ** Les ensembles de mains (connues ou pas)
 ** sont des tableaux de potentiels,
 ** car on veut determiner le potentiel
 ** le plus fort par un tournoi (plus rapide).
 *
 ** On compte aussi le nombre de cas parcourus
 ** avec un pointeur omega.
 ** Les fonctions de parcours ont pour
 ** valeur de retour ()
 ** et incrementent omega et les scores. *)
let proba_multiple n donnes table =
  let omega = ref 0.
  and nbCST = List.length donnes
  and pioche = getDeck donnes table
  (* Cartes en commun a tous les joueurs. *)
  and common = match table with
      (tc1, tc2, tc3, tcopt) ->
      let res = addAll [tc1; tc2; tc3] potentielVide
      in (match tcopt with
            None -> res
           |Some (tc4, tcopt) -> let res = addPotentiel tc4 res
                                 in (match tcopt with
                                       None -> res
                                      |Some tc5 -> addPotentiel tc5 res
                                    )
         )



  (* Premiere ligne pour une table a trois cartes.
   * Deuxieme pour quatre cartes.
   * Troisieme pour cinq. *)
  in let mainsCST = make_matrix 3 nbCST potentielVide
     and scoresInt = make nbCST 0
     and scoresFloat = make nbCST 0.
     and nbVar = n - nbCST



     in (let indice = (* Ligne de mainsCST a initialiser. *)
           match table with
             (_, _, _, None) -> 0
            |(_, _, _, Some (_, None)) -> 1
            |(_, _, _, Some (_, Some _)) -> 2
         in (let rec init i donnes =
               match donnes with
                 [] -> ()
                |donne :: donnes ->
                  match donne with
                    (c1, c2) -> (mainsCST.(indice).(i) <- addAll [c1; c2] common;
                                 init (i + 1) donnes)
             in init 0 donnes));



        let nb = nbCas nbVar nbCST
        and augmenteScore indice =
          scoresInt.(indice) <- scoresInt.(indice) + 1;
          if scoresInt.(indice) = upperBound
          then (scoresFloat.(indice) <-
                  scoresFloat.(indice) +. (float_of_int upperBound);
                scoresInt.(indice) <- 0)
          else ()


        (* Premiere ligne pour une donne inconnue a zero cartes.
         * Deuxieme pour une.
         * Troisieme pour deux. *)
        and mainsVar = make_matrix 3 (nbVar + 1) potentielVide


           (* Incremente les scores en fonction de la situation.
            * posWinner est la position dans mainsCST du vainqueur
            * strict du tournoi des donnes connues. *)
        in let incremente posWinner =
             match tournoi mainsVar.(2) (nbVar + 1) with
               None -> ()
              |Some pos -> if pos = nbVar
                           then augmenteScore posWinner
                           else ()





           (* Les deux fonctions qui suivent
            * servent a remplir les donnes inconnues.
            * posVar est le numero de la donne inconnue. *)
           in let rec premierParcours deuxiemeParcours deck posVar posWinner =
                if posVar = nbVar (* Toutes les donnes remplies. *)
                then incremente posWinner
                else match deck with
                       [] -> ()
                      |c :: d ->
                        (mainsVar.(1).(posVar) <- addPotentiel c mainsVar.(0).(posVar);
                         (* On remplit la deuxieme carte. *)
                         deuxiemeParcours d d posVar posWinner;
                         (* On choisit une autre premiere carte. *)
                         premierParcours deuxiemeParcours d posVar posWinner)
              in let rec deuxiemeParcours fullDeck deck posVar posWinner =
                   match deck with
                     [] -> ()
                    |c :: d ->
                      (mainsVar.(2).(posVar) <- addPotentiel c mainsVar.(1).(posVar);
                       (* On remplit la donne suivante. *)
                       premierParcours deuxiemeParcours (delete c fullDeck) (posVar + 1) posWinner;
                       (* On choisit une autre seconde carte. *)
                       deuxiemeParcours fullDeck d posVar posWinner)





                 in let rec remplirTable common table deck fullDeck =
                      match table with
                        (tc1, tc2, tc3, None) ->
                         (match deck with
                            [] -> ()
                           |tc4 :: d -> (for i = 0 to nbCST - 1 do
                                           mainsCST.(1).(i) <- addPotentiel tc4 mainsCST.(0).(i)
                                         done;
                                         remplirTable (addPotentiel tc4 common)
                                           (tc1, tc2, tc3, Some (tc4, None)) d (delete tc4 fullDeck);
                                         remplirTable common table d fullDeck
                                        )
                         )
                       |(tc1, tc2, tc3, Some (tc4, None)) ->
                         (match deck with
                            [] -> ()
                           |tc5 :: d -> (for i = 0 to nbCST - 1 do
                                           mainsCST.(2).(i) <- addPotentiel tc5 mainsCST.(1).(i)
                                         done;
                                         remplirTable (addPotentiel tc5 common)
                                           (tc1, tc2, tc3, Some (tc4, Some tc5)) d (delete tc5 fullDeck);
                                         remplirTable common table d fullDeck
                                        )
                         )
                       |(tc1, tc2, tc3, Some (tc4, Some tc5)) ->
                         (omega := !omega +. nb;
                          match tournoi mainsCST.(2) nbCST with
                            None -> ()
                           |Some pos ->
                             (for i = 0 to nbVar - 1 do
                                mainsVar.(0).(i) <- common
                              done;
                              mainsVar.(2).(nbVar) <- mainsCST.(2).(pos);
                              premierParcours deuxiemeParcours fullDeck 0 pos
                             )
                         )






                    in remplirTable common table pioche pioche;
                       for i = 0 to nbCST - 1 do
                         scoresFloat.(i) <-
                           scoresFloat.(i) +. (float_of_int scoresInt.(i))
                       done;
                       let rec aux i res =
                         if i = 0
                         then res
                         else aux (i - 1) ((scoresFloat.(i-1)
                                            /. !omega) :: res)
                       in aux nbCST [];;
