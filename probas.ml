open Construction;;
open Compare;;

let addAll = List.fold_right addPotentiel;;

(* La fonction de parcours de la pioche que l'on utilisera.
 * n designe la profondeur du parcours.
 * value est la valeur qu'on incremente a chaque etape,
 * et qu'on renverra a la fin.
 * list est la liste a parcourir.
 * fonc sert a incrementer value.
 * Les elements parcourus courant de list sont stockes dans auxList.
 * Quand la profondeur atteint 0, on vide auxList en mettant
 * ses elements dans fonc. *)
let rec parcours n value auxList list fonc =
  match list with
    [] -> value
   |e :: l ->
     if n < 1
     then parcours n (fonc value (e :: auxList))
            auxList l fonc (* Vidage de auxList *)
     else parcours n
            (parcours (n-1) value (e :: auxList) l fonc)
            auxList l fonc;;

(* La fonction "fonc" qu'on utilisera dans le parcours dans
 * le calcul de la proba double.
 * v3 -> le nombre de combinaisons possibles.
 * v1 -> le nombre de combinaisons pour lesquelles
 * pot1 est plus fort que pot2.
 * pareil pour v2. *)
let addRes pot1 pot2 (v1, v2, v3) =
  let toAdd = comparePot pot1 pot2
  in if toAdd < 0
     then (v1, v2 + 1, v3 + 1)
     else if toAdd > 0
     then (v1 + 1, v2, v3 + 1)
     else (v1, v2, v3 + 1);;

(* On construit le potentiel partiellement rempli
 * des deux joueurs, puis on parcours la pioche
 * pour remplir ces potentiels, et calculer nos probabilites. *)
let proba_double (d1c1, d1c2) (d2c1, d2c2) (tc1, tc2, tc3, topt) =
  let common = addAll [tc1; tc2; tc3] potentielVide
  and pioche = List.fold_right delete [d1c1; d1c2; d2c1; d2c2; tc1; tc2; tc3] pioche
  in let pot1 = addAll [d1c1; d1c2] common
     and pot2 = addAll [d2c1; d2c2] common
     in let triplet =
          let aux p1 p2 value l = addRes (addAll l p1) (addAll l p2) value (* fonc pour le parcours. *)
          in match topt with
               None -> parcours 1 (0, 0, 0) [] pioche (aux pot1 pot2) (* Trois cartes sur table. *)
              |Some (tc4, topt) ->
                let pot1 = addPotentiel tc4 pot1
                and pot2 = addPotentiel tc4 pot2
                and pioche = delete tc4 pioche
                in match topt with
                     None -> parcours 0 (0, 0, 0) [] pioche (aux pot1 pot2) (* Quatre cartes. *)
                    |Some tc5 -> addRes (addPotentiel tc5 pot1) (addPotentiel tc5 pot2) (0, 0, 0) (* Cinq. *)
        in match triplet with
             (res1, res2, omega) ->
             let res1 = float_of_int res1
             and res2 = float_of_int res2
             and omega = float_of_int omega
             in (res1 /. omega, res2 /. omega);;

(* Sert a la fonction "fonc" de proba_simple. *)
let addRes2 pot1 pot2 (v1, v2) =
  let toAdd = comparePot pot1 pot2
  in if toAdd > 0
     then (v1 + 1, v2 + 1)
     else (v1, v2 + 1);;

(* Ici, on utilisera un parcours dans un parcours.
 * Le premier remplira la donne inconnue.
 * Le second remplira la table.
 * Pour trois cartes sur table, le temps d'execution
 * est de moins de 25 secondes. *)
let proba_simple (dc1, dc2) (tc1, tc2, tc3, topt) =
  let common = addAll [tc1; tc2; tc3] potentielVide
  and pioche = List.fold_right delete [dc1; dc2; tc1; tc2; tc3] pioche
  in let couple =
       (* "fonc" pour le parcours de remplissage de la table. *)
       let aux p1 p2 value l = addRes2 (addAll l p1) (addAll l p2) value
       in let bigAux com n deck value l = parcours n value [] (* "fonc" pour celui de la donne. *)
                                            (List.fold_right delete l deck)
                                            (aux (addAll [dc1; dc2] com) (addAll l com))
          in match topt with
               None -> parcours 1 (0, 0) [] pioche (bigAux common 1 pioche) (* Trois cartes sur table. *)
              |Some (tc4, topt) ->
                let common = addPotentiel tc4 common
                and pioche = delete tc4 pioche
                in match topt with
                     None -> parcours 1 (0, 0) [] pioche (* Quatre. *)
                               (bigAux common 0 pioche)
                    |Some tc5 ->
                      let common = addPotentiel tc5 common (* Cinq. *)
                      and pioche = delete tc5 pioche
                      in let bigAux value l = addRes2 (addAll [dc1; dc2] common) (addAll l common) value
                         in parcours 1 (0, 0) [] pioche bigAux
     in match couple with
          (res, omega) -> let res = float_of_int res
                          and omega = float_of_int omega
                          in res /. omega;;
