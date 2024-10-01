open Construction;;
open Compute;;

(* La premiere composante renvoyee represente le type
 * de la combinaison. Plus elle est petite, plus le type est fort.
 * Si les types de deux combinaisons sont les memes, on regarde le maximum
 * lexicographique des deuxiemes composantes, ou il y a les rangs
 * importants. *)
let toComparable com =
  match com with
    QuinteFlush r -> (1, [toInt r])
   |Carre (r1, r2) -> (2, [toInt r1; toInt r2])
   |Full (r1, r2) -> (3, [toInt r1; toInt r2])
   |Couleur (r1, r2, r3, r4, r5) -> (4, [toInt r1; toInt r2; toInt r3; toInt r4; toInt r5])
   |Suite r -> (5, [toInt r])
   |Brelan (r1, r2, r3) -> (6, [toInt r1; toInt r2; toInt r3])
   |DPaire (r1, r2, r3) -> (7, [toInt r1; toInt r2; toInt r3])
   |Paire (r1, r2, r3, r4) -> (8, [toInt r1; toInt r2; toInt r3; toInt r4])
   |CarteHaute (r1, r2, r3, r4, r5) -> (9, [toInt r1; toInt r2; toInt r3; toInt r4; toInt r5]);;

(* Renvoie -1 si com2>com1
 * 0 si com2 ~ com1
 * 1 sinon. *)
let compareComb com1 com2 =
  let (val1, list1) = toComparable com1
  and (val2, list2) = toComparable com2
  in let rec compareList l1 l2 =
       match (l1, l2) with
         (v1 :: l3, v2 :: l4) ->
          if v1 < v2
          then -1
          else if v1 > v2
          then 1
          else compareList l3 l4
        |([], []) -> 0
        |([], _) -> -1
        |(_, []) -> 1
     in if val1 < val2
        then 1
        else if val1 > val2
        then -1
        else compareList list1 list2;;

let comparePot pot1 pot2 = compareComb (computeComb pot1) (computeComb pot2);;
