open Construction;;

type comb = QuinteFlush of rang
          | Carre of rang * rang
          | Full of rang * rang
          | Couleur of rang * rang * rang * rang * rang
          | Suite of rang
          | Brelan of rang * rang * rang
          | DPaire of rang * rang * rang
          | Paire of rang * rang * rang * rang
          | CarteHaute of rang * rang * rang * rang * rang;;

(* Fonction secondaire utilisée dans computeQuinteFlush.
 * Renvoie le plus grand des rang option. *)
let union o1 o2 =
  match o1 with
    None -> o2
   |Some r1 -> match o2 with
                 None -> o1
                |Some r2 -> if (compare r1 r2)
                            then o2
                            else o1;;

(* Cherche une quinte dans une liste triée décroissante.
 * Renvoie le rang de la plus grande quinte trouvée. (None si rien n'est trouvé) *)
let extraitSuite list =
  (* Parcours la liste et retient les rangs consecutifs,
   * jusqu'a ce qu'il cinq (une quinte).
   * max est le rang de la sous-quinte courante.
   * prev est le rang de l'element precedent a celui parcouru.
   * nb est le nombre d'element consecutifs de la sous-quinte courante.
   * list2 est la sous-liste qu'il reste a parcourir. *)
  let rec aux max prev nb list2 =
    match list2 with
      [] -> None
     |rg :: l ->
       let r1 = (toInt prev)
       and r2 = (toInt rg)
       in if r1 = r2 + 1 (* Le rang est consecutif. *)
          then if nb = 4 (* Quinte trouvee. *)
               then Some max
               else aux max rg (nb + 1) l
          else aux rg rg 1 l (* On remet le compte a un. *)


  in match aux Deux Deux 0 list with
       None -> (match list with (* On cherche le cas particulier ou l'as vaut un. *)
                  [] -> None
                 |rg :: l ->
                   if rg = As

                   then let rec sousListeCinq list2 =
                          match list2 with
                            [] -> []
                           |r :: l2 ->
                             if r = Cinq
                             then list2
                             else if (toInt r) < 5
                             then []
                             else sousListeCinq l2


                        in aux Cinq Six 1 (sousListeCinq l)
                   else None)
      |Some r -> Some r;;

(* Renvoie le maximum (lexicographique) de deux listes.
 * On l'utilisera sur des listes triées décroissantes. *)
let rec maxList l1 l2 =
  match l1 with
    [] -> l2
   |a :: list1 ->
     match l2 with
       [] -> l1
      |b :: list2 ->
        if compare a b
        then l2
        else if a = b
        then if maxList list1 list2 = list1
             then l1
             else l2
        else l1;;

(* Quand on fait une paire/brelan/..., on veut "remplir" notre main
 * avec des cartes de plus haut rang possible, pour obtenir une combinaison
 * de 5 cartes. Cette fonction nous renvoie la liste "bouche-trou" recherchée.
 * On ne l'utilise que sur des listes triées décroissantes. toSkip et toSkip2opt
 * donnent le/les un/deux rang(s) qu'on veut skip dans la liste. Par exemple, si je
 * fais une double paire, je ne peux pas remplir ma main avec des cartes de même rang
 * que l'une de mes paires : j'ai deux valeurs à skip. getMoreOpt nous indique combien
 * de rangs doit contenir notre liste (1 pour un Carré ou une double paire, 2 pour un brelan ...).
 * getMoreOpt a pour type un type qui n'a que 3 valeurs : bool option. *)
let rec getMaxWithout toSkip toSkip2opt getMoreOpt = function
    [] -> failwith "Main trop petite."
   |rg::l ->
     let skipIt = if rg = toSkip (* On doit skip rg. *)
                  then true
                  else match toSkip2opt with
                         None -> false (* On ne doit pas le skip. *)
                        |Some toSkip2 -> toSkip2 = rg
     in if skipIt
        then getMaxWithout toSkip toSkip2opt getMoreOpt l
        else match getMoreOpt with
               None -> [rg] (* Un seul element a rechercher. *)
              |Some getThird ->
                let newMoreOpt = if getThird (* 3 elements a rechercher. *)
                                 then Some false (* Plus que deux maintenant. *)
                                 else None (* On en avait deux a rechercher, plus qu'un maintenant. *)
                in rg :: getMaxWithout toSkip toSkip2opt newMoreOpt l;;

(* A part computePaires, les fonctions de recherche de combinaison
 * suivantes renvoient une comb option. Leur fonctionnement n'est pas garanti
 * si on les utilise sur des potentiels illegaux (par exemple, moins de 5 cartes).
 * De plus, on n'utilisera une de ces fonctions que sur des potentiels
 * qui ne contiennent pas de combinaison meilleure que la combinaison recherchee.
 * Par exemple, on ne recherchera pas une carte haute dans un potentiel qui contient
 * un full. cf la fonction computeComb tout en bas pour voir comment on procede. *)

let computeQuinteFlush (_, _, _, _, couleurs) =
  match union (union (extraitSuite (couleurs Pique))
                 (extraitSuite (couleurs Coeur)))
          (union (extraitSuite (couleurs Trefle))
             (extraitSuite (couleurs Carreau))) (* On cherche la meilleure quinte. *)
  with Some r -> Some (QuinteFlush r) (* On l'a trouvee. *)
      |None -> None;; (* Il n'y en a pas. *)

let computeCarre (_, _, quadruplets, suites, c) =
  match quadruplets with
    q::_ -> (match getMaxWithout q None None suites with
               [] -> failwith "Main trop petite."
              |rg :: _ -> Some (Carre (q, rg))) (* On remplit notre main avec une carte. *)
   |[] -> None;;

(* Ici, il faut faire attention au cas ou on a deux triplettes, et la paire qui remplit
 * notre full est a prendre dans ces triplettes. *)
let computeFull (paires, triplets, _, _, _) =
  match (paires, triplets) with
    (p :: l, t :: []) -> Some (Full (t, p))
   |([], t1 :: t2 :: _) -> Some (Full (t1, t2))
   |(l1, t :: l2) -> (match maxList l1 l2 with
                        r :: _ -> Some (Full (t, r))
                       |[] -> None)
   |(_, _) -> None;;

let computeCouleur (_, _, _, _, couleurs) =
  let topCinq = function
      r1 :: r2 :: r3 :: r4 :: r5 :: _ -> r1 :: r2 :: r3 :: r4 :: r5 :: []
     |_ :: _|[] -> []
  in let lP = topCinq (couleurs Pique)
     and lCo = topCinq (couleurs Coeur)
     and lCa = topCinq (couleurs Carreau)
     and lT = topCinq (couleurs Trefle)
     in match maxList (maxList lT lCo) (maxList lP lCa) with (* Maximum lexicographique. *)
          r1 :: r2 :: r3 :: r4 :: r5 :: _ -> Some (Couleur (r1, r2, r3, r4, r5))
         |_ :: _|[] -> None;;

let computeSuite (_, _, _, suites, _) =
  match extraitSuite suites with
    Some r -> Some (Suite r)
   |None -> None;;

let computeBrelan (_, triplets, _, suites, _) =
  match triplets with
    t :: _ -> (match getMaxWithout t None (Some false) suites with (* On remplit la main avec deux cartes. *)
                 r1 :: r2 :: _ -> Some (Brelan (t, r1, r2))
                |[]|_::_ -> failwith "Main trop petite.")
   |[] -> None;;

(* On cherche d'abord une double paire,
 * puis une paire, puis si on ne trouve rien,
 * on renvoie une carte haute. *)
let computePaires (paires, _, _, suites, _) =
  match paires with
    p1 :: p2 :: _ -> (match getMaxWithout p1 (Some p2) None suites with (* On remplit la main avec une carte. *)
                        r :: _ -> DPaire (p1, p2, r)
                       |[] -> failwith "Main trop petite.")
   |p :: _ -> (match getMaxWithout p None (Some true) suites with (* On remplit la main avec trois cartes. *)
                 r1 :: r2 :: r3 :: _ -> Paire (p, r1, r2, r3)
                |_ :: _|[] -> failwith "Main trop petite.")
   |[] -> match suites with
            r1 :: r2 :: r3 :: r4 :: r5 :: _ -> CarteHaute (r1, r2, r3, r4, r5)
           |_ :: _|[] -> failwith "Main trop petite.";;

(* On cherche d'abord une quinte flush.
 * Si on n'en trouve pas, on cherche un Carré.
 * Si on n'en trouve pas ... etc *)
let computeComb pot =
  let rec aux list =
    match list with
      [] -> computePaires pot
     |e :: l -> match e pot with
                  None -> aux l
                 |Some com -> com
  in aux [computeQuinteFlush; computeCarre; computeFull;
                     computeCouleur; computeSuite; computeBrelan];;
