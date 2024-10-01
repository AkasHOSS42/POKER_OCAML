open Construction;;
open Probas;;

let input = open_in Sys.argv.(1);;

exception FichierInvalide;;

(* Bouge la tete de lecture. *)
let move gap = seek_in input ((pos_in input) + gap);;

(* Lit une carte dans le buffer input. *)
let getCard () =
  let rg =
    match input_char input with
      'A' -> As
     |'R' -> Roi
     |'D' -> Dame
     |'V' -> Valet
     |'2' -> Deux
     |'3' -> Trois
     |'4' -> Quatre
     |'5' -> Cinq
     |'6' -> Six
     |'7' -> Sept
     |'8' -> Huit
     |'9' -> Neuf
     |'1' -> (match input_char input with
                '0' -> Dix
               |_ -> raise FichierInvalide)
     |_ -> raise FichierInvalide
  in let col =
       match input_char input with
         'p' -> Pique
        |'t' -> Trefle
        |'c' -> (match input_char input with
                   'a' -> Carreau
                  |'o' -> Coeur
                  |_ -> raise FichierInvalide)
        |_ -> raise FichierInvalide
     in (col, rg);;

(* Lit une donne dans input. *)
let getDonne () =
  let c1 = getCard ()
  in move 1;
     let c2 = getCard ()
     in (move 1;
        (c1, c2));;

let donne1 = getDonne ();;

let donne2 =
  match input_char input with
    '?' -> move 1; None
   |_ -> move (-1); Some (getDonne ());;

(* Lit la table dans input. *)
let board =
  let c1 = getCard ()
  in move 1;
     let c2 = getCard ()
     in (move 1;
         let c3 = getCard ()
         in (try
               move 1;
               let c4 = getCard ()
               in (try
                     move 1;
                     let c5 = getCard ()
                     in (c1, c2, c3, Some (c4, Some c5)
                        )
                   with End_of_file -> (c1, c2, c3, Some (c4, None)
                                       )
                  )
             with End_of_file -> (c1, c2, c3, None)
            )
        );;

(* Calcul, puis affichage du resultat. *)
match donne2 with
  None -> let res = proba_simple donne1 board
          in if res=1.
             then print_string "Le joueur 1 est gagnant.\n"
             else (print_string "Joueur 1 : ";
             print_float res;
             print_newline ())
 |Some d -> match proba_double donne1 d board with
              (res1, res2) ->
              if res1 = 1.
              then print_string "Le joueur 1 est gagnant.\n"
              else if res2 = 1.
              then print_string "Le joueur 2 est gagnant.\n"
              else (print_string "Joueur 1 : ";
                    print_float res1;
                    print_newline ();
                    print_string "Joueur 2 : ";
                    print_float res2;
                    print_newline ());;

close_in input;;
