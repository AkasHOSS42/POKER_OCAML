#use "extension.ml";;

(* On teste les situations des fichiers test1 a test6. *)

proba_multiple 2 [((Carreau, As), (Pique, Dix))]
  ((Coeur, Roi), (Trefle, Dix), (Carreau, Dix), None);;

proba_multiple 2 [((Trefle, Roi), (Carreau, Huit)); ((Carreau, Quatre), (Trefle, Huit))]
  ((Carreau, Neuf), (Trefle, Sept), (Pique, Dix), Some ((Pique, Huit), None));;

proba_multiple 2 [((Trefle, Roi), (Trefle, As))]
  ((Trefle, Dix), (Trefle, Valet), (Trefle, Dame), None);;

proba_multiple 2 [((Trefle, As), (Trefle, Sept)); ((Trefle, Roi), (Carreau, Deux))]
  ((Trefle, Valet), (Coeur, Quatre), (Pique, Dix), None);;

proba_multiple 2 [((Carreau, Deux), (Carreau, Valet))]
  ((Pique, Cinq), (Coeur, Dame), (Trefle, Valet), Some ((Pique, Six), Some (Pique, Neuf)));;

proba_multiple 2 [((Trefle, Trois), (Carreau, Huit)); ((Carreau, Trois), (Trefle, Huit))]
  ((Pique, Neuf), (Coeur, Trois), (Pique, Sept), None);;

(* Un exemple de "meilleur cas".
 * Le temps d'execution est rapide,
 * et pourtant on a 18 cartes inconnues. *)

proba_multiple 10 [((Trefle, Trois), (Carreau, Huit)); ((Carreau, Trois), (Trefle, Huit))]
  ((Pique, Neuf), (Coeur, Trois), (Pique, Sept), None);;

proba_multiple 10 [((Carreau, Roi), (Coeur, Quatre));
                    ((Trefle, Six), (Pique, Dix));
                    ((Carreau, Quatre), (Trefle, Dame));
                    ((Coeur, Roi), (Pique, Cinq));
                    ((Trefle, Cinq), (Trefle, Sept));
                    ((Coeur, Deux), (Trefle, Trois));
                    ((Coeur, Six), (Carreau, Dix));
                    ((Coeur, Huit), (Carreau, Huit))]
  ((Pique, Valet), (Trefle, Neuf), (Pique, Six), Some ((Carreau, Sept), Some (Pique, Trois)));;

proba_multiple 3 [((Trefle, Trois), (Trefle, Quatre));
                    ((Carreau, Quatre), (Trefle, Sept))]
  ((Pique, Valet), (Trefle, Neuf), (Pique, Six), Some ((Carreau, Sept), None));;
