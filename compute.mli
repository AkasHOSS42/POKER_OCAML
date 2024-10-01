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

(* Retourne la meilleure combinaison de cinq cartes
 * possible dans un potentiel. *)
val computeComb : potentiel -> comb;;
