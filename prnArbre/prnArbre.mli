(* +------------------------------------------------------------------------+
   |                                                                        |
   |                                                                        |
   |                 Boite � outils pour les manipulations d'arbres         |
   |                                                                        |
   |                                                                        |
   +------------------------------------------------------------------------+ *)
(* D'apr�s Michel Quercia, le 10/01/98 *)

                (* +-----------------------------------------+
                   |  Types des arbres g�n�raux et binaires  |
                   +-----------------------------------------+ *)

type 'a arbrebinaire =
  | Nil
  | Noeud of 'a arbrebinaire * 'a * 'a arbrebinaire;;

type 'a arbre = {etiquette : 'a; branches : 'a foret}
and 'a foret == 'a arbre list;;

                       (* +--------------------------+
                          |  Fonctions d'impression  |
                          +--------------------------+ *)

value print_string_arbre : string arbre -> unit;;
value print_string_arbrebinaire : string arbrebinaire -> unit;;
value print_int_arbre : int arbre -> unit;;
value print_int_arbrebinaire : int arbrebinaire -> unit;;
(* impression semi-graphique avec la biblioth�que [format]. *)
