(*Arbres binaires de recherche*)
(*I089 - I091*)

type arbrebin = VideB | N of arbrebin * int * arbrebin;;



#directory "prnArbre";;
include "prnArbre.ini";;

let print_arbrebin a=
  let rec f a=
  match a with 
    |VideB -> Nil
   |N(ag,e,ad)-> Noeud ((f ag),(string_of_int e),(f ad))
  in print_string_arbrebinaire (f a);;
install_printer "print_arbrebin";;

let print_tas (t,n)=
  let rec f = function
    |i when i>=n -> Nil
   |i -> Noeud ((f (2*i+1),(string_of_int t.(i)),(f (2*i+2))))
  in print_string_arbrebinaire (f 0);;
install_printer "print_tas";;

let mem_vect a vect =
   let l = vect_length vect
   and bool = ref false in
      for i = 0 to l - 1 do
         if a = vect.(i) then bool := true else ()
      done;
      !bool;;

let random_intvect nbp nbptot =
   (*tableau de nbp entiers*)
   if nbptot = 0
   then [||]
   else
      let ivect = make_vect (abs nbp) 0 in
         for i = 0 to (abs nbp) - 1 do
            ivect.(i) <- random__int nbptot + 1
         done;
         ivect;;

let random_intdisvect nbp nbptot =
   (*tableau de nbp entiers distincts*)
   let range = min (abs nbp) (abs nbptot) in
      let ivect = make_vect range 0 in
         for i = 0 to range - 1 do
            let rd = ref (random__int nbptot + 1) in
               while mem_vect !rd ivect do
                  rd := random__int nbptot + 1
               done;
               ivect.(i) <- !rd
         done;
         ivect;;

let random_intlist nbp nbptot =
   (*liste de nbp entiers*)
   let range = min (abs nbp) (abs nbptot) in
      let rec boucle = function
         | 0 -> []
         | i -> random__int nbptot + 1 :: boucle (i - 1)
      in boucle range;;

let random_intdislist nbp nbptot =
   (*liste de nbp entiers distincts*)
   let range = min (abs nbp) (abs nbptot) in
      let rec boucle ilist = function
         | 0 -> []
         | i ->
            let rd = ref (random__int nbptot + 1) in
               while mem !rd ilist do
                  rd := random__int nbptot + 1
               done;
               !rd :: (boucle (!rd :: ilist) (i - 1))
      in boucle [] range;;

let rec bin_min_max = function
   | VideB -> failwith "arbre vide, pas de max"
   | N (VideB, x, VideB) -> true, x, x
   | N (g, x, VideB) ->
      let b, m, M = bin_min_max g in
         (b & x >= M, min x m, max x M)
   | N (VideB, x, d) ->
      let b, m, M = bin_min_max d in
         (b & x <= m, min x m, max x M)
   | N (g, x, d) ->
      let bg, mg, Mg = bin_min_max g
      and bd, md, Md = bin_min_max d in
         (bg & bd & x >= Mg & x <= md, min x (min mg md), max x (max Mg Md));;

let est_de_rech a =
   (*teste si un arbre binaire est un arbre binaire de recherche*)
   let res, m, M = bin_min_max a in res;;

let est_de_rech = function
   (*teste si un arbre binaire est un arbre binaire de recherche*)
   | VideB -> false
   | a -> let res, m, M = bin_min_max a in res;;

let rec inser y = function
   | VideB -> N (VideB, y, VideB)
   | N (g, x, d) ->
      if x < y
      then N (g, x, inser y d)
      else N (inser y g, x, d);;

let rec rech y = function
   | VideB -> false
   | N (g, x, d) -> if x = y then true else (rech y g || rech y d);;

let rec fusion1 = function
   | VideB, d -> d
   | N (gg, x, gd), d -> N (gg, x, fusion1 (gd, d));;

let rec sup y = function
   | VideB -> VideB
   | N (g, x, d) ->
      if x < y then N (g, x, sup y d)
      else if x > y then N (sup y g, x, d)
      else fusion1 (g, d);;

let rec enlever_max = function
   | VideB -> failwith "arbre vide"
   | N (g, y, VideB) -> (g, y)
   | N (g, y, d) ->
      let arb, a = enlever_max d in
         N (g, y, arb), a;;

let fusion2 = function
	|VideB,d -> d
	|g,VideB -> g
	|g,d ->
	let gg,y = enlever_max g in
	N(gg,y,d);;

let rec sup_un y = function
   | VideB -> VideB
   | N (g, x, d) ->
      if x < y then N (g, x, sup_un y d)
      else if x > y then N (sup_un y g, x, d)
      else fusion2 (g, d);;

let rec supprime_un x = function
   | VideB -> VideB
   | N (g, y, d) when x = y -> fusion2 (g, d)
   | N (g, y, d) when x < y -> N (supprime_un x g, y, d)
   | N (g, y, d) -> N (g, y, supprime_un x d);;

let rec supprime_les x = function
   | VideB -> VideB
   | N (g, y, d) when x = y -> fusion2 (supprime_les x g, supprime_les x d)
   | N (g, y, d) when x < y -> N (supprime_les x g, y, d)
   | N (g, y, d) -> N (g, y, supprime_les x d);;

(*
Bilan de Complexité

Les fonctions recherche, ins, sup dans un ABR sont en O(hauteur)
Donc si l'arbre est équilibré, en O(log n), n étant le nombre de noeuds
*)

(*Tas*)
(*Tas représentés par un tableau trop grand et l'indice du dernier élément utilisé*)

let rec remonte (t, n) = function
   (*remonte l'élément d'indice i dans un tas représenté par un couple T,n*)
   | 0 -> ()
   | i when t.((i - 1) / 2) >= t.(i) -> ()
   | i ->
      let a = t.((i - 1) / 2) in
         t.((i - 1) / 2) <- t.(i);
         t.(i) <- a;
         remonte (t, n) ((i - 1) / 2);;

let echange t i j =
   let a = t.(i) in
      t.(i) <- t.(j);
      t.(j) <- a;;

let rec descendre (t, n) = function
   | i when 2 * i + 1 > n - 1 -> () (*tout en bas*)
   | i when t.(i) < t.(2 * i + 1) ->
      if t.(2 * i + 1) > t.(2 * i + 2)
      then (echange t i (2 * i + 1); descendre (t, n) (2 * i + 1))
      else (echange t i (2 * i + 2); descendre (t, n) (2 * i + 2))
   | i when t.(i) < t.(2 * i + 2) -> echange t i (2 * i + 2); descendre (t, n) (2 * i + 2)
   | _ -> ();;

let supprime_racine (t, n) =
	(*supprime la racine*)
   t.(0) <- t.(n - 1);
   descendre (t, n - 1) 0;
   t, n - 1;;

let inser_tas a (t, n) =
   t.(n) <- a;
   remonte (t, n) n;;

let t = [|0; 0; 0; 0; 0; 0; 0; 0; 0|] in
   inser_tas 3 (t, 0);
   t;;

(*Tri par tas*)

let rec liste_of_tas = function
   | (_, 0) -> []
   | (t, n) ->
      let a = t.(0) in
         a :: liste_of_tas (supprime_racine (t, n));;

let rec ajoute_liste (t, n) = function
   | [] -> t, n
   | a :: l -> (inser_tas a (t, n); ajoute_liste (t, n + 1) l);;

let t = [|0; 0; 0; 0; 0; 0; 0; 0; 0|] in
   ajoute_liste (t, 0) [1; 4; 2; 8; 4];;
let t = make_vect 20 0 in
   ajoute_liste (t, 0) (random_intlist 10 100);;


let tas_of_liste l =
   let n = list_length l in
      ajoute_liste (make_vect n 0, 0) l;;

let tri_par_tas l =
   liste_of_tas (tas_of_liste l);;








