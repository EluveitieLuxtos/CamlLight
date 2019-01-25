(* +------------------------------------------------------------------------+
   |                                                                        |
   |                                                                        |
   |         Boite à outils pour impression semi-graphique d'arbres         |
   |                                                                        |
   |                                                                        |
   +------------------------------------------------------------------------+ *)

(*
type 'a arbrebinaire =
  | Nil
  | Noeud of 'a arbrebinaire * 'a * 'a arbrebinaire;;

type 'a arbre = {etiquette : 'a; branches : 'a foret}
and 'a foret == 'a arbre list;;
*)

(* D'après Michel Quercia, le 10/01/98 *)

type 'a g_arbre = G_noeud of 'a * ('a g_arbre list);;
type 'a b_arbre = B_vide | B_noeud of 'a * ('a b_arbre) * ('a b_arbre);;

                     (* +-------------------------------+
                        |  Fonctions de transformation  |
                        +-------------------------------+ *)

let rec g_map f = function
  G_noeud(x,fils) -> G_noeud(f(x), map (g_map f) fils);;

let rec b_map f = function
| B_vide -> B_vide
| B_noeud(x,g,d) -> B_noeud(f(x), b_map f g, b_map f d);;

    (* +------------------------------------------------------+
       |  Imprime un arbre général sous forme semi-graphique  |
       +------------------------------------------------------+ *)

#open "format";;

let centre chaine l =
  let lc = string_length(chaine) in
  if lc >= l then chaine
             else let g = (l-lc)/2 in let d = l-lc-g in
                  (make_string g `_`) ^ chaine ^(make_string d `_`)

;;

(* calcule les abscisses relatives d'impression  *)
(* des noeuds et l'encombrement total d'un arbre *)
let rec abscisses_g_arbre x0 = function G_noeud(etiq,fils) ->

  let (enc,xf0,xfn,foret) = abscisses_foret 0 fils in
  let xf = (xf0+xfn)/2 in
  let foret' = g_decale (-xf) foret in

  let etiq' = centre etiq (xfn-xf0-3) in
  let l = string_length(etiq') in
  (max enc l, G_noeud((x0+(max xf (l/2)),etiq'),foret'))

and abscisses_foret x0 = function
| []  -> (0,0,0,[])
| [a] -> let (enc,(G_noeud((x,_),_) as b)) = abscisses_g_arbre x0 a in (enc,x,x,[b])
| a::suite ->
    let (enc,(G_noeud((x,_),_) as b)) = abscisses_g_arbre x0 a in
    let (enc',_,y,f) = abscisses_foret (x0+enc+2) suite in (enc+enc'+2,x,y,b::f)


and g_decale dx = function
| [] -> []
| G_noeud((x,e),f)::suite -> G_noeud((x+dx,e),f) :: g_decale dx suite

;;


let rec print_g_noeuds x0 = function
| [] -> if x0 > 0 then print_cut()
| G_noeud((x,e),_)::suite ->
    let l = string_length(e) in
    for i=x0 to x-(l-1)/2-1 do print_char ` ` done;
    print_string(e);
    print_g_noeuds (x+l/2+1) suite
;;

let rec print_g_traits_fils x0 x = function
| [] -> x0
| G_noeud((y,_),_)::suite ->
    let z = (if y < 0 then x+y else if y = 0 then x+y-1 else x+y-2) in
    for i=x0 to z do print_char ` ` done;
    print_char (if y < 0 then `/` else if y = 0 then `|` else `\\`);
    print_g_traits_fils (z+2) x suite
;;

let rec print_g_traits x0 = function
| [] -> if x0 > 0 then print_cut()
| G_noeud((x,_),f)::suite ->
    let x1 = print_g_traits_fils x0 x f in
    print_g_traits x1 suite
;;

let rec liste_g_fils = function
| [] -> []
| G_noeud((x,e),f)::suite -> (g_decale x f) @ (liste_g_fils suite)
;;

let rec print_g_foret = function
| [] -> ()
| f  -> print_g_noeuds 0 f;
        print_g_traits 0 f;
        print_g_foret(liste_g_fils f)
;;

let print_string_g_arbre a =
  open_vbox(0);
  print_g_foret [snd(abscisses_g_arbre 0 a)];
  close_box()
;;

let print_int_g_arbre(a) = print_string_g_arbre(g_map string_of_int a);;

    (* +------------------------------------------------------+
       |  Imprime un arbre binaire sous forme semi-graphique  |
       +------------------------------------------------------+ *)

(* calcule les abscisses relatives d'impression  *)
(* des noeuds et l'encombrement total d'un arbre *)
let rec abscisses_b_arbre = function

| B_vide -> (0,B_vide)

| B_noeud(etiq,B_vide,B_vide) ->
   let l = string_length(etiq) in (l, B_noeud((l/2,etiq),B_vide,B_vide))

| B_noeud(etiq,g,B_vide) ->
   let l = string_length(etiq)
   and (eg,xg,g') = match abscisses_b_arbre g with
   | (eg,B_noeud((x,e),g,d)) -> (eg,x,B_noeud((-1,e),g,d))
   | _ -> failwith "cas impossible"
   in
   let x0 = min (-l/2) (-xg-1) and x1 = max (l-l/2) (eg-xg-1)
   in (x1-x0, B_noeud((-x0,etiq),g',B_vide))

| B_noeud(etiq,B_vide,d) ->
   let l = string_length(etiq)
   and (ed,xd,d') = match abscisses_b_arbre d with
   | (ed,B_noeud((x,e),g,d)) -> (ed,x,B_noeud((1,e),g,d))
   | _ -> failwith "cas impossible"
   in
   let x0 = min (-l/2) (-xd+1) and x1 = max (l-l/2) (ed-xd+1)
   in (x1-x0, B_noeud((-x0,etiq),B_vide,d'))

| B_noeud(etiq,g,d) ->

   let (eg,xg,etig,gg,gd) = match abscisses_b_arbre g with
   | (eg,B_noeud((x,etig),gg,gd)) -> (eg,x,etig,gg,gd)
   | _ -> failwith "cas impossible"
   in
  
   let (ed,xd,etid,dg,dd) = match abscisses_b_arbre d with
   | (ed,B_noeud((x,etid),dg,dd)) -> (ed,eg+x+2,etid,dg,dd)
   | _ -> failwith "cas impossible"
   in

   let xm = (xd+xg)/2 in
   let etiq' = centre etiq (xd-xg-3) in
   let l = string_length(etiq')
   in (max (eg+ed+2) l, B_noeud((max xm (l/2),etiq'),
                          B_noeud((xg-xm,etig),gg,gd),
                          B_noeud((xd-xm,etid),dg,dd)))

;;


(* impression *)
let rec print_b_noeuds x0 = function
| [] -> if x0 > 0 then print_cut()
| B_vide::suite -> print_b_noeuds x0 suite
| B_noeud((x,e),_,_)::suite ->
    let l = string_length(e) in
    for i=x0 to x-(l-1)/2-1 do print_char ` ` done;
    print_string(e);
    print_b_noeuds (x+l/2+1) suite
;;

let print_b_traits_fils x0 x = function
| B_vide -> x0
| B_noeud((y,_),_,_) ->
    let z = (if y < 0 then x+y else if y = 0 then x+y-1 else x+y-2) in
    for i=x0 to z do print_char ` ` done;
    print_char (if y < 0 then `/` else if y = 0 then `|` else `\\`);
    z+2
;;

let rec print_b_traits x0 = function
| [] -> if x0 > 0 then print_cut()
| B_vide::suite -> print_b_traits x0 suite
| B_noeud((x,_),g,d)::suite ->
    let x1 = print_b_traits_fils x0 x g in
    let x2 = print_b_traits_fils x1 x d in
    print_b_traits x2 suite
;;

let rec liste_b_fils = function
| [] -> []
| B_vide::suite -> liste_b_fils suite
| B_noeud((x,e),g,d)::suite ->
   (match g with B_vide -> [] | B_noeud((y,e),g,d) -> [B_noeud((x+y,e),g,d)])
 @ (match d with B_vide -> [] | B_noeud((y,e),g,d) -> [B_noeud((x+y,e),g,d)])
 @ (liste_b_fils suite)
;;

let rec print_b_foret = function
| [] -> ()
| f  -> print_b_noeuds 0 f;
        print_b_traits 0 f;
        print_b_foret(liste_b_fils f)
;;

let print_string_b_arbre a =
  open_vbox(0);
  print_b_foret [snd(abscisses_b_arbre a)];
  close_box()
;;

let print_int_b_arbre(a) = print_string_b_arbre(b_map string_of_int a);;

                     (* +---------------------------------------+
                        |  Transformations dans les types perso |
                        +---------------------------------------+ *)

let rec g_arbre_of_arbre a =
  G_noeud (a.etiquette, map g_arbre_of_arbre a.branches);;
  
let rec b_arbre_of_arbrebinaire = function
  | Nil -> B_vide
  | Noeud (u,x,v) -> B_noeud (x,b_arbre_of_arbrebinaire u,b_arbre_of_arbrebinaire v);;
  
let print_string_arbre a =
  print_string_g_arbre (g_arbre_of_arbre a)
and print_int_arbre a =
  print_int_g_arbre (g_arbre_of_arbre a)
and  print_string_arbrebinaire a =
  print_string_b_arbre (b_arbre_of_arbrebinaire a)
and  print_int_arbrebinaire a =
  print_int_b_arbre (b_arbre_of_arbrebinaire a);;
