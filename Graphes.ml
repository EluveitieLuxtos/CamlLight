#open "graphics";;
#open "random";;
#open "printf";;

let width=512;;
let height=512;;

open_graph (" "^(string_of_int width)^"x"^(string_of_int height)) ;;
clear_graph();;

type graphe_matrice == int vect vect ;;
type graphe_liste == int list vect ;;


(* ---------------Manupulations élémentaires-----------------------------------*)
(*-----------------------------------------------------------------------------*)


let liste_of_matrice m=
  let n =vect_length m in
  let t=make_vect n [] in
  let rec list_of_ligne l i =function
    |(-1)->l
    |k -> 
      if m.(i).(k)=1 then list_of_ligne (k::l) i (k-1) 
      else list_of_ligne l i (k-1) in
  for i=0 to n-1 do
    t.(i)<-list_of_ligne [] i (n-1)
  done;
  t;;


let matrice_of_liste t=
  let  n=vect_length t in
  let m=make_matrix n n 0 in
  let rec ligne_of_list i =function
    |[]->()
    |j::q -> m.(i).(j)<-1;ligne_of_list i q in
  for i = 0 to n-1 do
    ligne_of_list i t.(i)
  done;
  m;;



let trace_un_sommet label x y c=
  let s=string_of_int label in
  let a,b=text_size s 
  and r,_=text_size "99" in
  set_color c;
  draw_circle x y r;
  moveto (x-a/2) (y-b/2);
  draw_string (string_of_int label);;



let position n nbs =
  let cx=width/2
  and cy=height/2 
  and r= float_of_int (9*(min width height)/20) 
  and pifois2 =asin 1.0 *. 4.0 in
  let x=cx + int_of_float (r *. cos ((float_of_int n) *. pifois2 /. (float_of_int nbs)))
  and y=cy + int_of_float (r *. sin ((float_of_int n) *. pifois2 /. (float_of_int nbs))) in
  x,y;;

let trace_les_sommets nbs c=
  for i =0 to nbs-1 do
    let x,y=position i nbs in
    trace_un_sommet i x y c
  done;;

let trace_arete nbs i j c=
  let xi,yi=position i nbs
  and xj,yj=position j nbs
  and r=float_of_int (fst (text_size "99")) in
  let d= sqrt (float_of_int ((xj-xi)*(xj-xi) +  (yj-yi)*(yj-yi))) in
  let dx=int_of_float (float_of_int (xj-xi) /. d *. r)
  and dy=int_of_float (float_of_int (yj-yi) /. d *. r) in
  set_color c;
  moveto (xi+dx) (yi+dy);
  lineto (xj-dx) (yj-dy);;

let trace_graphe gl cs ca=
  let nbs=vect_length gl in
  trace_les_sommets nbs cs;
  let rec trace_list i =function
    |j::q -> trace_arete nbs i j ca;trace_list i q 
    |[] -> () in
  for i =0 to nbs-1 do
    trace_list i gl.(i)
  done;;

let graphe_matrice_aleatoire nbs p =
  let m=make_matrix nbs nbs 0 in
  for i = 0 to nbs-1 do
    for j = i+1 to nbs-1 do
      if random__float 1.0 <p then begin
        m.(i).(j) <- 1;
        m.(j).(i) <- 1
      end
    done
  done;
  m;;
    
(*--------------------essais manipulation élémentaires--------------------------*)
(*------------------------------------------------------------------------------*)

clear_graph();
trace_les_sommets 30 black;;
trace_arete 30 24 21 (rgb 100 150 120);;
trace_arete 30 1 9 black;;
trace_arete 30 0 3 green;;
clear_graph();;
let m = liste_of_matrice (graphe_matrice_aleatoire 10 0.1);;
trace_graphe m red blue;;
clear_graph ();;



let visite_intempestive graphe indice =
   let n = vect_length graphe in
      let t = make_vect n (- 1) in
         let rec propage_mark = function
            | i when graphe.(i) = [] -> t.(i) <- indice
            | i when t.(i) <> -1 or i >= n -> ()
            | i -> t.(i) <- indice; do_list propage_mark graphe.(i)
         in propage_mark indice;
            t;;
visite_intempestive m 3;;

let tableau_composante_connexe graphe =
   let n = vect_length graphe in
      let t = make_vect n (- 1) in
         let rec propage_mark indice = function
            | i when graphe.(i) = [] -> t.(i) <- indice
            | i when t.(i) <> - 1 or i >= n -> ()
            | i -> t.(i) <- indice; do_list (propage_mark indice) graphe.(i)
         in
            for i = 0 to n - 1 do
               if t.(i) = - 1
               then propage_mark i i
               else ()
            done;
            t;;

let liste_composantes_of_vect t =
   let n = vect_length t in
      let rec compo = function
         | - 1 -> []
         | i ->
            let rec sommets = function
               | - 1 -> []
               | j -> if t.(j) = i then j :: (sommets (j - 1)) else (sommets (j - 1))
            in
               let s = sommets (n - 1) in
                  if s = []
                  then compo (i - 1)
                  else s :: compo (i - 1)
      in compo (n - 1);;

let composante_connexe graphe =
	liste_composantes_of_vect (tableau_composante_connexe graphe);;
composante_connexe m;;

let trace_composante_connexe graphe =
   let t = composante_connexe graphe in
      let points = vect_length graphe
      and j = ref 1 in
         let rec affiche j = function
            | [] -> incr (j)
            | [a] -> incr (j)
            | a :: b :: q -> let c = (!j * 255 / points) in trace_arete points a b (rgb (255 - c) 0 c); affiche j (b :: q)
         in do_list (affiche j) t;;

let est_acyclique nul graphe =
   (*prend en argument l'élément nul et la matrice d'adjacence d'un graphe orienté et vérifie s'il est acyclique*)
   let p = vect_length graphe
   and q = vect_length graphe.(0)
   and bool = ref true in
      let marques = make_matrix p q 0 in
         let rec recherche i =
            if not !bool
            then []
            else
               let l1 = ref [] in
                  for j = 0 to q - 1 do
                     if graphe.(i).(j) <> nul
                     then
                        if marques.(i).(j) = 0
                        then (
                              marques.(i).(j) <- 1;
                              let l2 = recherche j in
                                 marques.(i).(j) <- 0;
                                 if exists (mem i) l2
                                 then bool := false;
                                 l1 := flat_map (fun l -> [i :: l]) l2 @ (!l1)
                           )
                        else bool := false
                  done;
                  !l1
         in
            let l1 = ref [] in
               for i = 0 to p - 1 do
                  l1 := recherche i
               done;
               !bool
;;

clear_graph();
let n = 20 in
let p = (1./.(float_of_int n)) in
let m = liste_of_matrice (graphe_matrice_aleatoire n p) in
trace_les_sommets n black;
trace_composante_connexe m;;
clear_graph ();;






