(*Quadtrees*)
(*I095 - I097*)

(*
Les celules du quadtree sont indexées sous la forme suivante :
 _ _ 
|0|1|
 - - 
|2|3|
 - - 
*)

#open "graphics";;

type vecteur = {x: float; y: float};;
type corps =
   {mass: float;
      mutable pos: vecteur;
      mutable vel: vecteur;
      mutable acc: vecteur};;
type arbre =
   | Vide
   | Feuille of corps
   | Noeud of cellule
   and cellule =
   {mutable cm_mass: float;
      mutable cm_pos: vecteur;
      filles: arbre vect};;

let rec pow i n = match i with
   | i when n = 0 -> 1
   | i when n = 1 -> i
   | i when n mod 2 = 0 -> pow (i * i) (n / 2)
   | i -> i * pow (i * i) ((n - 1) / 2);;

let random_vecteur sup =
   {x = 2. *. random__float sup -. sup; y = 2. *. random__float sup -. sup};;

let random_corps mass pos vel acc =
   {mass = random__float mass;
      pos = random_vecteur pos;
      vel = random_vecteur vel;
      acc = random_vecteur acc};;

let rec random_corpslist mass pos vel acc = function
   | i when i <= 0 -> []
   | i -> random_corps mass pos vel acc :: random_corpslist mass pos vel acc (i - 1);;

let add v1 v2 =
   {x = v1.x +. v2.x; y = v1.y +. v2.y};;

let sub v1 v2 =
	{x = v1.x -. v2.x; y = v1.y -. v2.y};;

let scal m u =
   {x = m *. u.x; y = m *. u.y};;

let rec profondeur = function
   | Vide -> 0
   | Feuille (c) -> 0
   | Noeud (c) ->
      let p0 = profondeur c.filles.(0)
      and p1 = profondeur c.filles.(1)
      and p2 = profondeur c.filles.(2)
      and p3 = profondeur c.filles.(3) in
         1 + max (max p0 p1) (max p2 p3);;

let rec max_masse = function
   | Vide -> 0.
   | Feuille (c) -> c.mass
   | Noeud (c) ->
      let m0 = max_masse c.filles.(0)
      and m1 = max_masse c.filles.(1)
      and m2 = max_masse c.filles.(2)
      and m3 = max_masse c.filles.(3) in
         max c.cm_mass (max (max m0 m1) (max m2 m3));;

let rec max_taille = function
 | Vide -> 0.
   | Feuille (c) -> max (abs_float c.pos.x) (abs_float c.pos.y)
   | Noeud (c) ->
      let m0 = max_taille c.filles.(0)
      and m1 = max_taille c.filles.(1)
      and m2 = max_taille c.filles.(2)
      and m3 = max_taille c.filles.(3)
      and m4 = max (abs_float c.cm_pos.x) (abs_float c.cm_pos.y) in
         max m4 (max (max m0 m1) (max m2 m3));;

let indice_fille p_c p =
   if p.x >= p_c.x
   then
      if p.y >= p_c.y
      then 1
      else 3
   else
   if p.y >= p_c.y
   then 0
   else 2;;

let position_fille p_c taille = function
   | 0 -> add p_c {x = -. taille /. 4.; y = taille /. 4.}
   | 1 -> add p_c {x = taille /. 4.; y = taille /. 4.}
   | 2 -> add p_c {x = -. taille /. 4.; y = -. taille /. 4.}
   | 3 -> add p_c {x = taille /. 4.; y = -. taille /. 4.}
   | _ -> failwith "erreur de numerotation de cellule";;










let cm_corpslist = function
   (*calcule le centre de masse d'une liste de corps*)
   | [] -> {x = 0.; y = 0.}
   | corpslist ->
      let rec boucle = fun
         | [] _ -> {x = 0.; y = 0.}, 0.
         | (t :: q) mass ->
            let pos, masstot = boucle q mass in
               add (scal t.mass t.pos) pos, masstot +. t.mass
      in
         let c_p, masstot = boucle corpslist 0. in
            if masstot = 0.
            then {x = 0.; y = 0.}
            else scal (1. /. masstot) c_p;;

let rec cm_arb = function
	(*renvoie le centre de masse de l'arbre*)
   | Vide -> 0., {x = 0.; y = 0.}
   | Feuille (c) -> c.mass, c.pos
   | Noeud (c) ->
      let cm_mass0, c_pos0 = cm_arb c.filles.(0)
      and cm_mass1, c_pos1 = cm_arb c.filles.(1)
      and cm_mass2, c_pos2 = cm_arb c.filles.(2)
      and cm_mass3, c_pos3 = cm_arb c.filles.(3) in
         let cm_mass = cm_mass0 +. cm_mass1 +. cm_mass2 +. cm_mass3 in
            if cm_mass = 0.
            then (c.cm_mass <- 0.; c.cm_pos <- {x = 0.; y = 0.})
            else (
                  let cm_pos = scal (1. /. cm_mass) (add (add (scal cm_mass0 c_pos0) (scal cm_mass1 c_pos1)) (add (scal cm_mass2 c_pos2) (scal cm_mass3 c_pos3))) in c.cm_mass <- cm_mass; c.cm_pos <- cm_pos
               )
            ; cm_mass, c.cm_pos;;

let rec insere_corps corps arb p_c taille = match arb with
   | Noeud (c) ->
      let i = indice_fille p_c corps.pos in
         c.filles.(i) <- insere_corps corps c.filles.(i) (position_fille p_c taille i) (taille /. 2.); Noeud (c)
   | Feuille (c) ->
      let avect = [|Vide; Vide; Vide; Vide|]
      and i = indice_fille p_c c.pos
      and j = indice_fille p_c corps.pos in
         avect.(i) <- Feuille (c);
         avect.(j) <- Feuille (corps);
         Noeud ({cm_mass = 0.; cm_pos = {x = 0.; y = 0.}; filles = avect})
   | Vide -> Feuille (corps);;

let construit_arbre taille corpslist =
   let rec construit p_c taille = function
      | [] -> Vide
      | t :: q -> insere_corps t (construit p_c taille q) p_c taille
   in
      let p_c = cm_corpslist corpslist in
         construit p_c taille corpslist;;

construit_arbre 1. (random_corpslist 1. 1. 1. 1. 5);;

let random_arb mass pos vel acc n =
   let rec taille_max = function
      | [] -> 0.
      | t :: q -> max (max (abs_float t.pos.x) (abs_float t.pos.y)) (taille_max q)
   in
      let corpslist = random_corpslist mass pos vel acc n in
         construit_arbre (2. *. taille_max corpslist) corpslist;;

let barycentres arb =
	(*idem cm_arb mais ne renvoie rien*)
   let rec boucle = function
      | Vide -> 0., {x = 0.; y = 0.}
      | Feuille (c) -> c.mass, c.pos
      | Noeud (c) ->
         let cm_mass0, c_pos0 = boucle c.filles.(0)
         and cm_mass1, c_pos1 = boucle c.filles.(1)
         and cm_mass2, c_pos2 = boucle c.filles.(2)
         and cm_mass3, c_pos3 = boucle c.filles.(3) in
            let cm_mass = cm_mass0 +. cm_mass1 +. cm_mass2 +. cm_mass3 in
               if cm_mass = 0.
               then (c.cm_mass <- 0.; c.cm_pos <- {x = 0.; y = 0.}; 0., {x = 0.; y = 0.})
               else (
                     let cm_pos = scal (1. /. cm_mass) (add (add (scal cm_mass0 c_pos0) (scal cm_mass1 c_pos1)) (add (scal cm_mass2 c_pos2) (scal cm_mass3 c_pos3))) in
                        c.cm_mass <- cm_mass; c.cm_pos <- cm_pos; cm_mass, cm_pos)
   in
      let cm_mass, cm_pos = boucle arb in ();;







let print_quadtree arb r =
   (*dessine un quadtree avec les corps en rouge et les centres de masses des cellules en bleu avec des cercles de rayon r*)
   let res = 600
   and line_width = 2
   and mass_u, posc_u = cm_arb arb
   and p_max = profondeur arb + 2
   and m_max = max_masse arb
   and t_max = 2. *. max_taille arb in
   let rec f prof p_c taille = function
      | Vide ->
         let t = taille /. (2. ** (float_of_int prof))
         and col = 255 * prof / p_max in
         set_color (rgb col col col);
         draw_circle (projectx p_c.x) (projecty p_c.y) (res / (pow 4 prof) + 1);
         moveto (projectx (p_c.x -. t)) (projecty (p_c.y -. t));
         lineto (projectx (p_c.x +. t)) (projecty (p_c.y +. t))
      | Feuille (c) ->
         set_color red;
         fill_circle (projectx c.pos.x) (projecty c.pos.y) (rayon c.mass)
      | Noeud (c) ->
         let t = taille /. 2.
         and col = 255 * prof / p_max in
         if prof = 1
         then ()
         else (
               set_color blue;
               fill_circle (projectx c.cm_pos.x) (projecty c.cm_pos.y) (rayon c.cm_mass)
            );
         set_color (rgb col col col);
         moveto (projectx p_c.x) (projecty (p_c.y +. t));
         lineto (projectx p_c.x) (projecty (p_c.y -. t));
         moveto (projectx (p_c.x -. t)) (projecty p_c.y);
         lineto (projectx (p_c.x +. t)) (projecty p_c.y);
         f (prof + 1) (position_fille p_c taille 0) (taille /. 2.) c.filles.(0);
         f (prof + 1) (position_fille p_c taille 1) (taille /. 2.) c.filles.(1);
         f (prof + 1) (position_fille p_c taille 2) (taille /. 2.) c.filles.(2);
         f (prof + 1) (position_fille p_c taille 3) (taille /. 2.) c.filles.(3)
   and projectx x = int_of_float (float_of_int res *. (0.5 +. (x -. posc_u.x) /. t_max))
   and projecty y = int_of_float (float_of_int res *. (0.5 +. (y -. posc_u.y) /. t_max))
   and rayon x = int_of_float (float_of_int r *. (x /. m_max)) + 1
   in
   open_graph (string_of_int res ^ "x" ^ string_of_int res);
   clear_graph ();
   set_line_width line_width;
   f 1 posc_u t_max arb;;

let mass = 1.
and pos = 1.
and vel = 1.
and acc = 1.
and n = 4 in
   random_arb mass pos vel acc n;;

let mass = 1.
and pos = 1.
and vel = 1.
and acc = 1.
and n = 10 in
   let a = random_arb mass pos vel acc n in
      barycentres a;
      a,profondeur a,print_quadtree a 40;;


close_graph();;



