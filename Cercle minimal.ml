(*Recherche de cercle minimal englobant une liste de points dans un plan*)

#open "graphics";;

type point = {x: float; y: float};;
type vecteur = {xn: float; yn: float};;


let deplacement a v =
   {x = a.x +. v.xn; y = a.y +. v.yn}
;;

let vecteur a b =
   {xn = b.x -. a.x; yn = b.y -. a.y}
;;

let norme v =
   sqrt (v.xn ** 2. +. v.yn ** 2.)
;;

let mult_scalaire a v =
   {xn = v.xn *. a; yn = v.yn *. a}
;;

let add_vect v1 v2 =
   {xn = v1.xn +. v2.xn; yn = v1.yn +. v2.yn}
;;

let rec distp_plist p1 = function
   | [] -> 0.
   | p2 :: q -> distp_plist p1 q +. norme (vecteur p1 p2);;

let rec distplist_plist plist = function
   | [] -> []
   | p :: q -> (p, distp_plist p plist) :: distplist_plist plist q;;

let rec distplist_max p1 = function
   | [] -> 0.
   | p2 :: q ->
      let d2 = distplist_max p1 q in
      let d1 = norme (vecteur p1 p2) in
         if d2 > d1
         then d2
         else d1;;

let bary0_plist = function
   (*calcule le barycentre d'une liste de points*)
   | [] -> {x = 0.; y = 0.}
   | plist ->
      let rec somme = function
         | [] -> {xn = 0.; yn = 0.}
         | p :: q -> add_vect (somme q) (vecteur {x = 0.; y = 0.} p)
      in
         let x = somme plist
         and coeftot = list_length plist in
            deplacement {x = 0.; y = 0.} (mult_scalaire (1. /. float_of_int coeftot) x);;

let bary1_plist = function
   (*calcule le barycentre d'une liste de points pondérée avec 1 coefficient*)
   | [] -> {x = 0.; y = 0.}
   | plist ->
      let rec somme = function
         | [] -> {xn = 0.; yn = 0.},0.
         | (p1, coef1) :: q ->
            let v2,coef2 = somme q in
               let v1 = vecteur {x = 0.; y = 0.} p1 in
                  add_vect (mult_scalaire coef1 v1) v2, coef1 +. coef2
      in
         let x, coeftot = somme plist in
            if coeftot = 0.
            then {x = 0.; y = 0.}
            else deplacement {x = 0.; y = 0.} (mult_scalaire (1. /. coeftot) x);;

let cerclemin0 plist =
   let bary = bary0_plist plist in
      let r = distplist_max bary plist in
         bary, r;;

let cerclemin1 plist =
   let bary = bary1_plist (distplist_plist plist plist) in
      let r = distplist_max bary plist in
         bary, r;;







let random_p sup =
   (*point dont les coordonnées sont comprises entre -sup et sup*)
   {x = 2. *. (random__float sup) -. sup; y = 2. *. (random__float sup) -. sup};;

let rec random_plist nbptot sup = match nbptot with
   (*liste de n points*)
   | i when i <= 0 -> []
   | i -> random_p sup :: (random_plist (nbptot - 1) sup)
and random_pvect nbptot sup =
   (*tableau de n points*)
   let pvect = make_vect nbptot {x = 0.; y = 0.} in
      for i = 0 to nbptot - 1 do
         pvect.(i) <- random_p sup
      done;
      pvect;;

let rec taille_max = function
   | [] -> 0.
   | p :: q -> max (max (abs_float p.x) (abs_float p.y)) (taille_max q);;

let print_cercles plist =
   let bary0, r0 = cerclemin0 plist
   and bary1, r1 = cerclemin1 plist
   and res = 600
   and line_width = 2
   and t_max = taille_max plist in
      let r_max = max r0 r1 in
         let rec affiche_points = function
            | [] -> ()
            | p :: q -> fill_circle (projectx p.x) (projecty p.y) 2; affiche_points q
         and projectx x = int_of_float (float_of_int res *. (0.5 +. (x -. bary0.x) /. (3. *. t_max)))
         and projecty y = int_of_float (float_of_int res *. (0.5 +. (y -. bary0.y) /. (3. *. t_max)))
         and rayon x = int_of_float (float_of_int res *. (x /. (3. *. t_max)))
         in
            open_graph (string_of_int res ^ "x" ^ string_of_int res);
            clear_graph ();
            set_line_width line_width;
            moveto 5 580;
            set_color blue;
            draw_string "cercle de barycentre simple";
            draw_circle (projectx bary0.x) (projecty bary0.y) (rayon r0);
            moveto 5 560;
            set_color red;
            draw_string "cercle de barycentre coefficienté";
            draw_circle (projectx bary1.x) (projecty bary1.y) (rayon r1);
            set_color black;
            affiche_points plist;;

let rec compte_true = function
   | [] -> 0
   | t :: q when t -> 1 + compte_true q
   | t :: q -> compte_true q;;

let blist = ref [] in
   for i = 1 to 50 do
      let plist = random_plist 3 10. in
         let p0, r0 = cerclemin0 plist
         and p1, r1 = cerclemin1 plist in
            blist := (r0 > r1) :: !blist
   done;
   compte_true !blist;;

let blist = ref [] in
   for i = 1 to 50 do
      let plist = random_plist 10 10. in
         let p0, r0 = cerclemin0 plist
         and p1, r1 = cerclemin1 plist in
            blist := (r0 > r1) :: !blist
   done;
   compte_true !blist;;

print_cercles (random_plist 50 10.);;

close_graph();;




