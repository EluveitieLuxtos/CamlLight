(*Snake*)
(*Cosinus plus rapide que la racine carrée pour calculer un côté d'un triangle
Concaténation de tableaux plus rapide que la concaténation de listes
Améliorable en faisant de vrais lacs : B-splines*)

#open "graphics";;

let res = 600		(*taille de la fenêtre*)
and anneaux = 6	(*taille des anneaux du serpent*)
and dt = 200.		(*durée totale de jeu*)
and dteph = 5.		(*durée d'actualisation des objets éphémères*)
and fond = rgb 0 170 0;;
let marge = res / 20;;

let delai () =
   let ti = sys__time () in
   while sys__time () < ti +. 0.1 do
      ()
   done;;

let pi = 4.*.atan 1.;;
type point = {x: int; y: int};;
type vecteur = {xn: int; yn: int};;
type segment =
	|Z of point * int
	|Q of point * int
	|S of point * int
	|D of point * int;;

type figure =
   | rect of point * int * int
   | cercle of point * int
   | poly of point vect;;

type objet =
	(*le premier float est la durée de vie du bonus*)
   | none
   | obstacle
   | bomb
   | shield of float
   | life of float * int
   | warp of float * point
   | slow of float * float
   | speed of float * float
   | score of float * int;;

type niveau =
   {mutable tk: float;	(*durée du niveau et durée maximale des éphémères*)
      teph: float;		(*durée d'actualisation des objets éphémères*)
      crossx: bool;
      crossy: bool;
      nbobj: int;
      objets: objet vect vect;
      decor: color vect vect};;

let deplacement a v =
   {x = a.x + v.xn; y = a.y + v.yn};;

let vecteur a b =
   {xn = b.x - a.x; yn = b.y - a.y};;

let add_vect v1 v2 =
   {xn = v1.xn + v2.xn; yn = v1.yn + v2.yn};;

let mult_scalaire a v =
   {xn = int_of_float (float_of_int v.xn *. a); yn = int_of_float (float_of_int v.yn *. a)};;

let prod_scalaire v1 v2 =
   v1.xn * v2.xn + v1.yn * v2.yn;;

let bary0_plist = function
   (*calcule le barycentre d'une liste de points*)
   | [] -> {x = 0; y = 0}
   | plist ->
      let rec somme = function
         | [] -> {xn = 0; yn = 0}
         | p :: q -> add_vect (somme q) (vecteur {x = 0; y = 0} p)
      in
         let x = somme plist
         and coeftot = list_length plist in
            deplacement {x = 0; y = 0} (mult_scalaire (1. /. float_of_int coeftot) x);;

let memp_rect pi p1 p2 =
   ((p1.x <= pi.x & pi.x <= p2.x) or (p2.x <= pi.x & pi.x <= p1.x))
   & ((p1.y <= pi.y & pi.y <= p2.y) or (p2.y <= pi.y & pi.y <= p1.y));;

let memp_cerc pi p r =
   let dist = sqrt ((float_of_int (pi.x - p.x)) ** 2. +. (float_of_int (pi.y - p.y)) ** 2.) in
      dist <= float_of_int r;;

let memp_poly p1 pvect =
   (*vérifie si un point appartient à un polygone par l'algorithme du crossing number*)
   let l = vect_length pvect in
      let j = ref (l - 1)
      and c = ref false in
         for i = 0 to l - 1 do
            if (pvect.(i).y > p1.y) <> (pvect.(!j).y > p1.y) & (float_of_int p1.x < float_of_int ((pvect.(!j).x - pvect.(i).x) * (p1.y - pvect.(i).y)) /. float_of_int (pvect.(!j).y - pvect.(i).y) +. float_of_int pvect.(i).x)
            then c := not !c
            else ();
            j := i
         done;
         !c;;

let rec memp_figlist p2 = function
   (*vérifie si un point appartient à une liste de lacs et de pierres, on ignore les maisons*)
   | [] -> false
   | rect (_, _, _) :: q -> memp_figlist p2 q
   | cercle (p1, r) :: q ->
      if memp_cerc p2 p1 r
      then true
      else memp_figlist p2 q
   | poly (pvect) :: q ->
      if memp_poly p2 pvect
      then true
      else memp_figlist p2 q;;

let inter_segm_segm p1 p2 p3 p4 =
   if p1.x = p2.x & p3.x = p4.x
   then p1.x = p3.x & (memp_rect p1 p3 p4 or memp_rect p2 p3 p4)
   else
   if p1.x = p2.x
   then
      let a = float_of_int (p4.y - p3.y) /. float_of_int (p4.x - p3.x) in
         let b = float_of_int p3.y -. float_of_int p3.x *. a
         and ix = p1.x in
            let iy = int_of_float (a *. float_of_int ix +. b) in
               memp_rect {x = ix; y = iy} p1 p2 & memp_rect {x = ix; y = iy} p3 p4
   else
   if p3.x = p4.x
   then
      let a = float_of_int (p2.y - p1.y) /. float_of_int (p2.x - p1.x) in
         let b = float_of_int p1.y -. float_of_int p1.x *. a
         and ix = p3.x in
            let iy = int_of_float (a *. float_of_int ix +. b) in
               memp_rect {x = ix; y = iy} p1 p2 & memp_rect {x = ix; y = iy} p3 p4
   else
      let a1 = float_of_int (p2.y - p1.y) /. float_of_int (p2.x - p1.x)
      and a2 = float_of_int (p4.y - p3.y) /. float_of_int (p4.x - p3.x) in
         let b1 = float_of_int p1.y -. float_of_int p1.x *. a1
         and b2 = float_of_int p3.y -. float_of_int p3.x *. a2 in
            if a1 = a2
            then
               if b1 = b2
               then true
               else false
            else
               let ix = int_of_float ((b2 -. b1) /. (a1 -. a2))
               and iy = int_of_float ((a1 *. b2 -. a2 *. b1) /. (a1 -. a2)) in
                  memp_rect {x = ix; y = iy} p1 p2 & memp_rect {x = ix; y = iy} p3 p4;;

let inter_segm_cerc p1 p2 p3 r3 =
   let x1 = float_of_int p1.x
   and y1 = float_of_int p1.y
   and x2 = float_of_int p2.x
   and y2 = float_of_int p2.y
   and x3 = float_of_int p3.x
   and y3 = float_of_int p3.y
   and r = float_of_int r3 in
      let b1 = sqrt ((x1 -. x3) ** 2. +. (y1 -. y3) ** 2.) < r
      and b2 = sqrt ((x2 -. x3) ** 2. +. (y2 -. y3) ** 2.) < r in
         if (b1 & (not b2)) or (b2 & (not b1))
         then true
         else
         if b1 & b2
         then false
         else
         if p1 = p2
         then false
         else
            let dx12 = x2 -. x1
            and dy12 = y2 -. y1
            and dx13 = x3 -. x1
            and dy13 = y3 -. y1 in
               let t = (dx12 *. dx13 +. dy12 *. dy13) /. (dx12 ** 2. +. dy12 ** 2.) in
                  if 0. <= t & t <= 1.
                  then
                     let xi = x1 +. t *. dx12
                     and yi = y1 +. t *. dy12 in
                        if sqrt ((x3 -. xi) ** 2. +. (y3 -. yi) ** 2.) <= r
                        then true
                        else false
                  else false;;

let inter_rec_poly p1 a1 b1 pvect =
   if pvect = [||]
   then false
   else
      let v = vect_length pvect
      and b = ref false
      and p11 = {x = p1.x + a1; y = p1.y}
      and p12 = {x = p1.x; y = p1.y + b1}
      and p13 = {x = p1.x + a1; y = p1.y + b1} in
         for i = 0 to v - 2 do
            b := !b
            or inter_segm_segm p1 p11 pvect.(i) pvect.(i + 1)
            or inter_segm_segm p11 p13 pvect.(i) pvect.(i + 1)
            or inter_segm_segm p13 p12 pvect.(i) pvect.(i + 1)
            or inter_segm_segm p12 p1 pvect.(i) pvect.(i + 1)
         done;
         !b
         or inter_segm_segm p1 p11 pvect.(0) pvect.(v - 1)
         or inter_segm_segm p11 p13 pvect.(0) pvect.(v - 1)
         or inter_segm_segm p13 p12 pvect.(0) pvect.(v - 1)
         or inter_segm_segm p12 p1 pvect.(0) pvect.(v - 1);;

let inter_rec_fig p1 a1 b1 = function
   (*vérifie si un rectangle intersecte ou appartient à une figure*)
   | rect (p2, a2, b2) ->
      let p11 = {x = p1.x + a1; y = p1.y}
      and p12 = {x = p1.x; y = p1.y + b1}
      and p21 = {x = p2.x + a2; y = p2.y}
      and p22 = {x = p2.x; y = p2.y + b2}
      and n = ref 0 in
         if p1.x <= p2.x & p2.x <= p11.x then incr n;
         if p1.y <= p2.y & p2.y <= p12.y then incr n;
         if p2.x <= p1.x & p1.x <= p21.x then incr n;
         if p2.y <= p1.y & p1.y <= p22.y then incr n;
         if p1.x <= p21.x & p21.x <= p11.x then incr n;
         if p1.y <= p22.y & p22.y <= p12.y then incr n;
         if p2.x <= p11.x & p11.x <= p21.x then incr n;
         if p2.y <= p12.y & p12.y <= p22.y then incr n;
         !n >= 4
   | cercle (p2, r2) ->
      let p11 = {x = p1.x + a1; y = p1.y}
      and p12 = {x = p1.x; y = p1.y + b1}
      and p13 = {x = p1.x + a1; y = p1.y + b1} in
         inter_segm_cerc p1 p11 p2 r2
         or inter_segm_cerc p11 p13 p2 r2
         or inter_segm_cerc p13 p12 p2 r2
         or inter_segm_cerc p12 p1 p2 r2
         or (p1.x <= p2.x & p2.x <= p11.x & p1.y <= p2.y & p2.y <= p12.y)
   | poly (pvect) ->
      if pvect = [||]
      then false
      else
         let p11 = {x = p1.x + a1; y = p1.y}
         and p12 = {x = p1.x; y = p1.y + b1}
         and p2 = pvect.(0) in
            inter_rec_poly p1 a1 b1 pvect
            or memp_poly p1 pvect
            or (p1.x <= p2.x & p2.x <= p11.x & p1.y <= p2.y & p2.y <= p12.y);;

let inter_cerc_fig p1 r1 = function
   (*vérifie si un cercle intersecte ou appartient à une figure*)
   | rect (p2, a2, b2) ->
      let p21 = {x = p2.x + a2; y = p2.y}
      and p22 = {x = p2.x; y = p2.y + b2}
      and p23 = {x = p2.x + a2; y = p2.y + b2} in
         inter_segm_cerc p2 p21 p1 r1
         or inter_segm_cerc p21 p23 p1 r1
         or inter_segm_cerc p23 p22 p1 r1
         or inter_segm_cerc p22 p2 p1 r1
         or (p2.x <= p1.x & p1.x <= p21.x & p2.y <= p1.y & p1.y <= p22.y)
   | cercle (p2, r2) ->
      let dist = sqrt (float_of_int (p2.x - p1.x) ** 2. +. float_of_int (p2.y - p1.y) ** 2.) in
         dist <= float_of_int (r1 + r2)
   | poly (pvect2) ->
      if pvect2 = [||]
      then false
      else
         let b = ref false
         and l = vect_length pvect2 in
            for i = 0 to l - 2 do
               b := !b or inter_segm_cerc pvect2.(i) pvect2.(i + 1) p1 r1
            done;
            !b
            or inter_segm_cerc pvect2.(0) pvect2.(l - 1) p1 r1
            or memp_poly p1 pvect2;;

let inter_poly_fig pvect1 = function
   (*vérifie si un polygone intersecte ou appartient à une figure*)
   | rect (p2, a2, b2) ->
      if pvect1 = [||]
      then false
      else
         let p21 = {x = p2.x + a2; y = p2.y}
         and p22 = {x = p2.x; y = p2.y + b2}
         and p1 = pvect1.(0) in
            inter_rec_poly p2 a2 b2 pvect1
            or memp_poly p2 pvect1
            or (p2.x <= p1.x & p1.x <= p21.x & p2.y <= p1.y & p1.y <= p22.y)
   | cercle (p2, r2) ->
      let b = ref false
      and l = vect_length pvect1 in
         for i = 0 to l - 2 do
            b := !b or inter_segm_cerc pvect1.(i) pvect1.(i + 1) p2 r2
         done;
         !b
         or inter_segm_cerc pvect1.(0) pvect1.(l - 1) p2 r2
         or memp_poly p2 pvect1
   | poly (pvect2) ->
      if pvect2 = [||]
      then false
      else
         let b = ref false
         and p2 = pvect2.(0) in
            for i = 0 to vect_length pvect1 - 1 do
               b := !b or memp_poly pvect1.(i) pvect2
            done;
            !b or memp_poly p2 pvect1;;

let rec inter_rec_figures p a b = function
   | [] -> false
   | t :: q ->
      if inter_rec_fig p a b t
      then true
      else inter_rec_figures p a b q
and inter_cerc_figures p r = function
   | [] -> false
   | t :: q ->
      if inter_cerc_fig p r t
      then true
      else inter_cerc_figures p r q
and inter_poly_figures pvect = function
   | [] -> false
   | t :: q ->
      if inter_poly_fig pvect t
      then true
      else inter_poly_figures pvect q;;









let random_bool() =
	let i = random__int 2 in
	if i = 0 then false else true;;

let random_p taille =
   (*un pixel*)
   {x = random__int taille; y = random__int taille};;

let random_pos p0 taille =
   (*position atteignable par le serpent*)
   {x = p0.x + random__int (taille / anneaux) * anneaux; y = p0.y + random__int (taille / anneaux) * anneaux};;

let random_c () =
	(*couleur aléatoire pas trop flashie*)
   rgb (random__int 225) (random__int 225) (random__int 225);;

let random_cvect l =
   let cvect = make_vect l transp
   and tamo = random__int (3 * anneaux) + anneaux in
      for i = 0 to l - 1 do
         cvect.(i) <- random_c ()
      done;
      tamo, cvect;;

let vect_of_list l =
   if l = []
   then [||]
   else
      let v = make_vect (list_length l) (hd l) in
         let rec boucle l i = match l, i with
            | [], _ -> v
            | t :: q, i -> v.(i) <- t; boucle q (i + 1)
         in boucle l 0;;

let vect_of_pvect pvect =
   let v = vect_length pvect in
      let res = make_vect v (0, 0) in
         for i = 0 to v - 1 do
            res.(i) <- pvect.(i).x, pvect.(i).y
         done;
         res;;

let random_pvect p0 taille nbptot =
   (*crée un tableau de nbptot points, étoilés autour de leur isobarycentre et situés dans un carré de côté taille et de coin inférieur gauche p0, en O(n*ln(n))*)
   let rec random_plist = function
      | i when i <= 0 -> []
      | i -> {x = p0.x + random__int taille; y = p0.y + random__int taille} :: random_plist (i - 1)
   and coord_polaires p1 = function
      (*calcule les coordonnées polaires des points avec pour centre p1*)
      | [] -> []
      | p2 :: q ->
         let dx = float_of_int (p2.x - p1.x)
         and dy = float_of_int (p2.y - p1.y) in
            let r = sqrt (dx ** 2. +. dy ** 2.) in
               if dy > 0.
               then let teta = acos (dx /. r) in (teta, p2) :: coord_polaires p1 q
               else let teta = 2. *. pi -. acos (dx /. r) in (teta, p2) :: coord_polaires p1 q
   and tri_lexico l =
      let ordre a b = min a b = a in
         sort__sort ordre l
   and extrait_liste = function
      | [] -> []
      | (teta, p) :: q -> p :: extrait_liste q
   in
      let plist = random_plist nbptot in
         let bary = bary0_plist plist in
            vect_of_list (extrait_liste (tri_lexico (coord_polaires bary plist)));;

let random_b tk dt =
   (*renvoie un bonus aléatoire, dont la date de péremption est comprise entre tk et tk+dt pour les éphémères*)
   let probas_vect = [|0.4; 0.05; 0.05; 0.1; 0.05; 0.05; 0.3|] in
   let proba_b = random__float 1.
   and proba_fix = ref 0.
   and i = ref 0 in
   while (!i < 7) & (!proba_fix < proba_b) do
      proba_fix := !proba_fix +. probas_vect.(!i);
      incr (i)
   done;
   match !i with
   | 1 -> bomb
   | 2 -> shield (random__float dt +. tk)
   | 3 -> life (random__float dt +. tk, random__int 2)
   | 4 -> warp (random__float dt +. tk, random_pos {x = 1; y = 1} (res - 2 * anneaux))
   | 5 -> slow (random__float dt +. tk, random__float 0.5 +. 0.5)
   | 6 -> speed (random__float dt +. tk, random__float 0.5 +. 1.)
   | 7 -> score (random__float dt +. tk, random__int 10 + 1)
   | _ -> none;;

let rec random_blist tk dt = function
   | i when i <= 0 -> []
   | i -> random_b tk dt :: random_blist tk dt (i - 1);;

let init_objets () =
   let n = res / anneaux in
      make_matrix n n none;;

let init_decor () =
	make_matrix res res transp;;

let rec init_niveaux = function
   | 0 -> []
   | i -> {tk = 0.; teph = dteph; crossx = false; crossy = false; nbobj = 5; objets = init_objets (); decor = init_decor ()} :: init_niveaux (i - 1);;

let rec random_tklist t1 =
   let t2 = random__float 60. +. 30. in
      if t1 +. t2 < dt
      then (t1 +. t2) :: random_tklist (t1 +. t2)
      else [t1 +. t2];;

let random_lac tala = random_pvect (random_p (res - tala)) tala (random__int 3 + 8);;

let affiche_motif p0 dx dy =
   let j = random__int 3 + 1
   and k = ref 0
   and l = ref 0
   and x = ref p0.x
   and y = ref p0.y in
      let tamo, cvect = random_cvect j in
         while !y < p0.y + dy do
            x := p0.x;
            while !x < p0.x + dx do
               set_color cvect.(!k);
               if !x + tamo > p0.x + dx
               then
                  if !y + tamo > p0.y + dy
                  then fill_rect !x !y (p0.x + dx - !x) (p0.y + dy - !y)
                  else fill_rect !x !y (p0.x + dx - !x) tamo
               else
               if !y + tamo > p0.y + dy
               then fill_rect !x !y tamo (p0.y + dy - !y)
               else fill_rect !x !y tamo tamo;
               x := !x + tamo;
               incr k;
               if !k >= j then k := 0
            done;
            y := !y + tamo;
            incr l;
            k:= !l mod j
         done;;

let affiche_maison p0 dx dy porte1 porte2 =
   affiche_motif p0 dx dy;
   set_color black;
   let x0 = p0.x
   and y0 = p0.y
   and k = ref 0
   and tamu = anneaux / 2 in
      for i = x0 to x0 + dx - 1 do
         incr k;
         if (porte1 < !k & !k < porte1 + anneaux * 3) or (porte2 < !k & !k < porte2 + anneaux * 3)
         then ()
         else fill_rect (i) (y0 - tamu) 1 (2 * tamu)
      done;
      for i = y0 to y0 + dy - 1 do
         incr k;
         if (porte1 < !k & !k < porte1 + anneaux * 3) or (porte2 < !k & !k < porte2 + anneaux * 3)
         then ()
         else fill_rect (x0 + dx - tamu) (i) (2 * tamu) 1
      done;
      for i = x0 + dx downto x0 + 1 do
         incr k;
         if (porte1 < !k & !k < porte1 + anneaux * 3) or (porte2 < !k & !k < porte2 + anneaux * 3)
         then ()
         else fill_rect (i) (y0 + dy - tamu) 1 (2 * tamu)
      done;
      for i = y0 + dy downto y0 + 1 do
         incr k;
         if (porte1 < !k & !k < porte1 + anneaux * 3) or (porte2 < !k & !k < porte2 + anneaux * 3)
         then ()
         else fill_rect (x0 - tamu) (i) (2 * tamu) 1
      done;;

let reset_niveaux () =
   (*reset les niveaux à chaque fois qu'on recommence à jouer*)
   let n = res / anneaux (*n représente le nombre de cases de large*)
   and tkvect = vect_of_list (random_tklist 0.) in
      let nbniv = vect_length tkvect
      and nbpi = random__int (n / 10 + 1) + (n / 10)
      and nbla = random__int (n / 30 + 1) + (n / 30)
      and nbma = random__int (n / 50 + 1) + (n / 50)
      and tapi = res / 50
      and tala = res / 8
      and tama = res / 3 in
         let niveaux = vect_of_list (init_niveaux nbniv) in
            for i = 0 to nbniv - 1 do
               niveaux.(i).tk <- tkvect.(i)
            done;
            let random_decor lk =
               let rec maisons declist = function
                  | 0 ->
                     set_color blue;
                     lacs declist nbla
                  | i ->
                     let p0 = ref (random_pos {x = 0; y = 0} (res - tama - 10 * anneaux))
                     and dx = random__int (tama / anneaux - 10) + 10
                     and dy = random__int (tama / anneaux - 10) + 10 in
                        while inter_rec_figures !p0 (dx * anneaux) (dy * anneaux) declist do
                           p0 := random_pos {x = 0; y = 0} (res - tama - 10 * anneaux)
                        done;
                        let porte1 = random__int (2 * (dx + dy))
                        and porte2 = random__int (2 * (dx + dy))
                        and x0 = !p0.x / anneaux
                        and y0 = !p0.y / anneaux
                        and k = ref (- 1) in
                           affiche_maison !p0 (anneaux * dx) (anneaux * dy) (anneaux * porte1) (anneaux * porte2);
                           for i = x0 to x0 + dx - 1 do
                              incr k;
                              if (porte1 < !k & !k < porte1 + 3) or (porte2 < !k & !k < porte2 + 3)
                              then ()
                              else niveaux.(lk).objets.(y0).(i) <- obstacle
                           done;
                           for i = y0 to y0 + dy - 1 do
                              incr k;
                              if (porte1 < !k & !k < porte1 + 3) or (porte2 < !k & !k < porte2 + 3)
                              then ()
                              else niveaux.(lk).objets.(i).(x0 + dx) <- obstacle
                           done;
                           for i = x0 + dx downto x0 + 1 do
                              incr k;
                              if (porte1 < !k & !k < porte1 + 3) or (porte2 < !k & !k < porte2 + 3)
                              then ()
                              else niveaux.(lk).objets.(y0 + dy).(i) <- obstacle
                           done;
                           for i = y0 + dy downto y0 + 1 do
                              incr k;
                              if (porte1 < !k & !k < porte1 + 3) or (porte2 < !k & !k < porte2 + 3)
                              then ()
                              else niveaux.(lk).objets.(i).(x0) <- obstacle
                           done;
                           maisons (rect (!p0, anneaux * dx, anneaux * dy) :: declist) (i - 1)
               and lacs declist = function
                  | 0 ->
                     set_color (rgb 150 150 150);
                     pierres declist nbpi
                  | i ->
                     let lac = ref (random_lac tala) in
                        while inter_poly_figures !lac declist do
                           lac := random_lac tala
                        done;
                        fill_poly (vect_of_pvect !lac);
                        lacs ((poly !lac) :: declist) (i - 1)
               and pierres declist = function
                  | 0 -> declist
                  | i ->
                     let p0 = ref (random_p (res - tapi))
                     and r = random__int tapi + anneaux in
                        while inter_cerc_figures !p0 r declist do
                           p0 := random_p (res - tapi)
                        done;
                        fill_circle !p0.x !p0.y r;
                        pierres (cercle (!p0, r) :: declist) (i - 1)
               in maisons [] nbma
            in

               close_graph ();
               open_graph (string_of_int res ^ "x" ^ string_of_int res);
					delai ();
               for lk = 0 to nbniv - 1 do
                  clear_graph ();
                  set_color fond;
                  fill_rect 0 0 res res;
                  let figlist = random_decor lk in
                     for i = 0 to n - 1 do
                        for j = 0 to n - 1 do
                           if memp_figlist {x = anneaux * i; y = anneaux * j} figlist
                           then niveaux.(lk).objets.(j).(i) <- obstacle
                              (*met les obstacles sur les lacs et les pierres*)
                        done;
                     done;
                     let decor = dump_image (get_image 0 0 res res) in
                        for i = 0 to res - 1 do
                           for j = 0 to res - 1 do
                              niveaux.(lk).decor.(i).(j) <- decor.(i).(j)
                           done;
                        done;
               done;
               close_graph ();
               niveaux;;










let getkey tk vel dir =
	(*renvoie la direction choisie par l'utilisateur (précédente configuration sinon)*)
   let p = ref dir in
      while sys__time () < tk +. vel do
      	delai ();
         if key_pressed ()
         then let chtemp = (wait_next_event [Key_pressed]).key in
               if chtemp = `z`
               then p := {x = 0; y = 1}
               else
               if chtemp = `q`
               then p := {x = - 1; y = 0}
               else
               if chtemp = `s`
               then p := {x = 0; y = - 1}
               else
               if chtemp = `d`
               then p := {x = 1; y = 0}
               else ()
         else ()
      done;
      !p;;

let debuter_partie () =
	while not key_pressed () do
		delai ()
	done;;

let rec jeu () =
   let niveaux = reset_niveaux ()

   in
   let ini_snake lk =
      let p0 = ref (random_pos {x = 5 * anneaux; y = 0} (res - 10 * anneaux)) in
      while niveaux.(lk).objets.(!p0.y / anneaux).(!p0.x / anneaux) = obstacle
         or niveaux.(lk).objets.(!p0.y / anneaux).(!p0.x / anneaux + 1) = obstacle
         or niveaux.(lk).objets.(!p0.y / anneaux).(!p0.x / anneaux + 2) = obstacle
         or niveaux.(lk).objets.(!p0.y / anneaux).(!p0.x / anneaux + 3) = obstacle do
         p0 := random_pos {x = 5 * anneaux; y = 0} (res - 10 * anneaux)
      done;
      [D (!p0, 5)]

   and affiche_points_of_rect lk p a b =
      (*affiche les points du décor correspondants au rectangle*)
      for i = p.x to p.x + a do
         for j = p.y to p.y + b do
            set_color niveaux.(lk).decor.(res - j).(i);
            plot (i + marge) (j + marge)
         done;
      done

   and movep p1 p2 coef2 =
      (*deplace le point p1 de coef2 fois le point p2*)
      {x = p1.x + coef2 * p2.x; y = p1.y + coef2 * p2.y}

   and rot dir i =
      (*redirige un vecteur de direction dir dans le sens donné par i*)
      if dir.x = - 1
      then {x = 0; y = i}
      else
      if dir.x = 1
      then {x = 0; y = i}
      else
      if dir.y = - 1
      then {x = i; y = 0}
      else
      if dir.y = 1
      then {x = i; y = 0}
      else dir

   in
   let efface_tete lk t dir =
      (*efface la tête tracée et la remplace par le corps*)
      let p1 = movep t dir (anneaux + 2)
      and p2 = movep t (rot dir 1) anneaux
      and p3 = movep t (rot dir (- 1)) anneaux in
      let xmin = min p1.x (min p2.x p3.x)
      and xmax = max p1.x (max p2.x p3.x)
      and ymin = min p1.y (min p2.y p3.y)
      and ymax = max p1.y (max p2.y p3.y) in
      affiche_points_of_rect lk {x = xmin; y = ymin} (xmax - xmin) (ymax - ymin);
      set_color black;
      fill_circle (t.x + marge) (t.y + marge) (anneaux / 2 + 1)

   and affiche_tete t dir =
      (*affiche la tête dans la bonne direction*)
      let p1 = movep t dir (anneaux + 2)
      and p2 = movep t (rot dir 1) anneaux
      and p3 = movep t (rot dir (- 1)) anneaux in
      set_color black;
      fill_poly [|p1.x + marge, p1.y + marge; p2.x + marge, p2.y + marge; p3.x + marge, p3.y + marge|]

   and extrait_tete = function
      (*renvoie la tête du serpent*)
      | [] -> {x = 0; y = 0}
      | Z (p, l) :: q -> p
      | Q (p, l) :: q -> p
      | S (p, l) :: q -> p
      | D (p, l) :: q -> p

   and except_tete = function
      (*renvoie le corps du serpent*)
      | [] -> []
      | Z (p, l) :: q ->
         if l = 1
         then q
         else Z (movep p {x = 0; y = - 1} anneaux, l - 1) :: q
      | Q (p, l) :: q ->
         if l = 1
         then q
         else Q (movep p {x = 1; y = 0} anneaux, l - 1) :: q
      | S (p, l) :: q ->
         if l = 1
         then q
         else S (movep p {x = 0; y = 1} anneaux, l - 1) :: q
      | D (p, l) :: q ->
         if l = 1
         then q
         else D (movep p {x = - 1; y = 0} anneaux, l - 1) :: q

   and concat_tete t = function
      (*rajoute une tête déjà calculée au corps du serpent*)
      | [] -> []
      | Z (p, l) :: q -> Z (t, l + 1) :: q
      | Q (p, l) :: q -> Q (t, l + 1) :: q
      | S (p, l) :: q -> S (t, l + 1) :: q
      | D (p, l) :: q -> D (t, l + 1) :: q

   in
   let rec except_queue lk = function
      (*enlève la queue du serpent*)
      | [] -> []
      | [Z (p, l)] ->
         affiche_points_of_rect lk {x = p.x - (anneaux / 2 + 1); y = p.y - anneaux * (l - 1) - (anneaux / 2)} (anneaux + 1) (anneaux + 1);
         if l = 1
         then []
         else [Z (p, l - 1)]
      | [Q (p, l)] ->
         affiche_points_of_rect lk {x = p.x + anneaux * (l - 1) - (anneaux / 2 + 1); y = p.y - (anneaux / 2)} (anneaux + 1) (anneaux + 1);
         if l = 1
         then []
         else [Q (p, l - 1)]
      | [S (p, l)] ->
         affiche_points_of_rect lk {x = p.x - (anneaux / 2 + 1); y = p.y + anneaux * (l - 1) - (anneaux / 2)} (anneaux + 1) (anneaux + 1);
         if l = 1
         then []
         else [S (p, l - 1)]
      | [D (p, l)] ->
         affiche_points_of_rect lk {x = p.x - anneaux * (l - 1) - (anneaux / 2 + 1); y = p.y - (anneaux / 2)} (anneaux + 1) (anneaux + 1);
         if l = 1
         then []
         else [D (p, l - 1)]
      | t :: q -> t :: except_queue lk q

   and efface_serpent lk dir snake =
      let t = extrait_tete snake in
      efface_tete lk t dir;
      let rec boucle = function
         | [] -> ()
         | Z (p, l) :: q ->
            for i = 0 to l - 1 do
               affiche_points_of_rect lk {x = p.x - (anneaux / 2 + 1); y = p.y - anneaux * i - (anneaux / 2)} (anneaux + 1) (anneaux + 1);
            done;
            boucle q
         | Q (p, l) :: q ->
            for i = 0 to l - 1 do
               affiche_points_of_rect lk {x = p.x + anneaux * i - (anneaux / 2 + 1); y = p.y - (anneaux / 2)} (anneaux + 1) (anneaux + 1);
            done;
            boucle q
         | S (p, l) :: q ->
            for i = 0 to l - 1 do
               affiche_points_of_rect lk {x = p.x - (anneaux / 2 + 1); y = p.y + anneaux * i - (anneaux / 2)} (anneaux + 1) (anneaux + 1);
            done;
            boucle q
         | D (p, l) :: q ->
            for i = 0 to l - 1 do
               affiche_points_of_rect lk {x = p.x - anneaux * i - (anneaux / 2 + 1); y = p.y - (anneaux / 2)} (anneaux + 1) (anneaux + 1);
            done;
            boucle q
      in boucle snake

   and cut_snake t = function
      (*teste si la nouvelle tête coupe un des segments du serpent, puis renvoie le serpent*)
      | [] -> false
      | Z (p, l) :: q ->
         if t.x = p.x & t.y <= p.y & t.y >= p.y - anneaux * (l - 1)
         then true
         else cut_snake t q
      | Q (p, l) :: q ->
         if t.y = p.y & t.x >= p.x & t.x <= p.x + anneaux * (l - 1)
         then true
         else cut_snake t q
      | S (p, l) :: q ->
         if t.x = p.x & t.y >= p.y & t.y <= p.y + anneaux * (l - 1)
         then true
         else cut_snake t q
      | D (p, l) :: q ->
         if t.y = p.y & t.x <= p.x & t.x >= p.x - anneaux * (l - 1)
         then true
         else cut_snake t q

   in
   let move_snake lk dir = function
      (*déplace la tete du serpent en retirant la queue puis affiche le serpent résultant*)
      | [] -> []
      | Q (p, l) :: q ->
         let t = movep p dir anneaux
         and q2 = except_queue lk (Q (p, l) :: q) in
         affiche_tete t dir;
         if dir.y = 0
         then concat_tete t q2
         else
         if dir.y = 1
         then Z (t, 1) :: q2
         else S (t, 1) :: q2
      | D (p, l) :: q ->
         let t = movep p dir anneaux
         and q2 = except_queue lk (D (p, l) :: q) in
         affiche_tete t dir;
         if dir.y = 0
         then concat_tete t q2
         else
         if dir.y = 1
         then Z (t, 1) :: q2
         else S (t, 1) :: q2
      | Z (p, l) :: q ->
         let t = movep p dir anneaux
         and q2 = except_queue lk (Z (p, l) :: q) in
         affiche_tete t dir;
         if dir.x = 0
         then concat_tete t q2
         else
         if dir.x = 1
         then D (t, 1) :: q2
         else Q (t, 1) :: q2
      | S (p, l) :: q ->
         let t = movep p dir anneaux
         and q2 = except_queue lk (S (p, l) :: q) in
         affiche_tete t dir;
         if dir.x = 0
         then concat_tete t q2
         else
         if dir.x = 1
         then D (t, 1) :: q2
         else Q (t, 1) :: q2

   and warp_snake lk dir t = function
      (*téléporte la tete du serpent en retirant la queue puis affiche le serpent résultant*)
      | [] -> []
      | Q (p, l) :: q ->
         let q2 = except_queue lk (Q (p, l) :: q) in
         affiche_tete t dir;
         if dir.y = 0
         then Q (t, 1) :: q2
         else
         if dir.y = 1
         then Z (t, 1) :: q2
         else S (t, 1) :: q2
      | D (p, l) :: q ->
         let q2 = except_queue lk (D (p, l) :: q) in
         affiche_tete t dir;
         if dir.y = 0
         then D (t, 1) :: q2
         else
         if dir.y = 1
         then Z (t, 1) :: q2
         else S (t, 1) :: q2
      | Z (p, l) :: q ->
         let q2 = except_queue lk (Z (p, l) :: q) in
         affiche_tete t dir;
         if dir.x = 0
         then Z (t, 1) :: q2
         else
         if dir.x = 1
         then D (t, 1) :: q2
         else Q (t, 1) :: q2
      | S (p, l) :: q ->
         let q2 = except_queue lk (S (p, l) :: q) in
         affiche_tete t dir;
         if dir.x = 0
         then S (t, 1) :: q2
         else
         if dir.x = 1
         then D (t, 1) :: q2
         else Q (t, 1) :: q2

   and expand_snake dir = function
      (*déplace la tete du serpent sans retirer la queue puis affiche le serpent résultant*)
      | [] -> []
      | Q (p, l) :: q ->
         let t = movep p dir anneaux in
         affiche_tete t dir;
         if dir.y = 0
         then Q (t, l + 1) :: q
         else
         if dir.y = 1
         then Z (t, 1) :: Q (p, l) :: q
         else S (t, 1) :: Q (p, l) :: q
      | D (p, l) :: q ->
         let t = movep p dir anneaux in
         affiche_tete t dir;
         if dir.y = 0
         then D (t, l + 1) :: q
         else
         if dir.y = 1
         then Z (t, 1) :: D (p, l) :: q
         else S (t, 1) :: D (p, l) :: q
      | Z (p, l) :: q ->
         let t = movep p dir anneaux in
         affiche_tete t dir;
         if dir.x = 0
         then Z (t, l + 1) :: q
         else
         if dir.x = 1
         then D (t, 1) :: Z (p, l) :: q
         else Q (t, 1) :: Z (p, l) :: q
      | S (p, l) :: q ->
         let t = movep p dir anneaux in
         affiche_tete t dir;
         if dir.x = 0
         then S (t, l + 1) :: q
         else
         if dir.x = 1
         then D (t, 1) :: S (p, l) :: q
         else Q (t, 1) :: S (p, l) :: q

   in
   let affiche_bonus lk xb yb = function
      | bomb -> set_color black; fill_circle xb yb (anneaux / 2 + 1)
      | shield (teph) -> set_color blue; fill_circle xb yb (anneaux / 2 + 1)
      | life (teph, i) -> set_color red; fill_circle xb yb (anneaux / 2 + 1)
      | warp (teph, p) -> set_color cyan; fill_circle xb yb (anneaux / 2 + 1)
      | slow (teph, f) -> set_color magenta; fill_circle xb yb (anneaux / 2 + 1)
      | speed (teph, f) -> set_color green; fill_circle xb yb (anneaux / 2 + 1)
      | score (teph, i) -> set_color yellow; fill_circle xb yb (anneaux / 2 + 1)
      | none -> affiche_points_of_rect lk {x = xb - (anneaux / 2 + 1); y = yb - (anneaux / 2 + 1)} anneaux anneaux
      | b -> ()

   and affiche_niveau lk = draw_image (make_image niveaux.(lk).decor) marge marge

   and affiche_vies lives =
      set_color red;
      fill_poly
      [|3 * marge / 2, res + 3 * marge;
         7 * marge / 4, res + 13 * marge / 4;
         2 * marge, res + 3 * marge;
         9 * marge / 4, res + 13 * marge / 4;
         5 * marge / 2, res + 3 * marge;
         2 * marge, res + 2 * marge|];
      let str = string_of_int lives in
      let l = string_length str in
      set_color cyan;
      set_text_size (marge / l);
      let i, _ = text_size "0" in
      moveto (2 * marge - l * i / 2) (res + 9 * marge / 4 + i * (l - 1));
      draw_string str

   and affiche_bouclier () =
      set_color blue;
      fill_poly
      [|7 * marge / 2, res + 3 * marge;
         15 * marge / 4, res + 13 * marge / 4;
         4 * marge, res + 3 * marge;
         17 * marge / 4, res + 13 * marge / 4;
         9 * marge / 2, res + 3 * marge;
         4 * marge, res + 2 * marge|]

   and efface_bouclier () =
      set_color 9868950;
      fill_poly
      [|7 * marge / 2, res + 3 * marge;
         15 * marge / 4, res + 13 * marge / 4;
         4 * marge, res + 3 * marge;
         17 * marge / 4, res + 13 * marge / 4;
         9 * marge / 2, res + 3 * marge;
         4 * marge, res + 2 * marge|]

   and affiche_score points =
      let str = string_of_int points in
      let l = string_length str in
      set_color yellow;
      fill_circle (6 * marge) (res + 21 * marge / 8) (2 * marge / 4 + l * 2);
      set_color black;
      set_text_size 13;
      moveto (6 * marge - 4 * l) (res + 19 * marge / 8);
      draw_string str

   and affiche_vitesse vel =
      set_color magenta;
      fill_poly
      [|63 * marge / 8, res + 13 * marge / 4;
         33 * marge / 4, res + 13 * marge / 4;
         32 * marge / 4, res + 13 * marge / 5;
         33 * marge / 4, res + 13 * marge / 5;
         31 * marge / 4, res + 2 * marge;
         63 * marge / 8, res + 12 * marge / 5;
         30 * marge / 4, res + 12 * marge / 5|];
      let uni = int_of_float vel in
      let diz = int_of_float (10. *. vel) - 10 * uni in
      set_color white;
      set_text_size 13;
      moveto (61 * marge / 8) (res + 12 * marge / 5);
      draw_string (string_of_int uni);
      moveto (124 * marge / 16) (res + 27 * marge / 10);
      draw_string ".";
      moveto (63 * marge / 8) (res + 14 * marge / 5);
      draw_string (string_of_int diz)

   and mort () =
      set_color black;
      fill_rect 0 0 (res + 2 * marge) (res + 2 * marge);
      set_color red;
      let i = string_length "Game Over"
      and j = string_length "Tu veux recommencer ?" in
      moveto (res / 2 - i * 4) (res / 2);
      draw_string "Game Over";
      moveto (res / 2 - j * 4) (res / 2 - 13);
      draw_string "Tu veux recommencer ?";
      debuter_partie ()

   and victoire () =
      set_color black;
      fill_rect 0 0 (res + 2 * marge) (res + 2 * marge);
      set_color blue;
      let i = string_length "Victory !"
      and j = string_length "Tu veux recommencer ?" in
      moveto (res / 2 - i * 4) (res / 2);
      draw_string "Victory !";
      moveto (res / 2 - j * 4) (res / 2 - 13);
      draw_string "Tu veux recommencer ?";
      debuter_partie ()

   and actu_eph tk = function
      | shield (teph) when tk > teph -> none
      | life (teph, i) when tk > teph -> none
      | warp (teph, p) when tk > teph -> none
      | slow (teph, f) when tk > teph -> none
      | speed (teph, f) when tk > teph -> none
      | score (teph, i) when tk > teph -> none
      | b -> b

   in
   let rec assign_bonus lk = function
      | [] -> []
      | b :: q ->
         let xb = ref 0
         and yb = ref 0
         and p = ref (random_pos {x = 1; y = 1} (res - 2 * anneaux)) in
         while niveaux.(lk).objets.(!yb / anneaux).(!xb / anneaux) <> none do
            p := random_pos {x = anneaux; y = anneaux} (res - 2 * anneaux);
            xb := !p.x;
            yb := !p.y
         done;
         affiche_bonus lk (!xb + marge) (!yb + marge) b;
         niveaux.(lk).objets.(!yb / anneaux).(!xb / anneaux) <- b;
         (!xb, !yb) :: assign_bonus lk q

   in
   let rec actu_ephlist tk lk = function
      (*actualise tous les éphémères et en rajoute de nouveaux*)
      | [] -> let blist = random_blist tk niveaux.(lk).tk niveaux.(lk).nbobj in assign_bonus lk blist
      | (xb, yb) :: q ->
         let o = actu_eph tk niveaux.(lk).objets.(yb / anneaux).(xb / anneaux) in
         if o = none
         then (
               niveaux.(lk).objets.(yb / anneaux).(xb / anneaux) <- none;
               affiche_bonus lk (xb + marge) (yb + marge) none;
               actu_ephlist tk lk q
            )
         else (xb, yb) :: actu_ephlist tk lk q

   in
   open_graph (string_of_int (res + 2 * marge) ^ "x" ^ string_of_int (res + 4 * marge));
	delai ();
   clear_graph ();
   set_color black;
   fill_rect 0 0 (res + 2 * marge) (res + 4 * marge);
   affiche_vies 3;
   efface_bouclier ();
   affiche_score 0;
   affiche_vitesse 1.0;
   affiche_niveau 0;
   debuter_partie ();
   let ti = sys__time () in
   let rec test_shield tk teph vel lk points lives shbool eph dir snake =
      if shbool
      then
         (efface_bouclier ();
            boucle tk teph vel lk points lives false eph dir snake)
      else (efface_serpent lk dir snake;
            affiche_vies (lives - 1);
            let vel2 = 0.1 /. float_of_int (lk + 1) in
            affiche_vitesse (0.1 /. vel2);
            boucle tk teph vel2 lk points (lives - 1) false eph {x = 1; y = 0} (ini_snake lk))
   and boucle tk teph vel lk points lives shbool eph dir = function
      (*bouge le serpent jusqu'à ce que le joueur perde ou que le temps soit écoulé*)
      | _ when lives < 0 -> mort (); jeu ()
         (*défaite*)
      | _ when tk > ti +. dt -> victoire (); jeu ()
         (*victoire*)
      | snake when tk > ti +. niveaux.(lk).tk ->
         affiche_niveau (lk + 1);
         affiche_vitesse (1. /. (8. *. vel));
         affiche_score (points + (lk + 1) * 100);
         debuter_partie ();
         boucle tk teph (0.8 *. vel) (lk + 1) (points + (lk + 1) * 100) (lives + 1) shbool [] dir (ini_snake (lk + 1))
         (*passage au niveau suivant*)
      | snake when teph > niveaux.(lk).teph ->
         let eph2 = actu_ephlist tk lk eph in
         boucle tk 0. vel lk (points + 1) lives shbool eph2 dir snake
         (*actualise les objets éphémères*)
      | snake ->
         let dir2 = getkey tk vel dir in
         efface_tete lk (extrait_tete snake) dir;
         if (dir.y = 0 & dir2.x = - dir.x) or (dir.x = 0 & dir2.y = - dir.y)
         then
            (efface_serpent lk dir snake;
               affiche_vies (lives - 1);
               let vel2 = 0.1 /. float_of_int (lk + 1) in
               affiche_vitesse (0.1 /. vel2);
               if shbool then efface_bouclier ();
               boucle tk teph vel2 lk points (lives - 1) false eph {x = 1; y = 0} (ini_snake lk))
         else
            let sn2 = move_snake lk dir2 snake in
            let t = extrait_tete sn2 in
            if t.x <= anneaux or t.x + anneaux >= res or t.y <= anneaux or t.y + anneaux >= res or cut_snake t (except_tete sn2)
            then
               (efface_serpent lk dir snake;
                  affiche_vies (lives - 1);
                  let vel2 = 0.1 /. float_of_int (lk + 1) in
                  affiche_vitesse (0.1 /. vel2);
                  if shbool then efface_bouclier ();
                  boucle tk teph vel2 lk points (lives - 1) false eph {x = 1; y = 0} (ini_snake lk))
            else
               let xb, yb = t.x / anneaux, t.y / anneaux
               and tk2 = sys__time () in
               let obj = niveaux.(lk).objets.(yb).(xb)
               and teph2 = teph +. tk2 -. tk in
               match obj with
               | none ->
                  boucle tk2 teph2 vel lk points lives shbool eph dir2 sn2
               | obstacle ->
                  test_shield tk2 teph2 vel lk points lives shbool eph dir2 sn2
               | bomb ->
                  niveaux.(lk).objets.(yb).(xb) <- none;
                  test_shield tk2 teph2 vel lk points lives shbool eph dir2 sn2
               | shield (_) ->
                  affiche_bouclier ();
                  niveaux.(lk).objets.(yb).(xb) <- none;
                  boucle tk2 teph2 vel lk points lives true eph dir2 sn2
               | life (_, i) ->
                  affiche_vies (lives + 1);
                  niveaux.(lk).objets.(yb).(xb) <- none;
                  boucle tk2 teph2 vel lk points (lives + i) shbool eph dir2 sn2
               | warp (_, p) ->
                  niveaux.(lk).objets.(yb).(xb) <- none;
                  boucle tk2 teph2 vel lk points lives shbool eph dir2 (warp_snake lk dir2 p sn2)
               | slow (_, f) ->
                  affiche_vitesse (1. /. (10. *. f *. vel));
                  niveaux.(lk).objets.(yb).(xb) <- none;
                  boucle tk2 teph2 (f *. vel) lk points lives shbool eph dir2 sn2
               | speed (_, f) ->
                  affiche_vitesse (1. /. (10. *. f *. vel));
                  niveaux.(lk).objets.(yb).(xb) <- none;
                  boucle tk2 teph2 (f *. vel) lk points lives shbool eph dir2 sn2
               | score (_, i) ->
                  affiche_score (points + i);
                  niveaux.(lk).objets.(yb).(xb) <- none;
                  boucle tk2 teph2 vel lk (points + i) lives shbool eph dir2 (expand_snake dir2 snake)
   in boucle ti 0. 0.1 0 0 3 false [] {x = 1; y = 0} (ini_snake 0);;

jeu ();;