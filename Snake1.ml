(*Snake*)

(*taille de la fenêtre et des anneaux du serpent*)
let res = 600
and anneaux = 8;;

#open "graphics";;

let pi = 4.*.atan 1.;;
type point = {x: int; y: int};;
type vecteur = {xn: int; yn: int};;
type segment =
	|Z of point * int
	|Q of point * int
	|S of point * int
	|D of point * int;;

type figure =
   | rect of int * int
   | cercle of int
   | ellipse of int * int
   | poly of point vect;;

type motif =
	| unif of color
   | reg_quad of int * int * color
   (*quadrillage avec des rectangles longueur * largeur*)
   | quad of (int * int * color) vect * int
   (*rectangles longueur * largeur, décalage entre une ligne et la suivante*)
   | mosa of (int * int * color vect) vect * int * int
   (*pavage circulaire contenant un tableau (rayon horizontal * rayon vertical * couleur) et les angles de début et de fin en degrés*)
   | reg_trigpoly of point * point * point * image
   (*prend un pavage à base de triangle en l'appliquant au triangle défini *)
   | reg_quadpoly of point * point * point * point * image
   (*prend un pavage à base de quadrilatère en l'appliquant au quadrilatère défini *)
   | reg_hexapoly of point * point * point * point * point * point * image
   (*prend un pavage à base d'hexagone en l'appliquant à l'hexagone défini *);;

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
   | score of float * int * figure * motif;;

type dessin =
   {figure: figure;
      motif: motif};;

type niveau =
   {tk: float;			(*durée du niveau et durée maximale des éphémères*)
      teph: float;	(*durée d'actualisation des objets éphémères*)
      crossx: bool;
      crossy: bool;
      nbobj:int;
      objets: objet vect vect;
      decor: dessin list};;

let random_p () =
   (*un pixel*)
   {x = random__int res; y = random__int res};;

let random_pos () =
	(*position atteignable par le serpent*)
   {x = random__int (res / anneaux) * anneaux; y = random__int (res / anneaux) * anneaux};;

let random_c () =
   rgb (random__int 255) (random__int 255) (random__int 255);;

let random_b tk dt =
   (*renvoie un bonus aléatoire, dont la date de péremption est comprise entre tk et tk+dt pour les éphémères*)
   let i = random__int 7 in
      match i with
      | 0 -> bomb
      | 1 -> shield (random__float dt +. tk)
      | 2 -> life (random__float dt +. tk, random__int 2)
      | 3 -> warp (random__float dt +. tk, random_pos ())
      | 4 -> slow (random__float dt +. tk, random__float 0.5 +. 0.5)
      | 5 -> speed (random__float dt +. tk, random__float 0.5 +. 1.)
      | 6 -> score (random__float dt +. tk, random__int 10, cercle (3), unif (random_c ()))
      | _ -> none;;

let rec random_blist tk dt = function
   | i when i <= 0 -> []
   | i -> random_b tk dt :: random_blist tk dt (i - 1);;

let init_objets () =
   let n = res / anneaux in
      make_matrix n n none;;

let niveaux =
   [|{tk = 120.; teph = 5.; crossx = false; crossy = false; nbobj = 5; objets = init_objets (); decor = []}|];;

let reset_niveaux () =
	(*reset les objets des niveaux à chaque fois qu'on recommence à jouer*)
   let n = res / anneaux
   and reset i j niveau = niveau.objets.(i).(j) <- none in
      for i = 0 to n - 1 do
         for j = 0 to n - 1 do
            do_vect (reset i j) niveaux
         done;
      done;;







let duree_correcte dt =
   let nbniv = vect_length niveaux in
      if dt < niveaux.(nbniv - 1).tk
      then true
      else false;;

let getkey tk vel dir =
	(*renvoie la direction choisie par l'utilisateur (précédente configuration sinon)*)
   let p = ref dir in
      while sys__time () < tk +. vel do
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

let movep p1 p2 coef2 =
	(*deplace le point p1 de coef2 fois le point p2*)
   {x = p1.x + coef2 * p2.x; y = p1.y + coef2 * p2.y};;

let rot dir i =
	(*effectue la rotation d'un vecteur de direction dans le sens donné par i*)
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
   else dir;;

let efface_tete t dir =
   (*efface la tête tracée et la remplace par le corps*)
   let p1 = movep t dir (anneaux + 2)
   and p2 = movep t (rot dir 1) anneaux
   and p3 = movep t (rot dir (- 1)) anneaux in
      set_color white;
      fill_poly [|p1.x, p1.y; p2.x, p2.y; p3.x, p3.y|];
      set_color black;
      fill_circle t.x t.y (anneaux / 2 + 1);;

let affiche_tete t dir =
   (*affiche la tête dans la bonne direction*)
   let p1 = movep t dir (anneaux + 2)
   and p2 = movep t (rot dir 1) anneaux
   and p3 = movep t (rot dir (- 1)) anneaux in
      set_color black;
      fill_poly [|p1.x, p1.y; p2.x, p2.y; p3.x, p3.y|];;

let extrait_tete = function
   (*renvoie la tête du serpent*)
   | [] -> {x = 0; y = 0}
   | Z (p, l) :: q -> p
   | Q (p, l) :: q -> p
   | S (p, l) :: q -> p
   | D (p, l) :: q -> p;;

let except_tete = function
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
      else D (movep p {x = - 1; y = 0} anneaux, l - 1) :: q;;

let concat_tete t = function
   (*rajoute une tête déjà calculée au corps du serpent*)
   | [] -> []
   | Z (p, l) :: q -> Z (t, l + 1) :: q
   | Q (p, l) :: q -> Q (t, l + 1) :: q
   | S (p, l) :: q -> S (t, l + 1) :: q
   | D (p, l) :: q -> D (t, l + 1) :: q;;

let rec except_queue = function
   (*enlève la queue du serpent*)
   | [] -> []
   | [Z (p, l)] ->
      set_color white;
      fill_circle p.x (p.y - anneaux * (l - 1)) (anneaux / 2 + 1);
      if l = 1
      then []
      else [Z (p, l - 1)]
   | [Q (p, l)] ->
      set_color white;
      fill_circle (p.x + anneaux * (l - 1)) p.y (anneaux / 2 + 1);
      if l = 1
      then []
      else [Q (p, l - 1)]
   | [S (p, l)] ->
      set_color white;
      fill_circle p.x (p.y + anneaux * (l - 1)) (anneaux / 2 + 1);
      if l = 1
      then []
      else [S (p, l - 1)]
   | [D (p, l)] ->
      set_color white;
      fill_circle (p.x - anneaux * (l - 1)) p.y (anneaux / 2 + 1);
      if l = 1
      then []
      else [D (p, l - 1)]
   | t :: q -> t :: except_queue q;;

let efface_serpent snake dir =
   let t = extrait_tete snake in
      efface_tete t dir;
      set_color white;
      let rec boucle = function
         | [] -> ()
         | Z (p, l) :: q ->
            for i = 0 to l - 1 do
               fill_circle p.x (p.y - anneaux * i) (anneaux / 2 + 1)
            done;
            boucle q
         | Q (p, l) :: q ->
            for i = 0 to l - 1 do
               fill_circle (p.x + anneaux * i) p.y (anneaux / 2 + 1)
            done;
            boucle q
         | S (p, l) :: q ->
            for i = 0 to l - 1 do
               fill_circle p.x (p.y + anneaux * i) (anneaux / 2 + 1)
            done;
            boucle q
         | D (p, l) :: q ->
            for i = 0 to l - 1 do
               fill_circle (p.x - anneaux * i) p.y (anneaux / 2 + 1)
            done;
            boucle q
      in boucle snake;;

let rec cut_snake t = function
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
      else cut_snake t q;;

let move_snake dir = function
   (*déplace la tete du serpent en retirant la queue puis affiche le serpent résultant*)
   | [] -> []
   | Q (p, l) :: q ->
      let t = movep p dir anneaux
      and q2 = except_queue (Q (p, l) :: q) in
         affiche_tete t dir;
         if dir.y = 0
         then concat_tete t q2
         else
         if dir.y = 1
         then Z (t, 1) :: q2
         else S (t, 1) :: q2
   | D (p, l) :: q ->
      let t = movep p dir anneaux
      and q2 = except_queue (D (p, l) :: q) in
         affiche_tete t dir;
         if dir.y = 0
         then concat_tete t q2
         else
         if dir.y = 1
         then Z (t, 1) :: q2
         else S (t, 1) :: q2
   | Z (p, l) :: q ->
      let t = movep p dir anneaux
      and q2 = except_queue (Z (p, l) :: q) in
         affiche_tete t dir;
         if dir.x = 0
         then concat_tete t q2
         else
         if dir.x = 1
         then D (t, 1) :: q2
         else Q (t, 1) :: q2
   | S (p, l) :: q ->
      let t = movep p dir anneaux
      and q2 = except_queue (S (p, l) :: q) in
         affiche_tete t dir;
         if dir.x = 0
         then concat_tete t q2
         else
         if dir.x = 1
         then D (t, 1) :: q2
         else Q (t, 1) :: q2;;

let warp_snake dir t = function
   (*téléporte la tete du serpent en retirant la queue puis affiche le serpent résultant*)
   | [] -> []
   | Q (p, l) :: q ->
      let q2 = except_queue (Q (p, l) :: q) in
         affiche_tete t dir;
         if dir.y = 0
         then Q (t, 1) :: q2
         else
         if dir.y = 1
         then Z (t, 1) :: q2
         else S (t, 1) :: q2
   | D (p, l) :: q ->
      let q2 = except_queue (D (p, l) :: q) in
         affiche_tete t dir;
         if dir.y = 0
         then D (t, 1) :: q2
         else
         if dir.y = 1
         then Z (t, 1) :: q2
         else S (t, 1) :: q2
   | Z (p, l) :: q ->
      let q2 = except_queue (Z (p, l) :: q) in
         affiche_tete t dir;
         if dir.x = 0
         then Z (t, 1) :: q2
         else
         if dir.x = 1
         then D (t, 1) :: q2
         else Q (t, 1) :: q2
   | S (p, l) :: q ->
      let q2 = except_queue (S (p, l) :: q) in
         affiche_tete t dir;
         if dir.x = 0
         then S (t, 1) :: q2
         else
         if dir.x = 1
         then D (t, 1) :: q2
         else Q (t, 1) :: q2;;

let expand_snake dir = function
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
         else Q (t, 1) :: S (p, l) :: q;;

let mort () =
   set_color black;
   fill_rect 0 0 res res;
   set_color red;
   moveto ((res - 200) / 2) (res / 2);
   draw_string "Looser. Tu veux recommencer ?";;

let victoire () =
   set_color black;
   fill_rect 0 0 res res;
   set_color blue;
   moveto ((res - 200) / 2) (res / 2);
   draw_string "Victory ! Tu veux recommencer ?";;

let affiche_bonus xb yb = function
   | bomb -> set_color black; fill_circle xb yb (anneaux/2 +1)
   | shield (teph) -> set_color blue; fill_circle xb yb (anneaux/2 +1)
   | life (teph, i) -> set_color red; fill_circle xb yb (anneaux/2 +1)
   | warp (teph, p) -> set_color cyan; fill_circle xb yb (anneaux/2 +1)
   | slow (teph, f) -> set_color magenta; fill_circle xb yb (anneaux/2 +1)
   | speed (teph, f) -> set_color green; fill_circle xb yb (anneaux/2 +1)
   | score (teph, i, f, m) -> set_color yellow; fill_circle xb yb (anneaux/2 +1)
   | none -> set_color white; fill_circle xb yb (anneaux/2 +1)
   | b -> ();;

let rec assign_bonus lk = function
   | [] -> []
   | b :: q ->
      let xb = ref 0
      and yb = ref 0
      and p = ref (random_pos ()) in
         while niveaux.(lk).objets.(!yb / anneaux).(!xb / anneaux) <> none do
            p := random_pos ();
            xb := !p.x;
            yb := !p.y
         done;
         affiche_bonus !xb !yb b;
         niveaux.(lk).objets.(!yb / anneaux).(!xb / anneaux) <- b;
         (!xb, !yb) :: assign_bonus lk q;;

let actu_eph tk = function
	| shield (teph) when tk > teph -> none
   | life (teph,i) when tk > teph -> none
   | warp (teph,p) when tk > teph -> none
   | slow (teph,f) when tk > teph -> none
   | speed (teph,f) when tk > teph -> none
   | score (teph,i,f,m) when tk > teph -> none
   | b -> b;;

let rec actu_ephlist tk lk = function
	(*actualise tous les éphémères et en rajoute de nouveaux*)
   | [] -> let blist = random_blist tk niveaux.(lk).tk niveaux.(lk).nbobj in assign_bonus lk blist
   | (xb, yb) :: q ->
      let o = actu_eph tk niveaux.(lk).objets.(yb / anneaux).(xb / anneaux) in
         if o = none
         then (
               niveaux.(lk).objets.(yb / anneaux).(xb / anneaux) <- none;
               affiche_bonus xb yb none;
               actu_ephlist tk lk q
            )
         else (xb, yb) :: actu_ephlist tk lk q;;




let jeu dt =
   open_graph (string_of_int res ^ "x" ^ string_of_int res);
   clear_graph ();
   if not duree_correcte dt
   then ()
   else
      (
         reset_niveaux ();
         let ti = sys__time ()
         and ini_snake = [D ({x = res / 2; y = res / 2}, 5)] in
            let rec test_shield tk teph vel lk lives shbool eph dir snake =
               if shbool
               then boucle tk teph vel lk lives false eph dir snake
               else (efface_serpent snake dir;
                     boucle tk teph 0.1 lk (lives - 1) false eph {x = 1; y = 0} ini_snake)
            and boucle tk teph vel lk lives shbool eph dir = function
               (*bouge le serpent jusqu'à ce que le joueur perde ou que le temps soit écoulé*)
               | _ when lives = 0 -> mort ()
                  (*défaite*)
               | _ when tk > ti +. dt -> victoire ()
                  (*victoire*)
               | snake when tk > ti +. niveaux.(lk).tk ->
                  clear_graph (); (*affiche_niveau lk;*)
                  boucle tk teph vel (lk + 1) lives shbool [] dir ini_snake
                  (*passage au niveau suivant*)
               | snake when teph > niveaux.(lk).teph ->
                  let eph2 = actu_ephlist tk lk eph in
                     boucle tk 0. vel lk lives shbool eph2 dir snake
                     (*actualise les objets éphémères*)
               | snake ->
                  let dir2 = getkey tk vel dir in
                     efface_tete (extrait_tete snake) dir;
                     if (dir.y = 0 & dir2.x = - dir.x) or (dir.x = 0 & dir2.y = - dir.y)
                     then
                        (efface_serpent snake dir;
                           boucle tk teph 0.1 lk (lives - 1) false eph {x = 1; y = 0} ini_snake)
                     else
                        let sn2 = move_snake dir2 snake in
                           let t = extrait_tete sn2 in
                              if t.x <= 0 or t.x >= res or t.y <= 0 or t.y >= res
                              then test_shield tk teph vel lk lives shbool eph dir2 sn2
                              else
                              if cut_snake t (except_tete sn2)
                              then test_shield tk teph vel lk lives shbool eph dir2 sn2
                              else
                                 let xb, yb = t.x / anneaux, t.y / anneaux
                                 and tk2 = sys__time () in
                                    let obj = niveaux.(lk).objets.(yb).(xb)
                                    and teph2 = teph +. tk2 -. tk in
                                       match obj with
                                       | none ->
                                          boucle tk2 teph2 vel lk lives shbool eph dir2 sn2
                                       | obstacle ->
                                          test_shield tk2 teph2 vel lk lives shbool eph dir2 sn2
                                       | bomb ->
                                          niveaux.(lk).objets.(yb).(xb) <- none;
                                          test_shield tk2 teph2 vel lk lives shbool eph dir2 sn2
                                       | shield (_) ->
                                          niveaux.(lk).objets.(yb).(xb) <- none;
                                          boucle tk2 teph2 vel lk lives true eph dir2 sn2
                                       | life (_, i) ->
                                          niveaux.(lk).objets.(yb).(xb) <- none;
                                          boucle tk2 teph2 vel lk (lives + i) shbool eph dir2 sn2
                                       | warp (_, p) ->
                                          niveaux.(lk).objets.(yb).(xb) <- none;
                                          boucle tk2 teph2 vel lk lives shbool eph dir2 (warp_snake dir2 p sn2)
                                       | slow (_, f) ->
                                          niveaux.(lk).objets.(yb).(xb) <- none;
                                          boucle tk2 teph2 (f *. vel) lk lives shbool eph dir2 sn2
                                       | speed (_, f) ->
                                          niveaux.(lk).objets.(yb).(xb) <- none;
                                          boucle tk2 teph2 (f *. vel) lk lives shbool eph dir2 sn2
                                       | score (_, i, fig, motif) ->
                                          niveaux.(lk).objets.(yb).(xb) <- none;
                                          boucle tk2 teph2 vel lk lives shbool eph dir2 (expand_snake dir2 snake)
            in boucle ti 0. 0.1 0 3 false [] {x = 1; y = 0} ini_snake
      );;




jeu 80.;;


