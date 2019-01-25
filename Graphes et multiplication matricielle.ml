(*Graphes*)

let random_matrix n p sup =
   let res = make_matrix n p 0 in
      for i = 0 to n - 1 do
         for j = 0 to p - 1 do
            res.(i).(j) <- random__int sup
         done;
      done;
      res;;
random_matrix 3 12 5;;

let random_graph_matrix nbs p =
  let m=make_matrix nbs nbs 0 in
  for i = 0 to nbs-1 do
    for j = i+1 to nbs-1 do
      if random__float 1.0 < p then begin
        m.(i).(j) <- 1;
        m.(j).(i) <- 1
      end
    done
  done;
  m;;
random_graph_matrix 10 0.1;;

let graph_matrix_of_list v =
   let n = vect_length v in
      let res = make_matrix n n 0 in
         let rec remplit i = function
            | [] -> ()
            | j :: q -> res.(i).(j) <- 1; remplit i q
         in
            for i = 0 to n - 1 do
               remplit i v.(i)
            done;
            res;;

let graph_list_of_matrix v =
   let n = vect_length v in
      let res = make_vect n [] in
         for i = 0 to n - 1 do
            for j = 0 to n - 1 do
               if v.(i).(j) = 1
               then res.(i) <- j :: res.(i)
            done;
         done;
         res;;

#open "graphics";;

let width=512;;
let height=512;;

open_graph (" "^(string_of_int width)^"x"^(string_of_int height)) ;;
clear_graph();;

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

clear_graph();
trace_les_sommets 30 black;;
trace_arete 30 24 21 (rgb 100 150 120);;
trace_arete 30 1 9 black;;
trace_arete 30 0 3 green;;
clear_graph();;
let m = graph_list_of_matrix (random_graph_matrix 10 0.1) in
trace_graphe m red blue;;
clear_graph ();;







(*Méthode vue en TD*)

let rec insere i = function
   (*insère l'élément i dans la liste l*)
   | [] -> [i]
   | j :: l when i <> j -> j :: (insere i l)
   | l -> l;;

let rec uno_list t =
   let n = vect_length t in
      let ajoute i j =
         t.(j) <- insere i t.(j)
      in
         for i = 0 to n - 1 do
            do_list (ajoute i) t.(i)
         done;
         t;;

let uno_matrix m =
   let n = vect_length m in
      for i = 0 to n - 1 do
         for j = 0 to n - 1 do
            if m.(i).(j) = 1
            then m.(j).(i) <- 1
         done;
      done;
      m;;

let I n =
	(*matrice identité de taille n*)
   let res = make_matrix n n 0 in
      for i = 0 to n - 1 do
         res.(i).(i) <- 1
      done;
      res;;

let mult_naif m1 m2 =
	(*multiplication matricielle en O(npq)*)
   if m1 = [||] or m2 = [||]
   then [||]
   else
      let n = vect_length m1
      and p = vect_length m2
      and q = vect_length m2.(0) in
         if vect_length m1.(0) <> p
         then failwith "multiplication matricielle impossible"
         else
            let res = make_matrix n q 0 in
               for i = 0 to n - 1 do
                  for j = 0 to q - 1 do
                     for k = 0 to p - 1 do
                        res.(i).(j) <- res.(i).(j) + m1.(i).(k) * m2.(k).(j)
                     done;
                  done;
               done;
               res;;
mult_naif (I 3) (I 3);;

let pow_naif m k =
   (*exponentiation rapide matricielle en O(ln(k)*n^3)*)
   if m = [||]
   then [||]
   else
      let n = vect_length m in
         if vect_length m.(0) <> n
         then failwith "multiplication matricielle impossible"
         else
            let mult m1 m2 =
               let res = make_matrix n n 0 in
                  for i = 0 to n - 1 do
                     for j = 0 to n - 1 do
                        for k = 0 to n - 1 do
                           res.(i).(j) <- res.(i).(j) + m1.(i).(k) * m2.(k).(j)
                        done;
                     done;
                  done;
                  res
            in
               let rec boucle m2 k = match m2 with
                  | _ when k = 0 -> I n
                  | m2 when k = 1 -> m2
                  | m2 when k mod 2 = 0 -> boucle (mult m2 m2) (k / 2)
                  | m2 -> mult m2 (boucle (mult m2 m2) ((k - 1) / 2))
               in boucle m k;;
pow_naif (I 5) 6;;
pow_naif (random_matrix 8 8 5) 6;;
pow_naif (random_graph_matrix 10 0.1) 6;;

(*
let add m1 m2 =
   (*additionne les deux matrices m1 et m2*)
   if m1 = [||] or m2 = [||]
   then failwith "addition matricielle impossible"
   else
      let n = vect_length m1
      and p = vect_length m1.(0) in
         if vect_length m2 <> n or vect_length m2.(0) <> p
         then failwith "addition matricielle impossible"
         else
            let res = make_matrix n p 0 in
               for i = 0 to n - 1 do
                  for j = 0 to p - 1 do
                     res.(i).(j) <- m1.(i).(j) + m2.(i).(j)
                  done;
               done;
               res;;
add (I 3) (I 3);;

let sub m1 m2 =
   (*soustrait les deux matrices m1 et m2*)
   if m1 = [||] or m2 = [||]
   then failwith "addition matricielle impossible"
   else
      let n = vect_length m1
      and p = vect_length m1.(0) in
         if vect_length m2 <> n or vect_length m2.(0) <> p
         then failwith "addition matricielle impossible"
         else
            let res = make_matrix n p 0 in
               for i = 0 to n - 1 do
                  for j = 0 to p - 1 do
                     res.(i).(j) <- m1.(i).(j) - m2.(i).(j)
                  done;
               done;
               res;;
sub (I 3) (I 3);;

let redim m =
   (*redimensionne une matrice pour l'algorithme de Strassen*)
   if m = [||]
   then failwith "multiplication matricielle impossible"
   else
      let n = vect_length m
      and p = vect_length m.(0) in
         let n2 = puis2sup (max n p) in
            let res = make_matrix n2 n2 0 in
               for i = 0 to n - 1 do
                  for j = 0 to n - 1 do
                     res.(i).(j) <- m.(i).(j)
                  done;
               done;
               res;;
redim (I 3);;

let sub_matrix m =
   (*extrait les 4 matrices carrées d'une matrice carrée de taille n=2^p*)
   let n = vect_length m / 2 in
      let m11 = make_matrix n n 0
      and m12 = make_matrix n n 0
      and m21 = make_matrix n n 0
      and m22 = make_matrix n n 0 in
         for i = 0 to n - 1 do
            for j = 0 to n - 1 do
               m11.(i).(j) <- m.(i).(j)
            done;
         done;
         for i = 0 to n - 1 do
            for j = 0 to n - 1 do
               m12.(i).(j) <- m.(i).(j + n)
            done;
         done;
         for i = 0 to n - 1 do
            for j = 0 to n - 1 do
               m21.(i).(j) <- m.(i + n).(j)
            done;
         done;
         for i = 0 to n - 1 do
            for j = 0 to n - 1 do
               m22.(i).(j) <- m.(i + n).(j + n)
            done;
         done;
         m11, m12, m21, m22;;
sub_matrix (I 4);;

let concat_matrix p11 p12 p21 p22 =
   (*concatène les 4 matrices pour reformer une matrice carrée de taille n=2^p*)
   let n = vect_length p11 in
      let res = make_matrix (2 * n) (2 * n) 0 in
         for i = 0 to n - 1 do
            for j = 0 to n - 1 do
               res.(i).(j) <- p11.(i).(j)
            done;
         done;
         for i = 0 to n - 1 do
            for j = 0 to n - 1 do
               res.(i).(j + n) <- p12.(i).(j)
            done;
         done;
         for i = 0 to n - 1 do
            for j = 0 to n - 1 do
               res.(i + n).(j) <- p21.(i).(j)
            done;
         done;
         for i = 0 to n - 1 do
            for j = 0 to n - 1 do
               res.(i + n).(j + n) <- p22.(i).(j)
            done;
         done;
         res;;
concat_matrix (I 2) (I 2) (I 2) (I 2);;
*)

let rec puis2sup n1 =
   let rec boucle n2 = function
      | i when n2 < n1 -> boucle (2 * n2) (i + 1)
      | i -> n2
   in boucle 1 0;;
puis2sup 11;;

let mult_strassen m1 m2 =
   (*multiplication matricielle par l'algorithme de Strassen en O(n^2,8)*)
   if m1 = [||] or m2 = [||]
   then failwith "multiplication matricielle impossible"
   else
      let n = vect_length m1
      and p = vect_length m2
      and q = vect_length m2.(0) in
         if vect_length m1.(0) <> p
         then failwith "multiplication matricielle impossible"
         else
            let zeros m n p =
            	let bool = ref true
            	and i = ref 0
            	and j = ref 0 in
            	while !i < n-1 & !bool = true do
            		j:= 0;
            		while !j < p-1 & !bool = true do
            			if m.(!i).(!j) <> 0 then bool := false;
            			incr j
            		done;
            		incr i
            	done;
            	!bool
            in
            if zeros m1 n p or zeros m2 p q
            then make_matrix n q 0
            else
            let redim m n p n2 =
               let res = make_matrix n2 n2 0 in
                  for i = 0 to n - 1 do
                     for j = 0 to n - 1 do
                        res.(i).(j) <- m.(i).(j)
                     done;
                  done;
                  res
            and sub_matrix1 m n2 =
               let m11 = make_matrix n2 n2 0
               and m12 = make_matrix n2 n2 0
               and m21 = make_matrix n2 n2 0
               and m22 = make_matrix n2 n2 0 in
                  for i = 0 to n2 - 1 do
                     for j = 0 to n2 - 1 do
                        m11.(i).(j) <- m.(i).(j)
                     done;
                  done;
                  for i = 0 to n2 - 1 do
                     for j = 0 to n2 - 1 do
                        m12.(i).(j) <- m.(i).(j + n2)
                     done;
                  done;
                  for i = 0 to n2 - 1 do
                     for j = 0 to n2 - 1 do
                        m21.(i).(j) <- m.(i + n2).(j)
                     done;
                  done;
                  for i = 0 to n2 - 1 do
                     for j = 0 to n2 - 1 do
                        m22.(i).(j) <- m.(i + n2).(j + n2)
                     done;
                  done;
                  m11, m12, m21, m22
            and sub_matrix2 m n p =
               let res = make_matrix n p 0 in
                  for i = 0 to n - 1 do
                     for j = 0 to p - 1 do
                        res.(i).(j) <- m.(i).(j)
                     done;
                  done;
                  res
            and add m1 m2 n5 =
               let res = make_matrix n5 n5 0 in
                  for i = 0 to n5 - 1 do
                     for j = 0 to n5 - 1 do
                        res.(i).(j) <- m1.(i).(j) + m2.(i).(j)
                     done;
                  done;
                  res
            and sub m1 m2 n5 =
               let res = make_matrix n5 n5 0 in
                  for i = 0 to n5 - 1 do
                     for j = 0 to n5 - 1 do
                        res.(i).(j) <- m1.(i).(j) - m2.(i).(j)
                     done;
                  done;
                  res
            and concat_matrix p11 p12 p21 p22 n5 =
               let res = make_matrix (2 * n5) (2 * n5) 0 in
                  for i = 0 to n5 - 1 do
                     for j = 0 to n5 - 1 do
                        res.(i).(j) <- p11.(i).(j)
                     done;
                  done;
                  for i = 0 to n5 - 1 do
                     for j = 0 to n5 - 1 do
                        res.(i).(j + n5) <- p12.(i).(j)
                     done;
                  done;
                  for i = 0 to n5 - 1 do
                     for j = 0 to n5 - 1 do
                        res.(i + n5).(j) <- p21.(i).(j)
                     done;
                  done;
                  for i = 0 to n5 - 1 do
                     for j = 0 to n5 - 1 do
                        res.(i + n5).(j + n5) <- p22.(i).(j)
                     done;
                  done;
                  res
            in
               let n3 = puis2sup (max n (max p q)) in
                  let t1 = redim m1 n p n3
                  and t2 = redim m2 p q n3 in
                     let rec boucle m n n4 =
                        if n4 = 1
                        then [|[|m.(0).(0) * n.(0).(0)|]|]
                        else
                           let n5 = n4 / 2 in
                              let m11, m12, m21, m22 = sub_matrix1 m n5
                              and n11, n12, n21, n22 = sub_matrix1 n n5 in
                                 let x1 = boucle (add m11 m22 n5) (add n11 n22 n5) n5
                                 and x2 = boucle (add m21 m22 n5) (n11) n5
                                 and x3 = boucle (m11) (sub n12 n22 n5) n5
                                 and x4 = boucle (m22) (sub n21 n11 n5) n5
                                 and x5 = boucle (add m11 m12 n5) (n22) n5
                                 and x6 = boucle (sub m21 m11 n5) (add n11 n12 n5) n5
                                 and x7 = boucle (sub m12 m22 n5) (add n21 n22 n5) n5 in
                                    let p11 = add (sub x4 x5 n5) (add x1 x7 n5) n5
                                    and p12 = add x3 x5 n5
                                    and p21 = add x2 x4 n5
                                    and p22 = add (sub x1 x2 n5) (add x3 x6 n5) n5 in
                                       concat_matrix p11 p12 p21 p22 n5
                     in sub_matrix2 (boucle t1 t2 n3) n q;;
mult_strassen (I 10) (I 10);;
mult_strassen (make_matrix 10 10 0) (I 10);;

let pow_strassen m k =
   (*exponentiation rapide matricielle par l'algorithme de Strassen en O(ln(k)*n^2,8)*)
   if m = [||]
   then [||]
   else
      let zeros m n4 =
         let bool = ref true
         and i = ref 0
         and j = ref 0 in
            while !i < n4 & !bool = true do
               j := 0;
               while !j < n4 & !bool = true do
                  if m.(!i).(!j) <> 0 then bool := false;
                  incr j
               done;
               incr i
            done;
            !bool
      and redim m n p n2 =
         let res = make_matrix n2 n2 0 in
            for i = 0 to n - 1 do
               for j = 0 to n - 1 do
                  res.(i).(j) <- m.(i).(j)
               done;
            done;
            res
      and sub_matrix1 m n4 =
         let m11 = make_matrix n4 n4 0
         and m12 = make_matrix n4 n4 0
         and m21 = make_matrix n4 n4 0
         and m22 = make_matrix n4 n4 0 in
            for i = 0 to n4 - 1 do
               for j = 0 to n4 - 1 do
                  m11.(i).(j) <- m.(i).(j)
               done;
            done;
            for i = 0 to n4 - 1 do
               for j = 0 to n4 - 1 do
                  m12.(i).(j) <- m.(i).(j + n4)
               done;
            done;
            for i = 0 to n4 - 1 do
               for j = 0 to n4 - 1 do
                  m21.(i).(j) <- m.(i + n4).(j)
               done;
            done;
            for i = 0 to n4 - 1 do
               for j = 0 to n4 - 1 do
                  m22.(i).(j) <- m.(i + n4).(j + n4)
               done;
            done;
            m11, m12, m21, m22
      and sub_matrix2 m n p =
         let res = make_matrix n p 0 in
            for i = 0 to n - 1 do
               for j = 0 to p - 1 do
                  res.(i).(j) <- m.(i).(j)
               done;
            done;
            res
      and add m1 m2 n5 =
         let res = make_matrix n5 n5 0 in
            for i = 0 to n5 - 1 do
               for j = 0 to n5 - 1 do
                  res.(i).(j) <- m1.(i).(j) + m2.(i).(j)
               done;
            done;
            res
      and sub m1 m2 n5 =
         let res = make_matrix n5 n5 0 in
            for i = 0 to n5 - 1 do
               for j = 0 to n5 - 1 do
                  res.(i).(j) <- m1.(i).(j) - m2.(i).(j)
               done;
            done;
            res
      and concat_matrix p11 p12 p21 p22 n4 =
         let res = make_matrix (2 * n4) (2 * n4) 0 in
            for i = 0 to n4 - 1 do
               for j = 0 to n4 - 1 do
                  res.(i).(j) <- p11.(i).(j)
               done;
            done;
            for i = 0 to n4 - 1 do
               for j = 0 to n4 - 1 do
                  res.(i).(j + n4) <- p12.(i).(j)
               done;
            done;
            for i = 0 to n4 - 1 do
               for j = 0 to n4 - 1 do
                  res.(i + n4).(j) <- p21.(i).(j)
               done;
            done;
            for i = 0 to n4 - 1 do
               for j = 0 to n4 - 1 do
                  res.(i + n4).(j + n4) <- p22.(i).(j)
               done;
            done;
            res
      in
         let rec mult m n n3 =
            if n3 = 1
            then [|[|m.(0).(0) * n.(0).(0)|]|]
            else
            if n3 < 4
            then mult_naif m n
            else
            if zeros m n3 or zeros n n3
            then make_matrix n3 n3 0
            else
               let n4 = n3 / 2 in
                  let m11, m12, m21, m22 = sub_matrix1 m n4
                  and n11, n12, n21, n22 = sub_matrix1 n n4 in
                     let x1 = mult (add m11 m22 n4) (add n11 n22 n4) n4
                     and x2 = mult (add m21 m22 n4) (n11) n4
                     and x3 = mult (m11) (sub n12 n22 n4) n4
                     and x4 = mult (m22) (sub n21 n11 n4) n4
                     and x5 = mult (add m11 m12 n4) (n22) n4
                     and x6 = mult (sub m21 m11 n4) (add n11 n12 n4) n4
                     and x7 = mult (sub m12 m22 n4) (add n21 n22 n4) n4 in
                        let p11 = add (sub x4 x5 n4) (add x1 x7 n4) n4
                        and p12 = add x3 x5 n4
                        and p21 = add x2 x4 n4
                        and p22 = add (sub x1 x2 n4) (add x3 x6 n4) n4 in
                           concat_matrix p11 p12 p21 p22 n4
         in
            let n = vect_length m
            and p = vect_length m.(0) in
               let n2 = puis2sup (max n p) in
                  let t = redim m n p n2 in
                     let rec boucle m k = match m with
                        | _ when k = 0 -> I n2
                        | m when k = 1 -> m
                        | m when k mod 2 = 0 -> boucle (mult m m n2) (k / 2)
                        | m -> mult m (boucle (mult m m n2) ((k - 1) / 2)) n2
                     in sub_matrix2 (boucle t k) n p;;
pow_strassen (I 16) 6;;
pow_strassen (make_matrix 12 12 1) 10;;
pow_naif (make_matrix 12 12 1) 10;;
pow_strassen (random_matrix 8 8 5) 6;;
pow_strassen (random_graph_matrix 10 0.1) 6;;

let compo_conn_TD a =
   let n = vect_length a in
      let an = pow_naif a (n - 1) in
         let res = make_vect n [] in
            for i = 0 to n - 1 do
               for j = 0 to n - 1 do
                  if an.(i).(j) <> 0
                  then res.(i) <- j :: res.(i)
               done;
            done;
            let rec simplifie = function
               | i when i = n -> []
               | i ->
                  let bool = ref false in
                     for j = 0 to i - 1 do
                        bool := !bool or mem i res.(j)
                     done;
                     if !bool
                     then simplifie (i + 1)
                     else (i :: res.(i)) :: simplifie (i + 1)
            in simplifie 0;;

let trace_compo_conn_TD graphe =
   let t = compo_conn_TD graphe in
      let points = vect_length graphe
      and j = ref 1 in
         let rec affiche j = function
            | [] -> incr (j)
            | [a] -> incr (j)
            | a :: b :: q -> let c = (!j * 255 / points) in trace_arete points a b (rgb (255 - c) 0 c); affiche j (b :: q)
         in do_list (affiche j) t;;





(*Autre méthode vue en TP*)

let tableau_compo_conn_TP graphe =
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

let compo_conn_TP graphe =
	liste_composantes_of_vect (tableau_compo_conn_TP graphe);;

let trace_compo_conn_TP graphe =
   let t = compo_conn_TP graphe in
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



compo_conn_TD (random_graph_matrix 20 0.1);;
clear_graph ();
let n = 20 in
   let p = (1. /. (float_of_int n)) in
      let m = random_graph_matrix n p in
         trace_les_sommets n black;
         trace_compo_conn_TD m;;
clear_graph ();;

compo_conn_TP (graph_list_of_matrix (random_graph_matrix 20 0.1));;
clear_graph ();
let n = 20 in
   let p = (1. /. (float_of_int n)) in
      let m = graph_list_of_matrix (random_graph_matrix n p) in
         trace_les_sommets n black;
         trace_compo_conn_TP m;;
clear_graph ();;