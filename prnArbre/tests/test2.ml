#directory "..";;
include "prnArbre.ini";;

type expr =
  | Var of string
  | Const of float
  | Log of expr
  | Exp of expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
;; 

let print_expr e =
  let rec f = function
    | Var s -> {étiquette = s;branches = []}
    | Const x -> {étiquette = string_of_float x;branches = []}
    | Log e -> {étiquette = "Log";branches = [f e]}
    | Exp e -> {étiquette = "Exp";branches = [f e]}
    | Add(e,e') -> {étiquette = "+";branches = [f e;f e']} 
    | Sub(e,e') -> {étiquette = "-";branches = [f e;f e']} 
    | Mult(e,e') -> {étiquette = "*";branches = [f e;f e']} 
    | Div(e,e') -> {étiquette = "/";branches = [f e;f e']} in
  print_string_arbre (f e)
;;

install_printer "print_expr";;

let exemple = Log (Add (Exp (Var "x"),Mult (Var "x",Const 2.0)));;

