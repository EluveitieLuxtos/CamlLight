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
    | Var s -> {�tiquette = s;branches = []}
    | Const x -> {�tiquette = string_of_float x;branches = []}
    | Log e -> {�tiquette = "Log";branches = [f e]}
    | Exp e -> {�tiquette = "Exp";branches = [f e]}
    | Add(e,e') -> {�tiquette = "+";branches = [f e;f e']} 
    | Sub(e,e') -> {�tiquette = "-";branches = [f e;f e']} 
    | Mult(e,e') -> {�tiquette = "*";branches = [f e;f e']} 
    | Div(e,e') -> {�tiquette = "/";branches = [f e;f e']} in
  print_string_arbre (f e)
;;

install_printer "print_expr";;

let exemple = Log (Add (Exp (Var "x"),Mult (Var "x",Const 2.0)));;

