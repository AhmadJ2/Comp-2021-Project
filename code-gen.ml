#use "semantic-analyser.ml";;
open Semantics;;

(* This module is here for you convenience only!
   You are not required to use it.
   you are allowed to change it. *)
module type CODE_GEN = sig
  (* This signature assumes the structure of the constants table is
     a list of key-value pairs:
     - The keys are constant values (Sexpr(x) or Void)
     - The values are pairs of:
       * the offset from the base const_table address in bytes; and
       * a string containing the byte representation (or a sequence of nasm macros)
         of the constant value
     For example: [(Sexpr(Nil), (1, "T_NIL"))]
   *)
  val make_consts_tbl : expr' list -> (constant * (int * string)) list

  (* This signature assumes the structure of the fvars table is
     a list of key-value pairs:
     - The keys are the fvar names as strings
     - The values are the offsets from the base fvars_table address in bytes
     For example: [("boolean?", 0)]
   *)  
  val make_fvars_tbl : expr' list -> (string * int) list

  (* If you change the types of the constants and fvars tables, you will have to update
     this signature to match: The first argument is the constants table type, the second 
     argument is the fvars table type, and the third is an expr' that has been annotated 
     by the semantic analyser.
   *)
  val generate : (constant * (int * string)) list -> (string * int) list -> expr' -> string
end;;


let rec mct acc exp = 
    match exp with 
    | Const'(Void) -> if (List.exists (fun (con, (inte, str)) -> str = "T_VOID") acc) then (acc@[(Void,(List.length acc, "T_VOID"))]) else (acc)
    | Const'(Sexpr(e))->
        (match e with
        | String(name)-> if (false = List.exists (fun (con, (inte, str)) -> con = (Sexpr(String(name)))) acc) then (acc@[(Sexpr(e),(List.length acc, "T_STRING"))]) else (acc)
        | Bool(bo) -> if (false = List.exists (fun (con, (inte, str)) -> con = (Sexpr(Bool(bo)))) acc) then (acc@[(Sexpr(e),(List.length acc, "T_BOOL"))]) else (acc)
        | Char(ch) -> if (false = List.exists (fun (con, (inte, str)) -> con = (Sexpr(Char(ch)))) acc) then (acc@[(Sexpr(e),(List.length acc, "T_CHAR"))]) else (acc)
        | Number(Fraction(num, den)) -> if (false = List.exists (fun (con, (inte, str)) -> con = (Sexpr(Number(Fraction(num, den))))) acc) then (acc@[(Sexpr(e),(List.length acc, "T_RATIONAL"))]) else (acc)
        | Number(Float(flo)) -> if (false = List.exists (fun (con, (inte, str)) -> con = (Sexpr(Number(Float(flo))))) acc) then (acc@[(Sexpr(e),(List.length acc, "T_FLOAT"))]) else (acc)
        | Nil -> if (false = List.exists (fun (con, (inte, str)) -> con = (Sexpr(Nil))) acc) then (acc@[(Sexpr(e),(List.length acc, "T_NIL"))]) else (acc)
        | _ -> acc)
    | If'(tst, thn, alt) -> mct (mct (mct acc tst) thn) alt
    | Seq'(seq) -> List.fold_left mct acc seq
    | Or'(lst) -> List.fold_left mct acc lst
    | Def'(_, e) -> mct acc e
    | Set'(_, e) -> mct acc e 
    | BoxSet'(_, e) -> mct acc e 
    | LambdaSimple' (_, e) -> mct acc e
    | LambdaOpt' (_, _, e) -> mct acc e 
    | Applic' (op, ap) -> List.fold_left mct (mct acc op) ap
    | ApplicTP' (op, ap) -> List.fold_left mct (mct acc op) ap
    | _ -> acc;;

    

let rec mft acc exp = 
    match exp with 


    | Var' (VarFree(e))-> if (false = List.exists (fun (str , inte)-> str = e) acc) then (acc@[(e, List.length acc)]) else acc 
    | If'(tst, thn, alt) -> mft (mft (mft acc tst) thn) alt
    | Seq'(seq) -> List.fold_left mft acc seq
    | Or'(lst) -> List.fold_left mft acc lst
    | Def'(f, e) -> mft (mft acc (Var'(f))) e
    | Set'(f, e) -> mft (mft acc (Var'(f))) e
    | BoxSet'(f, e) -> mft (mft acc (Var'(f))) e
    | LambdaSimple' (_, e) -> mft acc e
    | LambdaOpt' (_, _, e) -> mft acc e 
    | Applic' (op, ap) -> List.fold_left mft (mft acc op) ap
    | ApplicTP' (op, ap) -> List.fold_left mft (mft acc op) ap
    | _ -> acc;;

let g = raise X_not_yet_implemented;;




module Code_Gen : CODE_GEN = struct
  let make_consts_tbl asts = List.fold_left mct [] asts;;
  let make_fvars_tbl asts = List.fold_left mft [] asts;;

  let generate consts fvars e = raise X_not_yet_implemented;;
end;;

