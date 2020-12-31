#use "semantic-analyser.ml";;
open Semantics;;
exception X_WRONG_TYPE
exception X_not_implemented_codeGen
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
let find_off lst = 
  (if (lst = []) then (0) else (
  let ((exp'), (siz, _)) = (List.nth lst ((List.length lst)-1)) in
  if ( let (a, (_,_)) = (List.nth lst ((List.length lst )-1 )) in a =(Void)) then (1) else (
  (match exp' with
  | Void -> 1+ siz 
  | Sexpr(String(e))-> (String.length e) + 10 + siz
  | Sexpr(Bool(e)) -> 2 +siz
  | Sexpr(Char(e)) -> 2 +siz
  | Sexpr(Nil) -> 1 +siz
  | Sexpr(Number(Float(_)))-> 9 +siz
  | Sexpr(Number(Fraction(_,_)))-> 17 +siz
  | Sexpr(Pair(_,_)) -> 17+siz
  | Sexpr(Symbol(_)) -> 9+siz
  ))))
  

  
let rec mct acc exp = 
    match exp with 
    | Const'(Void) -> if (false = List.exists (fun (con, (inte, str)) -> str = "db T_VOID") acc) then (acc@[(Void,((find_off acc), "db T_VOID"))]) else (acc)
    | Const'(Sexpr(e))->
        (match e with
        | String(name)-> if (false = List.exists (fun (con, (inte, str)) -> con = (Sexpr(String(name)))) acc) then (acc@[(Sexpr(e),((find_off acc), "MAKE_LITERAL_STRING \""^name^"\", " ^string_of_int (String.length name)))]) else (acc)
        | Bool(bo) -> if (false = List.exists (fun (con, (inte, str)) -> con = (Sexpr(Bool(bo)))) acc) then (acc@[(Sexpr(e),((find_off acc), "MAKE_SINGLE_LIT T_BOOL ,"^(if (bo) then ("1") else ("0"))))]) else (acc)
        | Char(ch) -> if (false = List.exists (fun (con, (inte, str)) -> con = (Sexpr(Char(ch)))) acc) then (acc@[(Sexpr(e),((find_off acc), "MAKE_SINGLE_LIT T_CHAR ,"^string_of_int (int_of_char ch)))]) else (acc)
        | Number(Fraction(num, den)) -> if (false = List.exists (fun (con, (inte, str)) -> con = (Sexpr(Number(Fraction(num, den))))) acc) then (acc@[(Sexpr(e),((find_off acc), "MAKE_LITERAL_RATIONAL("^(string_of_int num)^", "^ (string_of_int den)^")"))]) else (acc)
        | Number(Float(flo)) -> if (false = List.exists (fun (con, (inte, str)) -> con = (Sexpr(Number(Float(flo))))) acc) then (acc@[(Sexpr(e),((find_off acc), "MAKE_LITERAL_FLOAT "^(string_of_float flo)^" "))]) else (acc)
        | Nil -> if (false = List.exists (fun (con, (inte, str)) -> con = (Sexpr(Nil))) acc) then (acc@[(Sexpr(e),((find_off acc), "db T_NIL"))]) else (acc)
        | Pair(car, cdr) -> let dis = (mct (mct acc (Const'(Sexpr(car)))) (Const'(Sexpr(cdr)))) in 
          (if (false = (List.exists (fun (con, (inte, str)) -> (con = (Sexpr(Pair(car, cdr))))) (dis))) then 
          (
              (dis)@[(Sexpr(e),((find_off dis),
              "MAKE_LITERAL_PAIR(const_tbl +"^
              let (a, (b, c)) = (List.find (fun (x, (y, z)) -> x = Sexpr(car))  dis) in (string_of_int (b))
              ^", const_tbl +"^
              let (a, (b, c)) = (List.find (fun (x, (y, z)) -> x = Sexpr(cdr))  dis) in (string_of_int (b))
              ^")"))]
            ) (*Make pair will take the offset of car and cdr*)
          else (dis))
        | Symbol(s) -> if (false = List.exists (fun (con, (inte, str)) -> con = (Sexpr(String(s)))) acc) 
        then (
        let newacc =   
        (mct acc (Const'(Sexpr(String(s))))) in newacc@[(Sexpr(e),( (find_off newacc) , ("MAKE_LITERAL_SYMBOL const_tbl+"^ (string_of_int ((find_off newacc) - 10 - (String.length s)) ))  ))] )
        else (acc@
        [(Sexpr(e), (find_off acc, "MAKE_LITERAL_SYMBOL "^let (a, (b, c)) = (List.find (fun (x, (y, z)) -> x = Sexpr(String(s))) acc) in (string_of_int (b))))]))

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



let wrap_const cnst const = match cnst with
    | Void -> "mov rax, SOB_VOID_ADDRESS"
    | Sexpr(Nil) -> "mov rax, SOB_NIL_ADDRESS"
    | Sexpr(Bool(e)) -> if (e) then ("mov rax, SOB_TRUE_ADDRESS") else ("mov rax, SOB_FALSE_ADDRESS")
    | Sexpr(Char(c)) -> "mov rax, const_tbl+" ^ (string_of_int  (fst (List.assoc (cnst) const))) ^""
    | Sexpr(String(c)) -> "mov rax, const_tbl+" ^ (string_of_int  (fst (List.assoc (cnst) const))) ^""
    | Sexpr(Number(Float(c))) -> "mov rax, const_tbl+" ^ (string_of_int  (fst (List.assoc (cnst) const))) ^""
    | Sexpr(Number(Fraction(num, den))) -> "mov rax, const_tbl+" ^ (string_of_int  (fst (List.assoc (cnst) const))) ^""
    | Sexpr(Pair(car, cdr)) -> "mov rax, const_tbl+" ^ (string_of_int  (fst (List.assoc (cnst) const))) ^""
    | Sexpr(Symbol(s))-> "mov rax, const_tbl+" ^ (string_of_int  (fst (List.assoc (cnst) const))) ^""

  let counter = ref 0;;

let rec g consts fvars e = match e with
    | Const'(c) -> wrap_const c consts
    | If'(tst, thn, els) -> 

    ( let c = !counter in
      let _ = (counter:=!counter+1) in
    
    (Printf.sprintf "%s\n\tcmp byte[rax+1], 1\n\tje true%d\n\t%s\n\tjmp continue%d\n\ttrue%d:\n\t%s\n\tcontinue%d:"
      (g consts fvars tst) c  (g consts fvars els) c c (g consts fvars thn) c))
    | _ -> raise X_not_implemented_codeGen;;



module Code_Gen : CODE_GEN = struct
  let make_consts_tbl asts = (List.fold_left mct [] ([Const'(Void); Const'(Sexpr(Nil));Const'(Sexpr(Bool(false)));
                  Const'(Sexpr(Bool(true)));]@asts));;
  let make_fvars_tbl asts = List.fold_left mft [] asts;;
  

  let generate consts fvars e = g consts fvars e;;
end;;

