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
  
let imp_macro n c= Printf.sprintf "mov rbx, [rsp + 8*3]
cmp rbx, %d
je e%dquals_
cmp rbx, %d
jl l%desser_

R%dgreaters_:
lea rcx, [rbx - %d] ;; difference, length of the list
lea rdx, [rsp + 8*(3+rbx)]
mov rsi, [rdx]
MAKE_PAIR(rax,rsi,SOB_NIL_ADDRESS)
sub rdx, 8
g%dreatloop_:
cmp rcx, 0
je g%dreatend_
mov rsi, [rdx]
push rbx
mov rbx, rax
MAKE_PAIR(rax,rsi,rbx)
pop rbx
sub rdx, 8
dec rcx
jmp g%dreatloop_


g%dreatend_:
mov [rsp + 8*(3+rbx)], rax
lea rax, [rsp+8*(2+rbx)]
lea rcx, [3 + %d]
m%doveloop_:
mov rdx, [rsp + 8*(rcx-1)]
mov [rax], rdx
sub rax, 8
dec rcx
cmp rcx, 0
jne m%doveloop_
lea rcx, [rbx - %d]
lea rbx ,[8*rcx]
add rsp, rbx
sub [rsp + 8* 3], rcx
jmp e%dnd_opt_

l%desser_:
lea rax, [4+rbx]
mov rcx, 0
l%dessloop_:
cmp rax, 0
je w%drapit_
mov rdx, [rsp + (8*rcx)]
mov [rsp + 8*rcx - 8], rdx
dec rax
inc rcx
jmp l%dessloop_
w%drapit_:
sub rsp, 8
mov rax, SOB_NIL_ADDRESS
mov [rsp + 8*(4 + rbx)], rax
add qword [rsp + 8*3], 1
jmp e%dnd_opt_
e%dquals_:
mov rcx, [rsp + 8*(3+rbx)]
MAKE_PAIR(rax, rcx, SOB_NIL_ADDRESS)
mov [rsp + 8*(3+rbx)], rax
e%dnd_opt_:" n c n c c n c c c c n c c n c c c c c c c c c 
  
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
    | Void -> "\n\tmov rax, SOB_VOID_ADDRESS"
    | Sexpr(Nil) -> "\n\tmov rax, SOB_NIL_ADDRESS"
    | Sexpr(Bool(e)) -> if (e) then ("\n\tmov rax, SOB_TRUE_ADDRESS") else ("\n\tmov rax, SOB_FALSE_ADDRESS")
    | Sexpr(Char(c)) -> "\n\tmov rax, const_tbl+" ^ (string_of_int  (fst (List.assoc (cnst) const))) ^""
    | Sexpr(String(c)) -> "\n\tmov rax, const_tbl+" ^ (string_of_int  (fst (List.assoc (cnst) const))) ^""
    | Sexpr(Number(Float(c))) -> "\n\tmov rax, const_tbl+" ^ (string_of_int  (fst (List.assoc (cnst) const))) ^""
    | Sexpr(Number(Fraction(num, den))) -> "\n\tmov rax, const_tbl+" ^ (string_of_int  (fst (List.assoc (cnst) const))) ^""
    | Sexpr(Pair(car, cdr)) -> "\n\tmov rax, const_tbl+" ^ (string_of_int  (fst (List.assoc (cnst) const))) ^""
    | Sexpr(Symbol(s))-> "\n\tmov rax, const_tbl+" ^ (string_of_int  (fst (List.assoc (cnst) const))) ^"";;

(* todo: apply proc: maybe macro expand it *)
(* todo: expand +, *, -, / *)

let counter = ref 0;;


let rec gener consts fvars env e = 
      Printf.sprintf "%s" (match e with
    | Const'(c) -> wrap_const c consts
    | If'(tst, thn, els) -> 
      ( let c = !counter in let _ = (counter:=!counter+1) in
      (Printf.sprintf "\n\t%s\n\tcmp byte[rax+1], 1\n\tje true%d\n\t%s\n\tjmp continue%d\n\ttrue%d:\n\t%s\n\tcontinue%d:"
        (gener consts fvars env tst) c  (gener consts fvars env els) c c (gener consts fvars env thn) c))
    | Var'(VarParam(s, minor)) -> (Printf.sprintf ";varparam %s\n\tmov rax, qword[rbp + 8 * (4 + %d)]" s minor) 
    | Set'(VarParam(s, minor), ex) -> (Printf.sprintf ";set varparam %s\n\t%s\n\tmov qword[rbp + 8  * ( 4 + %d)], rax\n\tmov rax, SOB_VOID_ADDRESS" s (gener consts fvars env ex) minor)
    | Var'(VarBound(s,major, minor)) -> (Printf.sprintf ";varbound %s\n\tmov rax, qword[rbp + 8 * 2]\n\tmov rax, qword[rax + 8 * %d]\n\tmov rax, qword[rax + 8 * %d]" s major minor) 
    | Set'(VarBound(s, major, minor), e) -> (let e = gener consts fvars env e in (Printf.sprintf ";set varbound %s\n\t%s \n\tmov rbx, qword[rbp +8 * 2]\n\tmov rbx, [rbx + 8*%d]\n\tmov qword[rbx + 8*%d], rax\n\t mov rax, SOB_VOID_ADDRESS" s e major minor))
    | Var'(VarFree(v)) -> Printf.sprintf ";varfree %s\n\tmov rax, qword[fvar_tbl + 8 * %d]" v (List.assoc v fvars)
    | Set'(VarFree(v), e) -> (let e = gener consts fvars env e in Printf.sprintf ";set varfree\n\t%s\n\t mov qword[fvar_tbl + 8 * %d], rax\n\tmov rax, SOB_VOID_ADDRESS" e (List.assoc v fvars))
    (* todo: test it*)
    | Seq'(seq) -> ";seq\n\t" ^(List.fold_left (fun acc x->acc^(Printf.sprintf "%s\n\t" (gener consts fvars env x))) "" seq)
    (* todo: test it*)
    | Or'(seq) -> let c = !counter in let _ = (counter:=!counter+1) in (generate_or consts fvars seq env c)
    | BoxGet'(v) -> (Printf.sprintf ";boxget\n\t%s\n\tmov rax, qword[rax]" (gener consts fvars env (Var'(v))))
    | BoxSet'(v, e) -> (Printf.sprintf ";boxset\n\t%s\n\tpush rax\n\t%s\n\t;set the box\n\tpop qword[rax]\n\tmov rax, SOB_VOID_ADDRESS" (gener consts fvars env e) (gener consts fvars env (Var'(v))))
    (* todo: test it*)
    | Applic'(proc, vars) -> let proc = (gener consts fvars env proc) in let n = 
        (List.length vars) in 
        (Printf.sprintf
        ";applic\n\t\t%s%s\n\tpush %d\n\tCLOSURE_ENV rsi, rax\n\tpush rsi\n\tCLOSURE_CODE rdx, rax\n\tcall rdx\n\tadd rsp, 8*1\n\tpop rbx\n\tshl rbx, 3\n\tadd rsp, rbx"
        (String.concat "" (List.map (fun v -> (Printf.sprintf "%s\n\tpush rax\n\t" (gener consts fvars env v))) (List.rev vars))) proc n)

    | Def'(VarFree(v), e) -> (Printf.sprintf ";definee\n\n\t%s\n\t;move val to var in definee\n\tmov qword[fvar_tbl + 8 * %d], rax\n\tmov rax, SOB_VOID_ADDRESS" (gener consts fvars env e)) (List.assoc v fvars)
    | LambdaSimple'(params, body) -> let c = !counter in let _ = (counter:=!counter+1) in (Printf.sprintf ";lambda simple\n\t%s\n%s" (lambdaenv c (env + 1)) (lambdaBody consts fvars body c (env + 1)))
    | LambdaOpt'(slst ,s, body) -> let c = !counter in let _ = (counter:=!counter+1) in (Printf.sprintf ";lambda opt\n%s\n\n\n\n\n\n%s" (lambdaenv c (env + 1)) (lambdaBodyopt  consts fvars body c (env + 1) (1+ (List.length slst))))
    (* | ApplicTP'(proc, vars) ->  *)
    | Box'(v) -> Printf.sprintf ";box\n\t%s\n\tMAKE_BOX rbx\n\tmov [rbx], rax\n\tmov rax, rbx" (gener consts fvars env (Var'(v)))
    | _ -> raise X_not_implemented_codeGen)

and generate_or consts fvars seq env exit_label = Printf.sprintf "%scontinue%d:" (List.fold_left (fun acc x -> acc^(Printf.sprintf "%s\n\t cmp rax, SOB_FALSE_ADDRESS\n\t jne continue%d\n\t" (gener consts fvars env x) exit_label)) "" seq) exit_label

and lambdaenv c env = let env1 = Printf.sprintf "\n\t;lambda env\n\n\tMALLOC rax, WORD_SIZE*%d\n\tmov rbx, qword[rbp + 8 * 2]\n\tmov rcx, %d\nloop%d:\n\tcmp rcx, 0\n\tje endd%d\n\tmov rdx, qword[rbx + 8*(rcx-1)]\n\tmov [rax + 8*rcx], rdx\n\tdec rcx\n\tjmp loop%d\nendd%d:" env (env-1) c c c c in
                            let env2 =  Printf.sprintf "mov rcx, [rbp + 8 * 3]\n\tpush rcx\n\tlea rcx, [rcx*WORD_SIZE]\n\tMALLOC rbx, rcx\n\tpop rcx\n\tmov [rax], rbx\nparamloop%d:\n\tcmp rcx, 0\n\tje end%d\n\tmov rdx, [rbp + 8*(3+rcx)]\n\tmov qword[rbx + 8*(rcx-1)], rdx\n\tdec rcx\n\tjmp paramloop%d\nend%d:" c c c c in
                            Printf.sprintf "%s\n\t%s\n\tmov rbx, rax\n\tMAKE_CLOSURE(rax, rbx, Lbody%d)\n\tjmp Lcont%d" env1 env2 c c

and lambdaBody consts fvars body count env = Printf.sprintf "; Lambda body\nLbody%d:\n\tpush rbp\n\tmov rbp, rsp\n%s\n\tpop rbp\n\tret\nLcont%d:" count (gener consts fvars env body) count

and lambdaBodyopt consts fvars body count env n = let opt_macro = imp_macro n count in Printf.sprintf ";lambda body\nLbody%d:\n\tpush rbp\n\t%s\n\tmov rbp, rsp\n%s\n\tpop rbp\n\tret\nLcont%d:" count opt_macro (gener consts fvars env body) count 



module Code_Gen : CODE_GEN = struct
  let make_consts_tbl asts = (List.fold_left mct [] ([Const'(Void); Const'(Sexpr(Nil));Const'(Sexpr(Bool(false)));
                  Const'(Sexpr(Bool(true)));]@asts));;
  let make_fvars_tbl asts = List.fold_left mft [] asts;;
  
  let generate consts fvars e = gener consts fvars 0 e;;
end;;

