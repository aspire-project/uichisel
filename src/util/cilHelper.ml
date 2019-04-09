(***********************************************************************)
(*                                                                     *)
(* Copyright (c) 2007-present.                                         *)
(* Programming Research Laboratory (ROPAS), Seoul National University. *)
(* All rights reserved.                                                *)
(*                                                                     *)
(* This software is distributed under the term of the BSD license.     *)
(* See the LICENSE file for details.                                   *)
(*                                                                     *)
(***********************************************************************)
open Cil
open Vocab
open Cilglobal

let rec collect_lvs_exp exp =
    match exp with
    | Lval lv -> BatSet.singleton lv
    | SizeOfE exp -> collect_lvs_exp exp 
    | AlignOfE exp -> collect_lvs_exp exp
    | UnOp (_, exp, _) -> collect_lvs_exp exp
    | BinOp (_, exp1, exp2, _) -> BatSet.union (collect_lvs_exp exp1) (collect_lvs_exp exp2)
    | CastE (_, exp) -> collect_lvs_exp exp
    | AddrOf lv -> BatSet.singleton lv
    | StartOf lv -> BatSet.singleton lv
    | _ -> BatSet.empty


let rec collect_instrs_of_block block = 
	let stmts = block.bstmts in 
	List.fold_left BatSet.union BatSet.empty (List.map collect_instrs_of_stmt stmts)  

and collect_instrs_of_stmt stmt =
	(match stmt.skind with
	| Instr instrs -> list2set instrs  
	| If (c, b1, b2, _) -> BatSet.union (collect_instrs_of_block b1) (collect_instrs_of_block b2) 
	| Switch (_, b, stmts, _) -> BatSet.union (collect_instrs_of_block b) (List.fold_left BatSet.union BatSet.empty (List.map collect_instrs_of_stmt stmts)) 
	| Loop (b, _, _, _) -> (collect_instrs_of_block b) 
	| Block block -> collect_instrs_of_block block 
	| _ -> BatSet.empty) 

let collect_instrs_of_file file = 
	foldGlobals file (fun set glob ->
		match glob with 
		| GFun (fundec, _) -> (List.fold_left BatSet.union BatSet.empty (List.map collect_instrs_of_stmt fundec.sallstmts))  
		| _ -> set  
		) BatSet.empty  
		
let rec collect_stmts_of_block block = 
	let stmts = block.bstmts in 
	List.fold_left BatSet.union BatSet.empty (List.map collect_stmts_of_stmt stmts)  

and collect_stmts_of_stmt stmt =
	BatSet.add stmt  
	(match stmt.skind with 
	| If (c, b1, b2, _) -> BatSet.union (collect_stmts_of_block b1) (collect_stmts_of_block b2) 
	| Switch (_, b, stmts, _) -> BatSet.union (collect_stmts_of_block b) (List.fold_left BatSet.union BatSet.empty (List.map collect_stmts_of_stmt stmts)) 
	| Loop (b, _, _, _) -> (collect_stmts_of_block b) 
	| Block block -> collect_stmts_of_block block 
	| _ -> BatSet.empty) 

let collect_stmts_of_file file = 
	foldGlobals file (fun set glob ->
		match glob with 
		| GFun (fundec, _) -> (List.fold_left BatSet.union BatSet.empty (List.map collect_stmts_of_stmt fundec.sallstmts))  
		| _ -> set  
		) BatSet.empty  
		
let rec numstmts_of_block block = 
	let stmts = block.bstmts in 
	List.fold_left (+) 0 (List.map numstmts_of_stmt stmts)  
and numstmts_of_stmt stmt = 
	match stmt.skind with 
	| If (c, b1, b2, _) -> 1 + (numstmts_of_block b1) + (numstmts_of_block b2) 
	| Switch (_, b, stmts, _) -> 1 + (numstmts_of_block b) + (List.fold_left (+) 0 (List.map numstmts_of_stmt stmts)) 
	| Loop (b, _, _, _) -> 1 + (numstmts_of_block b) 
	| Block block -> numstmts_of_block block
 	| Instr instrs -> BatSet.cardinal (list2set instrs)
	| Return _ -> 1
	| Goto _ -> 1
	| ComputedGoto _ -> 1
	| Break _ -> 1
	| Continue _ -> 1
	| _ -> 0
	
let numstmts_of_file file = BatSet.cardinal (collect_instrs_of_file file) 
	(* List.fold_left (fun (n_total_stmt,set) g ->                                            *)
	(* 	if (GlobalSet.mem g set) then (n_total_stmt,set)                                     *)
	(* 	else                                                                                 *)
	(* 		let n_total_stmt =                                                                 *)
  (*   		match g with                                                                     *)
  (*   		| Cil.GFun(fd,_) ->                                                              *)
  (*   			let fd_size = List.fold_left (+) 0 (List.map numstmts_of_stmt fd.sallstmts) in *)
  (*   			let n_total_stmt = n_total_stmt + fd_size in                                   *)
  (*   			n_total_stmt                                                                   *)
  (*   		| _ -> n_total_stmt                                                              *)
	(* 		in                                                                                 *)
	(* 		(n_total_stmt, GlobalSet.add g set)                                                *)
	(* ) (0, GlobalSet.empty) file.globals |> fst                                             *)
	  


(* ******************* *
 * to_string functions *
 * ******************* *)

let tostring s = Escape.escape_string (Pretty.sprint 0 s)

let rec s_exps : exp list -> string = fun es ->
  string_of_list ~first:"(" ~last:")" ~sep:", " s_exp es

and s_exp : exp -> string = function
  | Const c -> s_const c
  | Lval l -> s_lv l
  | SizeOf t -> "SizeOf(" ^ s_type t ^ ")"
  | SizeOfE e -> "SizeOfE(" ^ s_exp e ^ ")"
  | SizeOfStr s -> "SizeOfStr(" ^ s ^ ")"
  | AlignOf t -> "AlignOf(" ^ s_type t ^ ")"
  | AlignOfE e -> "AlignOfE(" ^ s_exp e ^ ")"
  | UnOp (u, e, _) -> s_uop u ^ s_exp_paren e
  | BinOp (b, e1, e2, _) -> s_exp_paren e1 ^ s_bop b ^ s_exp_paren e2
  | Question (c, e1, e2, _) ->
    s_exp_paren c ^ " ? " ^ s_exp_paren e1 ^ " : " ^ s_exp_paren e2
  | CastE (t, e) -> "(" ^ s_type t ^ ")" ^ s_exp_paren e
  | AddrOf l -> "&" ^ s_lv l
  | AddrOfLabel _ -> invalid_arg "AddrOfLabel is not supported."
  | StartOf l -> "StartOf(" ^ s_lv l ^ ")"

and s_exp_paren : exp -> string
= fun e ->
  match e with
  | UnOp _ | BinOp _ | Question _ | CastE _ -> "(" ^ s_exp e ^ ")"
  | _ -> s_exp e

and s_const : constant -> string
=fun c -> tostring (d_const () c)

and s_type : typ -> string
=fun typ -> tostring (d_type () typ)

and s_stmt : stmt -> string
= fun s -> tostring (d_stmt () s)

and s_lv : lval -> string = fun (lh, offset) ->
  s_lhost lh ^ s_offset offset

and s_lhost : lhost -> string = function
  | Var vi -> (if vi.vglob then "@" else "") ^ vi.vname
  | Mem e -> "*" ^ s_exp_paren2 e

and s_exp_paren2 : exp -> string
= fun e ->
  match e with
  | Lval (_, NoOffset) -> s_exp e
  | Lval _ | UnOp _ | BinOp _ | Question _ | CastE _ -> "(" ^ s_exp e ^ ")"
  | _ -> s_exp e

and s_offset : offset -> string = function
  | NoOffset -> ""
  | Field (fi, offset) -> "." ^ fi.fname ^ s_offset offset
  | Index (e, offset) -> "[" ^ s_exp e ^ "]" ^ s_offset offset

and s_uop u = tostring (d_unop () u)

and s_bop b = tostring (d_binop () b)

and s_instr : instr -> string
=fun i ->
  match i with
  | Set (lv,exp,_) -> "Set(" ^ s_lv lv ^ "," ^ s_exp exp ^ ")"
  | Call (Some lv,fexp,params,_) ->
      s_lv lv ^ ":= Call(" ^ s_exp fexp ^ s_exps params ^ ")"
  | Call (None,fexp,params,_) ->
      "Call(" ^ s_exp fexp ^ s_exps params ^ ")"
  | Asm _ -> "Asm"

and s_instrs : instr list -> string
=fun instrs ->
  List.fold_left (fun s i -> s ^ s_instr i) "" instrs

let s_location : location -> string
=fun loc ->
  let file = try
    let idx = String.rindex loc.file '/' in
    let len = String.length loc.file in
      String.sub loc.file (idx+1) (len-idx-1)
    with _ -> loc.file
  in file ^ ":" ^ string_of_int loc.line

let eq_lval : lval -> lval -> bool
= fun l1 l2 -> (s_lv l1) = (s_lv l2)

(* ************* *
 * Aux functions *
 * ************* *)

let rev_binop : binop -> binop = fun op ->
  match op with
  | Lt -> Gt
  | Gt -> Lt
  | Le -> Ge
  | Ge -> Le
  | Eq -> Eq
  | Ne -> Ne
  | _ -> invalid_arg "cilHelper.ml: rev_binop"

let not_binop : binop -> binop = fun op ->
  match op with
  | Lt -> Ge
  | Gt -> Le
  | Le -> Gt
  | Ge -> Lt
  | Eq -> Ne
  | Ne -> Eq
  | LAnd -> LOr
  | LOr -> LAnd
  | _ -> invalid_arg "cilHelper.ml: rev_binop"


let rec make_cond_simple : exp -> exp option
= fun cond ->
  match cond with
  | BinOp (op, CastE (_, e1), e2, t)
  | BinOp (op, e1, CastE (_, e2), t) ->
    let newe = BinOp (op, e1, e2, t) in
    make_cond_simple newe
  | BinOp (op, Lval _, _, _)
    when op = Lt || op = Gt || op = Le || op = Ge || op = Eq || op = Ne ->
    Some cond
  | BinOp (op, e, Lval x, t)
    when op = Lt || op = Gt || op = Le || op = Ge || op = Eq || op = Ne ->
    Some (BinOp (rev_binop op, Lval x, e, t))
  | BinOp (op, BinOp (PlusA, Lval x, Lval y, t2), e, t) ->
    make_cond_simple (BinOp (op, Lval x, BinOp (MinusA, e, Lval y, t2), t))
  | BinOp (op, BinOp (MinusA, Lval x, Lval y, t2), e, t) ->
    make_cond_simple (BinOp (op, Lval x, BinOp (PlusA, e, Lval y, t2), t))
  | UnOp (LNot, BinOp (op, e1, e2, t2), _)
    when op = Lt || op = Gt || op = Le || op = Ge || op = Eq || op = Ne ->
    make_cond_simple (BinOp (not_binop op, e1, e2, t2))
  | UnOp (LNot, BinOp (op, e1, e2, t2), t1)
    when op = LAnd || op = LOr ->
    let not_e1 = UnOp (LNot, e1, t1) in
    let not_e2 = UnOp (LNot, e2, t1) in
    (match make_cond_simple not_e1, make_cond_simple not_e2 with
     | Some e1', Some e2' -> Some (BinOp (not_binop op, e1', e2', t2))
     | _, _ -> None)
  | UnOp (LNot, UnOp (LNot, e, _), _) -> make_cond_simple e
  | UnOp (LNot, Lval _, _) -> Some cond
  | Lval _ -> Some cond
  | _ -> None

let rec remove_cast = function
    Cil.CastE (_, e) -> remove_cast e
  | Cil.BinOp (b, e1, e2, t) -> Cil.BinOp(b, remove_cast e1, remove_cast e2, t)
  | Cil.UnOp (u, e, t) -> Cil.UnOp (u, remove_cast e, t)
  | e -> e

let rec remove_coeff = function
    Cil.BinOp (Cil.Mult, Cil.SizeOfE _, e1, _)
  | Cil.BinOp (Cil.Mult, e1, Cil.SizeOfE _, _)
  | Cil.BinOp (Cil.Mult, Cil.SizeOf _, e1, _)
  | Cil.BinOp (Cil.Mult, e1, Cil.SizeOf _, _) -> remove_coeff e1
  | Cil.BinOp (b, e1, e2, t) -> Cil.BinOp(b, remove_coeff e1, remove_coeff e2, t)
  | Cil.UnOp (u, e, t) -> Cil.UnOp (u, remove_coeff e, t)
  | e -> e

let is_unsigned : Cil.typ -> bool = function
  | Cil.TInt (i, _) ->
    i = Cil.IUChar || i = Cil.IUInt || i = Cil.IUShort || i = Cil.IULong || i = Cil.IULongLong
  | _ -> false

(* NOTE : Cil.bitsSizeOf often fails: just return top for the moment
 * Adhoc solution: To avoid this failure, translate original C sources
 * into "CIL" (using -il option) and analyze the CIL program. *)
let byteSizeOf : Cil.typ -> int
=fun typ ->
  try (Cil.bitsSizeOf typ) / 8
  with e ->
    (if !Options.verbose >= 2 then prerr_endline ("warn: Cil.bitsSizeOf (" ^ s_type typ ^ ")"));
    raise e

