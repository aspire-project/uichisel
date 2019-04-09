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
val collect_lvs_exp: Cil.exp -> Cil.lval BatSet.t
val collect_instrs_of_block: Cil.block -> Cil.instr BatSet.t
val collect_instrs_of_stmt: Cil.stmt -> Cil.instr BatSet.t
val collect_instrs_of_file: Cil.file -> Cil.instr BatSet.t
val collect_stmts_of_block: Cil.block -> Cil.stmt BatSet.t
val collect_stmts_of_stmt: Cil.stmt -> Cil.stmt BatSet.t
val collect_stmts_of_file: Cil.file -> Cil.stmt BatSet.t
val numstmts_of_block: Cil.block -> int 
val numstmts_of_stmt: Cil.stmt -> int
val numstmts_of_file: Cil.file -> int 
val s_exps : Cil.exp list -> string
val s_exp : Cil.exp -> string
val s_exp_paren : Cil.exp -> string
val s_const : Cil.constant -> string
val s_type : Cil.typ -> string
val s_stmt : Cil.stmt -> string
val s_lv : Cil.lval -> string
val s_lhost : Cil.lhost -> string
val s_exp_paren2 : Cil.exp -> string
val s_offset : Cil.offset -> string
val s_uop : Cil.unop -> string
val s_bop : Cil.binop -> string
val s_instr : Cil.instr -> string
val s_instrs : Cil.instr list -> string
val s_location : Cil.location -> string
val eq_lval : Cil.lval -> Cil.lval -> bool

val rev_binop : Cil.binop -> Cil.binop
val not_binop : Cil.binop -> Cil.binop
val make_cond_simple : Cil.exp -> Cil.exp option
val remove_cast : Cil.exp -> Cil.exp
val remove_coeff : Cil.exp -> Cil.exp
val is_unsigned : Cil.typ -> bool
val byteSizeOf : Cil.typ -> int
