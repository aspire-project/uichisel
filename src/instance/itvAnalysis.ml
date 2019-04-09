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
open Graph
open Cil
open BasicDom
open Vocab
open Frontend
open IntraCfg
open ItvDom
open ArrayBlk
open AlarmExp
open Report
open Pretty
open Global

module Analysis = SparseAnalysis.Make(ItvSem)
module Table = Analysis.Table
module Spec = Analysis.Spec

let print_abslocs_info locs =
  let lvars = BatSet.filter Loc.is_lvar locs in
  let gvars = BatSet.filter Loc.is_gvar locs in
  let allocsites = BatSet.filter Loc.is_allocsite locs in
  let fields = BatSet.filter Loc.is_field locs in
    prerr_endline ("#abslocs    : " ^ i2s (BatSet.cardinal locs));
    prerr_endline ("#lvars      : " ^ i2s (BatSet.cardinal lvars));
    prerr_endline ("#gvars      : " ^ i2s (BatSet.cardinal gvars));
    prerr_endline ("#allocsites : " ^ i2s (BatSet.cardinal allocsites));
    prerr_endline ("#fields     : " ^ i2s (BatSet.cardinal fields))

(* **************** *
 * Alarm Inspection *
 * **************** *)
let ignore_alarm a arr offset =
  (!Options.bugfinder >= 1
    && (Allocsite.is_string_allocsite a
       || arr.ArrInfo.size = Itv.top
       || arr.ArrInfo.size = Itv.one
       || offset = Itv.top && arr.ArrInfo.size = Itv.nat
       || offset = Itv.zero))
  || (!Options.bugfinder >= 2
      && not (Itv.is_const arr.ArrInfo.size))
  || (!Options.bugfinder >= 3
       && (offset = Itv.top
          || Itv.meet arr.ArrInfo.size Itv.zero <> Itv.bot
          || (offset = Itv.top && arr.ArrInfo.offset <> Itv.top)))


let check_bo v1 v2opt : (status * Allocsite.t option * string) list =
  let arr = Val.array_of_val v1 in
  if ArrayBlk.eq arr ArrayBlk.bot then [(BotAlarm, None, "Array is Bot")] else
    ArrayBlk.foldi (fun a arr lst ->
      let offset =
        match v2opt with
        | None -> arr.ArrInfo.offset
        | Some v2 -> Itv.plus arr.ArrInfo.offset (Val.itv_of_val v2) in
      let status =
        try
          if Itv.is_bot offset || Itv.is_bot arr.ArrInfo.size then BotAlarm
          else if ignore_alarm a arr offset then Proven
          else
            let (ol, ou) = (Itv.lower offset, Itv.upper offset) in
            let sl = Itv.lower arr.ArrInfo.size in
            if ou >= sl || ol < 0 then UnProven
            else Proven
        with _ -> UnProven
      in
      (status, Some a, string_of_alarminfo offset arr.ArrInfo.size)::lst
    ) arr []

let check_nd v1 : (status * Allocsite.t option * string) list =
  let ploc = Val.pow_loc_of_val v1 in
  if PowLoc.eq ploc PowLoc.bot then [(BotAlarm, None, "PowLoc is Bot")] else
    if PowLoc.mem Loc.null ploc then
      [(UnProven, None, "Null Dereference")]
    else [(Proven, None, "")]

let inspect_aexp_bo : InterCfg.node -> AlarmExp.t -> Mem.t -> query list -> query list
=fun node aexp mem queries ->
  (match aexp with
    | ArrayExp (lv,e,loc) ->
        let v1 = Mem.lookup (ItvSem.eval_lv (InterCfg.Node.get_pid node) lv mem) mem in
        let v2 = ItvSem.eval (InterCfg.Node.get_pid node) e mem in
        let lst = check_bo v1 (Some v2) in
        List.map (fun (status,a,desc) ->
          { node = node; exp = aexp; loc = loc; allocsite = a;
          status = status; desc = desc }) lst
    | DerefExp (e,loc) ->
        let v = ItvSem.eval (InterCfg.Node.get_pid node) e mem in
        let lst = check_bo v None in
          if Val.eq Val.bot v then
            List.map (fun (status,a,desc) ->
              { node = node; exp = aexp; loc = loc; allocsite = a;
              status = status; desc = desc }) lst
          else
            List.map (fun (status,a,desc) ->
              if status = BotAlarm
              then { node = node; exp = aexp; loc = loc; status = Proven; allocsite = a;
                     desc = "valid pointer dereference" }
              else { node = node; exp = aexp; loc = loc; status = status; allocsite = a;
                     desc = desc }) lst
    | Strcpy (e1, e2, loc) ->
        let v1 = ItvSem.eval (InterCfg.Node.get_pid node) e1 mem in
        let v2 = ItvSem.eval (InterCfg.Node.get_pid node) e2 mem in
        let v2 = Val.of_itv (ArrayBlk.nullof (Val.array_of_val v2)) in
        let lst = check_bo v1 (Some v2) in
        List.map (fun (status,a,desc) -> { node = node; exp = aexp; loc = loc; allocsite = a;
            status = status; desc = desc }) lst
    | Strcat (e1, e2, loc) ->
        let v1 = ItvSem.eval (InterCfg.Node.get_pid node) e1 mem in
        let v2 = ItvSem.eval (InterCfg.Node.get_pid node) e2 mem in
        let np1 = ArrayBlk.nullof (Val.array_of_val v1) in
        let np2 = ArrayBlk.nullof (Val.array_of_val v2) in
        let np = Val.of_itv (Itv.plus np1 np2) in
        let lst = check_bo v1 (Some np) in
        List.map (fun (status,a,desc) -> { node = node; exp = aexp; loc = loc; allocsite = a;
            status = status; desc = desc }) lst
    | Strncpy (e1, e2, e3, loc)
    | Memcpy (e1, e2, e3, loc)
    | Memmove (e1, e2, e3, loc) ->
        let v1 = ItvSem.eval (InterCfg.Node.get_pid node) e1 mem in
        let v2 = ItvSem.eval (InterCfg.Node.get_pid node) e2 mem in
        let e3_1 = Cil.BinOp (Cil.MinusA, e3, Cil.mone, Cil.intType) in
        let v3 = ItvSem.eval (InterCfg.Node.get_pid node) e3_1 mem in
        let lst1 = check_bo v1 (Some v3) in
        let lst2 = check_bo v2 (Some v3) in
        List.map (fun (status,a,desc) -> { node = node; exp = aexp; loc = loc; allocsite = a;
            status = status; desc = desc }) (lst1@lst2)
    | _ -> []) @ queries

let inspect_aexp_nd : InterCfg.node -> AlarmExp.t -> Mem.t -> query list -> query list
=fun node aexp mem queries ->
  (match aexp with
  | DerefExp (e,loc) ->
    let v = ItvSem.eval (InterCfg.Node.get_pid node) e mem in
    let lst = check_nd v in
      if Val.eq Val.bot v then
        List.map (fun (status,a,desc) -> { node = node; exp = aexp; loc = loc; allocsite = a;
          status = status; desc = desc }) lst
      else
        List.map (fun (status,a,desc) ->
          if status = BotAlarm
          then { node = node; exp = aexp; loc = loc; status = Proven; allocsite = a;
            desc = "valid pointer dereference" }
          else { node = node; exp = aexp; loc = loc; status = status; allocsite = a;
            desc = desc }) lst
  | _ -> []) @ queries

let check_dz v =
  let v = Val.itv_of_val v in
  if Itv.le Itv.zero v then
    [(UnProven, None, "Divide by "^Itv.to_string v)]
  else [(Proven, None, "")]

let inspect_aexp_dz : InterCfg.node -> AlarmExp.t -> Mem.t -> query list -> query list
= fun node aexp mem queries ->
  (match aexp with
      DivExp (_, e, loc) ->
      let v = ItvSem.eval (InterCfg.Node.get_pid node) e mem in
      let lst = check_dz v in
        List.map (fun (status,a,desc) -> { node = node; exp = aexp; loc = loc; allocsite = None;
          status = status; desc = desc }) lst
  | _ -> []) @ queries

let machine_gen_code : query -> bool
= fun q ->
  (* yacc-generated code *)
  Filename.check_suffix q.loc.Cil.file ".y" || Filename.check_suffix q.loc.Cil.file ".yy.c" ||
  Filename.check_suffix q.loc.Cil.file ".simple" ||
  (* sparrow-generated code *)
  InterCfg.Node.get_pid q.node = InterCfg.global_proc

let rec unsound_exp : Cil.exp -> bool
= fun e ->
  match e with
  | Cil.BinOp (Cil.PlusPI, Cil.Lval (Cil.Mem _, _), _, _) -> true
  | Cil.BinOp (b, _, _, _) when b = Mod || b = Cil.Shiftlt || b = Shiftrt || b = BAnd
      || b = BOr || b = BXor || b = LAnd || b = LOr -> true
  | Cil.BinOp (bop, Cil.Lval (Cil.Var _, _), Cil.Lval (Cil.Var _, _), _)
    when bop = Cil.PlusA || bop = Cil.MinusA -> true
  | Cil.BinOp (_, e1, e2, _) -> (unsound_exp e1) || (unsound_exp e2)
  | Cil.CastE (_, e) -> unsound_exp e
  | Cil.Lval lv -> unsound_lv lv
  | _ -> false

and unsound_lv : Cil.lval -> bool = function
  | (_, Cil.Index _) -> true
  | (Cil.Var v, _) -> is_global_integer v || is_union v.vtype || is_temp_integer v
  | (Cil.Mem _, Cil.NoOffset) -> true
  | (_, _) -> false
and is_global_integer v = v.vglob && Cil.isIntegralType v.vtype
and is_union typ =
  match Cil.unrollTypeDeep typ with
    Cil.TPtr (Cil.TComp (c, _), _) -> not c.cstruct
  | _ -> false
and is_temp_integer v =
  !Options.bugfinder >= 2
  && (try String.sub v.vname 0 3 = "tmp" with _ -> false)
  && Cil.isIntegralType v.vtype

let unsound_aexp : AlarmExp.t -> bool = function
  | ArrayExp (lv, e, _) -> unsound_exp e
  | DerefExp (e, _) -> unsound_exp e
  | _ -> false

let formal_param : Global.t -> query -> bool
= fun global q ->
  let cfg = InterCfg.cfgof global.icfg (InterCfg.Node.get_pid q.node) in
  let formals = IntraCfg.get_formals cfg |> List.map (fun x -> x.Cil.vname) in
  let rec find_exp = function
    | Cil.BinOp (_, e1, e2, _) -> (find_exp e1) || (find_exp e2)
    | Cil.CastE (_, e) -> find_exp e
    | Cil.Lval lv -> find_lv lv
    | _ -> false
  and find_lv = function
    | (Cil.Var v, _) -> (List.mem v.vname formals) && Cil.isIntegralType v.vtype
    | (_, _) -> false
  in
  match q.exp with
  | ArrayExp (_, e, _) | DerefExp (e, _) -> find_exp e
  | _ -> false

let unsound_filter : Global.t -> query list -> query list
= fun global ql ->
  let filtered =
    List.filter (fun q ->
      not (machine_gen_code q)
      && not (unsound_aexp q.exp)
(*     not (formal_param global q)*)) ql
  in
  let partition =
    list_fold (fun q m ->
      let p_als = try BatMap.find (q.loc,q.node) m with _ -> [] in
        BatMap.add (q.loc,q.node) (q::p_als) m
    ) filtered BatMap.empty
  in
  BatMap.fold (fun ql result ->
      if List.length (Report.get ql UnProven) > 3 then
        (List.map (fun q -> { q with status = Proven}) ql)@result
      else ql@result) partition []

let filter : query list -> status -> query list
= fun qs s -> List.filter (fun q -> q.status = s) qs

let generate : Global.t * Table.t * target -> query list
=fun (global,inputof,target) ->
  let nodes = InterCfg.nodesof global.icfg in
  let total = List.length nodes in
  list_fold (fun node (qs,k) ->
    prerr_progressbar ~itv:1000 k total;
    let mem = Table.find node inputof in
    let cmd = InterCfg.cmdof global.icfg node in
    let aexps = AlarmExp.collect cmd in
    let qs = list_fold (fun aexp ->
      if mem = Mem.bot then id (* dead code *)
      else
        match target with
          BO -> inspect_aexp_bo node aexp mem
        | ND -> inspect_aexp_nd node aexp mem
        | DZ -> inspect_aexp_dz node aexp mem
      ) aexps qs
    in
    (qs, k+1)
  ) nodes ([],0)
  |> fst
  |> opt (!Options.bugfinder > 0) (unsound_filter global)

let generate_with_mem : Global.t * Mem.t * target -> query list
=fun (global,mem,target) ->
  let nodes = InterCfg.nodesof global.icfg in
    list_fold (fun node ->
      let cmd = InterCfg.cmdof global.icfg node in
      let aexps = AlarmExp.collect cmd in
        if mem = Mem.bot then id (* dead code *)
        else
          match target with
            BO -> list_fold (fun aexp  -> inspect_aexp_bo node aexp mem) aexps
          | ND -> list_fold (fun aexp  -> inspect_aexp_nd node aexp mem) aexps
          | DZ -> list_fold (fun aexp  -> inspect_aexp_dz node aexp mem) aexps
    ) nodes []

(* ********** *
 * Marshaling *
 * ********** *)

let marshal_in : Global.t -> Global.t * Table.t * Table.t
= fun global ->
  let filename = Filename.basename global.file.fileName in
  let global = MarshalManager.input (filename ^ ".itv.global") in
  let input = MarshalManager.input (filename ^ ".itv.input") in
  let output = MarshalManager.input (filename ^ ".itv.output") in
  (global,input,output)

let marshal_out : Global.t * Table.t * Table.t -> Global.t * Table.t * Table.t
= fun (global,input,output) ->
  let filename = Filename.basename global.file.fileName in
  MarshalManager.output (filename ^ ".itv.global") global;
  MarshalManager.output (filename ^ ".itv.input") input;
  MarshalManager.output (filename ^ ".itv.output") output;
  (global,input,output)


let save_source source_name global orig_icfg =
	let cil_globals = global.file.globals in     
	global.file.globals <- global.icfg.InterCfg.globals;  
	let exclude_locs = 
		List.fold_left (fun exclude_locs node ->
			let loc_orig = IntraCfg.Cmd.location_of (InterCfg.cmdof orig_icfg node) in
			let loc = try IntraCfg.Cmd.location_of (InterCfg.cmdof global.icfg node) with _ -> Cil.locUnknown in
			if (Cil.compareLoc loc_orig loc) != 0 then BatSet.add loc_orig exclude_locs else exclude_locs
		) BatSet.empty (InterCfg.nodesof orig_icfg) 
	in  
	Utils.save_with_excludes global.file source_name exclude_locs;
	global.file.globals <- cil_globals


(* -1: b1 goto b2 *)
(* 1: b2 goto b1 *)
(* 0: b1 independent b2 *)
let rec dependent b1 b2 =
	let stmts1 = (CilHelper.collect_stmts_of_block b1) in 
	let stmts2 = (CilHelper.collect_stmts_of_block b2) in
	let gotos1 = BatSet.fold (fun s set -> match s.skind with Goto (stmtref, _) -> BatSet.add !stmtref set | _ -> set) stmts1 BatSet.empty in
  let gotos2 = BatSet.fold (fun s set -> match s.skind with Goto (stmtref, _) -> BatSet.add !stmtref set | _ -> set) stmts2 BatSet.empty in
	if not (BatSet.is_empty (BatSet.intersect gotos1 stmts2)) then -1
	else if not (BatSet.is_empty (BatSet.intersect gotos2 stmts1)) then 1
	else 0   

(* input false_conds: conditions confirmed false (Cil.location * bool) *)
(* input to_remove_ref: ref to stmts to be removed *)
class blockVisitor false_conds to_remove_ref = object(self)
  inherit nopCilVisitor
	method vstmt (s: Cil.stmt) =
		(match s.skind with 
		| If (c, b1, b2, loc) ->   
			let dep = dependent b1 b2 in
			(* let _ = print_endline (Printf.sprintf "**** ========= %s at %s, %b: %b ======== ****" (CilHelper.s_stmt s) (CilHelper.s_location loc) false (BatSet.mem (loc, false) to_remove)) in *)
			(* let _ = print_endline (Printf.sprintf "**** ========= %s at %s, %b: %b ======== ****" (CilHelper.s_stmt s) (CilHelper.s_location loc) true (BatSet.mem (loc, true) to_remove)) in   *)
			if dep != -1 && (BatSet.mem (loc, false) false_conds) then
				to_remove_ref := BatSet.union !to_remove_ref (CilHelper.collect_stmts_of_block b2)
				(* s.skind <- (Block b1)  *)
			else if dep != 1 && (BatSet.mem (loc, true) false_conds) then
				(* s.skind <- (Block b2) *)
				to_remove_ref := BatSet.union !to_remove_ref (CilHelper.collect_stmts_of_block b1)
			else () 
		| _ -> ()); DoChildren 
		
  (* method vblock (block: Cil.block) =                                                                                                                                                            *)
	(* 	match !currentGlobal with                                                                                                                                                                   *)
	(* 	| GFun (fundec, _) ->                                                                                                                                                                       *)
	(* 		let bstmts = block.bstmts in                                                                                                                                                              *)
	(* 			List.iter (fun s ->                                                                                                                                                                     *)
	(* 				match s.skind with                                                                                                                                                                    *)
	(* 				| If (c, b1, b2, loc) ->                                                                                                                                                              *)
	(* 					let dep = dependent b1 b2 in                                                                                                                                                        *)
	(* 					let _ = print_endline (Printf.sprintf "**** ========= %s at %s, %b: %b ======== ****" (CilHelper.s_stmt s) (CilHelper.s_location loc) false (BatSet.mem (loc, false) to_remove)) in *)
	(* 					let _ = print_endline (Printf.sprintf "**** ========= %s at %s, %b: %b ======== ****" (CilHelper.s_stmt s) (CilHelper.s_location loc) true (BatSet.mem (loc, true) to_remove)) in   *)
	(* 					if (*dep != -1 &&*) (BatSet.mem (loc, false) to_remove) then                                                                                                                        *)
	(* 						s.skind <- (Block b1)                                                                                                                                                             *)
	(* 					else if (*dep != 1 &&*) (BatSet.mem (loc, true) to_remove) then                                                                                                                     *)
	(* 						s.skind <- (Block b2)                                                                                                                                                             *)
	(* 					else ()                                                                                                                                                                             *)
	(* 				| _ -> ()                                                                                                                                                                             *)
	(* 			) bstmts;                                                                                                                                                                               *)
  (*   	Cil.ChangeTo { block with bstmts }                                                                                                                                                        *)
	(* 	| _ -> Cil.DoChildren                                                                                                                                                                       *)
end


let inspect_alarm : Global.t -> Spec.t -> Table.t -> Table.t -> Report.query list
= fun global _ inputof outputof ->
  (if !Options.bo then generate (global,inputof,Report.BO) else [])
  @ (if !Options.nd then generate (global,inputof,Report.ND) else [])
  @ (if !Options.dz then  generate (global,inputof,Report.DZ) else [])

let get_locset mem =
  ItvDom.Mem.foldi (fun l v locset ->
    locset
    |> PowLoc.add l
    |> PowLoc.union (Val.pow_loc_of_val v)
    |> BatSet.fold (fun a -> PowLoc.add (Loc.of_allocsite a)) (Val.allocsites_of_val v)
  ) mem PowLoc.empty

(** *********************************************************************************** *)
(** ************************ User-interactive Chisel ********************************** *)
(** *********************************************************************************** *)

let get_refutable_branches global branch2vals already_covered =
	(** all branch conditions *)
	let target_branches =
		let nodes = InterCfg.nodesof global.icfg in
		List.fold_left (fun target_branches node ->
			if (String.compare (InterCfg.Node.get_pid node) InterCfg.global_proc) = 0 then target_branches
			else
			let cmd = try InterCfg.cmdof global.icfg node with _ -> assert false in
			match cmd with 
			| IntraCfg.Cmd.Cassume (e, loc, b) ->
				if (BatSet.mem node already_covered) then target_branches   
				else BatSet.add node target_branches 
			| _ -> target_branches 
		) BatSet.empty nodes 
	in
	(** Remaining only constant branch conditions + ones not exercised during oracle runs *)
	let target_branches =
		BatSet.filter (fun node ->
			let cmd = try InterCfg.cmdof global.icfg node with _ -> assert false in
			match cmd with 
			| IntraCfg.Cmd.Cassume (e, loc, b) ->
				if not (BatMap.mem loc branch2vals) then (* not exercised during oracle runs *)
					true
				else
					let values = try BatMap.find loc branch2vals with _ -> assert false in 
					if ((BatSet.cardinal values) = 1) then    
						let value = BatSet.choose (values) in
						(value = 0 && b) || (value != 0 && (not b))
					else false
			| _ -> assert false  
		) target_branches 
	in
	(** Remaining only non-dominating branch conditions *)
	let target_branches =
		BatSet.filter (fun node ->
			let pid, n = (InterCfg.Node.get_pid node, InterCfg.Node.get_cfgnode node) in
			let dominated_by_some =    
  			BatSet.exists (fun node' -> 
  				let pid', n' = (InterCfg.Node.get_pid node', InterCfg.Node.get_cfgnode node')  in
  				(String.compare pid pid') = 0 && (IntraCfg.is_dom (InterCfg.cfgof global.icfg pid) n' n) 
  			) (BatSet.remove node target_branches) 
			in 
			not dominated_by_some
		) target_branches
	in 	
	target_branches
	
	
(** sync(global) = global *)
let sync orig_icfg source_name (global, spec, inputof, outputof) =
	(* only remain icfg *)
	let global = {global with callgraph = CallGraph.empty; dump = Dump.empty; mem = ItvDom.Mem.bot; table = ItvDom.Table.bot} in
	let global = {global with icfg = InterCfg.compute_dom_and_scc global.icfg} in 
	let global = PreAnalysis.perform global in  (* including removing unreachable function *)
	let locset = get_locset global.mem in
	let locset_fs = PartialFlowSensitivity.select global locset in
	let unsound_lib = UnsoundLib.collect global in
  let unsound_update = (!Options.bugfinder >= 2) in
  let unsound_bitwise = (!Options.bugfinder >= 1) in
	let spec = {spec with Spec.locset = locset; Spec.locset_fs = locset_fs; Spec.premem = global.mem; Spec.unsound_lib;
    Spec.unsound_update; Spec.unsound_bitwise; } in
	let (global, inputof, outputof) =
		try  
			Analysis.perform spec global
		with _ -> let _ = save_source (Printf.sprintf "%s/err.c" !Options.marshal_dir) global orig_icfg in failwith "rerun" 
	in
	(global, spec, inputof, outputof)
	
	
(* PE(!p, P) = *)
(*   0. record |P| *)
(*   1. for all assume(p), n s.t. T(assume(p)) = \bot and assume(p) dom n, cmdof[n |-> skip]  (T = inputof, forall n. n dom n) *)
(*   2. T = A(P)  (A = analysis) *)
(*   3. if |P| changed, goto 0 *)
(* output: icfg *)	
let iter = ref 0 
let rec partial_evaluation orig_icfg source_name (global,spec,inputof,outputof) =
	(** reset all the info *)
	incr iter ;
	(* should not sync here: inputof can become intentionally out of sync for computing gains *)
	let prev_size = InterCfg.num_of_nonskip_nodes global.icfg in
	(** do partial evaluation one step *)
	let false_conds (*: (Cil.location * bool) BatSet.t*) =
		let nodes = InterCfg.nodesof global.icfg in
  	List.fold_left (fun false_conds node ->
			(* we should use input memory (not output memory) because output memory may not contain necessary values *)
			(* if they are not used in later parts *)
    	let cmd = try InterCfg.cmdof global.icfg node with _ -> assert false in
			match cmd with 
			| IntraCfg.Cmd.Cassume (cond_exp, loc, b) ->
				let input_mem = Table.find node inputof in
				(* to make it a non-bottom memory *)
				let (mem, _) = Analysis.run AbsSem.Strong spec node (input_mem, global) in
				if mem = Mem.bot && (not (BatSet.is_empty (CilHelper.collect_lvs_exp cond_exp))) then
					BatSet.add node false_conds
				else false_conds  
			| _ -> false_conds
		) BatSet.empty nodes
	in
	(** 1. remove dead branches **)
	let new_icfg =
		let rec fix f s =
			let s' = IntraCfg.NodeSet.fold (fun e set -> IntraCfg.NodeSet.union (f e) set) s s in 
			if IntraCfg.NodeSet.subset s' s then s else fix f s' 	 
		in
		BatSet.fold (fun node new_icfg ->
			let pid, cfgnode = (InterCfg.Node.get_pid node, InterCfg.Node.get_cfgnode node) in 
			let cfg = InterCfg.cfgof new_icfg pid in  
			let to_remove = 
				fix (fun n -> IntraCfg.children_of_dom_tree n cfg) (IntraCfg.NodeSet.singleton cfgnode)
				|> IntraCfg.NodeSet.remove cfgnode 
			in 
			IntraCfg.NodeSet.fold (fun n new_icfg ->
				(* InterCfg.remove_node (InterCfg.Node.make pid n) new_icfg *)
				InterCfg.add_cmd new_icfg (InterCfg.Node.make pid n) IntraCfg.Cmd.Cskip
			) to_remove new_icfg
		) false_conds global.icfg 
	in   
	let global = {global with icfg = new_icfg } in 
	let (global, spec, inputof, outputof) = sync orig_icfg source_name (global, spec, inputof, outputof) in
	let curr_size = InterCfg.num_of_nonskip_nodes global.icfg  in
	(*let _ = prerr_endline (Printf.sprintf "=== After PE: %d -> %d (#.stmts) ===" prev_size curr_size) in*)
	if curr_size < prev_size then
		partial_evaluation orig_icfg source_name (global,spec,inputof,outputof)
	else
		(global,spec,inputof,outputof)
	 	
		
(** compute gain for each branch condition *)
let compute_gains orig_icfg source_name target_branches (global, spec, inputof, outputof) =
	let curr_size = InterCfg.num_of_nonskip_nodes global.icfg in
	let lst = 
  	BatSet.fold (fun target_branch_node branch2reduction ->
			let _ = prerr_endline (Printf.sprintf "Calculating gain: %d / %d" ((List.length branch2reduction) + 1) (BatSet.cardinal target_branches)) in 
  		let pe_info (* global * spec, inputof * outputof *) =
  			let inputof' = (Table.add target_branch_node Mem.bot inputof) in 
  			partial_evaluation orig_icfg source_name (global,spec,inputof',outputof)  
  		in
  		let after_size =
				let (global', _, _, _) = pe_info in
				InterCfg.num_of_nonskip_nodes global'.icfg 
			in
  		let reduction = (curr_size - after_size) in
			(target_branch_node, reduction, pe_info) :: branch2reduction
  	) target_branches []
	in
	List.sort (fun (_, n1, _) (_, n2, _) -> n2 - n1) lst


(** Collect constant conditional expressions from oracle runs and perform initial partial evaluation    
  NOTE: write to file source_name only for oracle runs
	return 
	  target_branches: IntraCfg.Cmd.t BatSet.t
		(global,spec,inputof,outputof) after PE    
	*)
let ui_chisel_init source_name (global,spec,inputof,outputof) = 
	let global_filename = "global" in
	let orig_icfg = global.icfg in
	(* `Assoc                                               *)
  (* [ ("callgraph", CallGraph.to_json global.callgraph); *)
  (*   ("cfgs", InterCfg.to_json global.icfg)]            *)
	(* |> Yojson.Safe.pretty_to_channel stderr;             *)
	(** do pe upfront *)
	let (global, spec, inputof, outputof) = partial_evaluation orig_icfg source_name (global,spec,inputof,outputof) in
	save_source (Printf.sprintf "%s/after_init_pe.c" !Options.marshal_dir) global orig_icfg;
	Utils.save_global global global_filename;
	(** instrument the code to extract values of conditional exprs (global.file will change) *)
	let info_map_ref = ref BatMap.empty in 
	ignore(Cil.visitCilFileSameGlobals (new Instrument.collectVisitor info_map_ref) global.file); 
	ignore(Cil.visitCilFileSameGlobals (new Instrument.valuationVisitor !info_map_ref) global.file);
	(** Run the instrumented code *)
	(* clean up any previous log file *)
	let _ = (try Unix.unlink (!Options.marshal_dir ^ "/" ^ Instrument.outfile_name) with _ -> ()) in
	(* save instrumented source into file *) 
	let _ = Utils.save global.file source_name in
	let _ = Utils.save global.file (!Options.marshal_dir ^ "/instrumented_" ^ source_name) in
	(* run the oracle script *)
	let _ = Utils.oracle source_name in 
	(** Get valuations of conditional exprs *)
	let branch2vals = (* loc -> int BatSet.t *)
		let lines = Utils.get_lines (!Options.marshal_dir ^ "/" ^ Instrument.outfile_name) in
		List.fold_left (fun branch2vals line ->
			try
				let tokens = Str.split (Str.regexp " ") line in
				let _ = assert ((List.length tokens) = 4) in 
				let (line, file, byte, value) = 
					((int_of_string (List.nth tokens 0)), List.nth tokens 1, (int_of_string (List.nth tokens 2)), (int_of_string (List.nth tokens 3))) 
				in  
				let loc = {Cil.line = line; Cil.file = file; Cil.byte = byte} in
				let values = try BatMap.find loc branch2vals with _ -> BatSet.empty in 
				BatMap.add loc (BatSet.add value values) branch2vals
			with _ -> branch2vals  
		) BatMap.empty lines
	in 
	let _ = if ((BatMap.cardinal branch2vals) = 0) then failwith "Fail to instrument the program!" in 
	prerr_endline (Printf.sprintf "=== #. instrumented_branches: %d === \n" (BatMap.cardinal branch2vals));
	(** revert the original program back *)
	let global = Utils.load_global global_filename in
	let _ = Utils.save global.file source_name in
	let target_branches = get_refutable_branches global branch2vals BatSet.empty in
	prerr_endline (Printf.sprintf "=== #. constant branches: %d === \n" (BatSet.cardinal target_branches));
	let branch2reduction = compute_gains orig_icfg source_name target_branches (global, spec, inputof, outputof) in
	(branch2vals, target_branches, branch2reduction, global, spec, inputof, outputof)
	

exception Found of InterCfg.Node.t 
let node_of_cmd target_branch_cmd global =
	let nodes = InterCfg.nodesof global.icfg in
	try 
  	List.iter (fun node -> 	 
  		let cmd = InterCfg.cmdof global.icfg node in
  		if (Pervasives.compare cmd target_branch_cmd) = 0 then raise (Found node) 
  	) nodes;
  	None
	with Found node -> Some node

(** Main algorithm: *)
(* P <- P_orig *)
(* repeat *)
(*   P_instr <- instrument(P) *)
(*   write P_instr; run(P_instr) *)
(*   Pred_f <- preds always false *)
(*   for p in Pred_f *)
(*     P_!p <- |PE(!p, P)| *)
(*     gain(p) <- |P| - P_!p *)
(*   write P *)
(*   p <- p in Pred_f s.t. answer(!p) = T *)
(*   if (p = \bot) break *)
(*   P <- P_!p *)
(* write P *)
(* *)
(* starting from the for loop, pgms are cfgs*)
(* |P| = # nodes of non-skip cmd *)
(* PE(!p, P) = *)
(*   0. record |P| *)
(*   1. for all assume(p), n s.t. T(assume(p)) = \bot and assume(p) dom n, cmdof[n |-> skip]  (T = inputof, forall n. n dom n) *)
(*   2. T = A(P)  (A = analysis) *)
(*   3. if |P| changed, goto 0 *)
(* output: icfg *)

(* write to file source_name only when terminating *)
(* "target_branches" shrinks as iterations go by. *)
(* invariant: target_branches contain nodes of which commands exist. *)
let rec ui_chisel log_oc source_name orig_icfg (iter,already_covered,branch2vals,target_branches,branch2reduction)  (global,spec,inputof,outputof) =
	if (List.length branch2reduction) = 0 then 
		let _ = prerr_endline "No questions to be answered!" in
		let _ = save_source source_name global orig_icfg in 
		(global,spec,inputof,outputof)
	else  
	(** logging *)
	let curr_iter_source_name = Printf.sprintf "%s/iter_%d_%s" !Options.marshal_dir iter source_name in
	let _ = save_source curr_iter_source_name global orig_icfg in
	let _ = Printf.fprintf log_oc "%d" iter in 
	
	
	(** store the current programs size *)
	let curr_size = InterCfg.num_of_nonskip_nodes global.icfg in
	prerr_endline (Printf.sprintf "======== UI CHISEL : iteration - %d (size: %d, #questions: %d) =========" iter curr_size (BatSet.cardinal target_branches)); 	
	
	(* print all questions and gains *)
	let _ = 
		prerr_endline "======== Questions and Gains ========";
		List.iter (fun (target_branch_node, reduction, _) ->
			let target_branch_cmd = InterCfg.cmdof global.icfg target_branch_node in
			match target_branch_cmd with 
  		| IntraCfg.Cmd.Cassume (cond, loc, b) ->
				let question = (Cil.UnOp (Cil.LNot, cond, Cil.typeOf cond)) in
				prerr_endline (Printf.sprintf "%s at %s: %d ?" (CilHelper.s_exp question) (Cilglobal.s_location loc) reduction)
			| _ -> ()
		) branch2reduction;
		prerr_endline "=====================================\n";
		let max_gain, sum_gain =
  		List.fold_left (fun (max_gain, sum_gain) (_, reduction, _) -> 
  			((if max_gain < reduction then reduction else max_gain), sum_gain + reduction)
			) (0, 0) branch2reduction
  	in
		Printf.fprintf log_oc "\t%d\t%d\t%.1f" (List.length branch2reduction) max_gain ((float_of_int sum_gain) /. (float_of_int (List.length branch2reduction))) 
	in
	(* ask the user the topmost question *)
	let (target_branch_node, reduction, pe_info) = List.hd branch2reduction in 
	let target_branch_cmd = InterCfg.cmdof global.icfg target_branch_node in
	let answer = 
  	match target_branch_cmd with 
  	| IntraCfg.Cmd.Cassume (cond, loc, b) -> 	
  		(let question = (Cil.UnOp (Cil.LNot, cond, Cil.typeOf cond)) in 
			let _ = Printf.fprintf log_oc "\t%s at %s?" (CilHelper.s_exp question) (Cilglobal.s_location loc) in
  		Printf.printf "%s at %s: %d ?\n" (CilHelper.s_exp question) (Cilglobal.s_location loc) reduction;
  		try read_int () with _ -> -1) 
		| _ -> assert false  
	in
	let _ = Printf.fprintf log_oc "\t%d" answer in
	let reason =
		 prerr_endline "Reason?:";
		 try read_line () with _ -> " "
	in
	let _ = Printf.fprintf log_oc "\t%s" reason in
	
	let already_covered = BatSet.add target_branch_node already_covered in
	if answer = 1 then (* user confirms this likely invariant *)
		let (global, spec, inputof, outputof) = pe_info in
		(* recompute target_branches: some branches may have been removed by the partial evaluation *) 
		let target_branches = get_refutable_branches global branch2vals already_covered in
		(* recompute the gains *)
		let branch2reduction = compute_gains orig_icfg source_name target_branches (global, spec, inputof, outputof) in
		(* logging *) 
		let next_n_pid = List.length (InterCfg.pidsof global.icfg) in  
		let next_size = InterCfg.num_of_nonskip_nodes global.icfg in
		let _ = Printf.fprintf log_oc "\t%d\t%d\n" next_n_pid next_size in
		ui_chisel log_oc source_name orig_icfg (iter+1,already_covered,branch2vals,target_branches,branch2reduction) (global,spec,inputof,outputof)
	else if answer = 0 then (* user does not confirm the likely invariant. *)
		let target_branches' = get_refutable_branches global branch2vals already_covered in
		let delta_target_branches = BatSet.diff target_branches' target_branches in
		let delta_branch2reduction = compute_gains orig_icfg source_name delta_target_branches (global, spec, inputof, outputof) in
		let branch2reduction = List.filter (fun (n,_,_) -> (Pervasives.compare n target_branch_node) != 0) (delta_branch2reduction @ branch2reduction) in
		let branch2reduction = List.sort (fun (_, n1, _) (_, n2, _) -> n2 - n1) branch2reduction in  
		(* logging *) 
		let next_n_pid = List.length (InterCfg.pidsof global.icfg) in  
		let next_size = InterCfg.num_of_nonskip_nodes global.icfg in
		let _ = Printf.fprintf log_oc "\t%d\t%d\n" next_n_pid next_size in
		ui_chisel log_oc source_name orig_icfg (iter+1,already_covered,branch2vals,target_branches',branch2reduction) (global,spec,inputof,outputof)		
	else (* user wants to stop: save the file and quit. *)
		let _ = save_source source_name global orig_icfg in
		let _ = prerr_endline "Chisel is asked to stop the process!" in 
		(global,spec,inputof,outputof)

	
let do_analysis : Global.t -> Global.t * Table.t * Table.t * Report.query list
= fun global ->
  let _ = prerr_memory_usage () in
  let locset = get_locset global.mem in
  let locset_fs = PartialFlowSensitivity.select global locset in
  let unsound_lib = UnsoundLib.collect global in
  let unsound_update = (!Options.bugfinder >= 2) in
  let unsound_bitwise = (!Options.bugfinder >= 1) in
  let spec = { Spec.empty with
    Spec.locset; Spec.locset_fs; premem = global.mem; Spec.unsound_lib;
    Spec.unsound_update; Spec.unsound_bitwise; } in
  cond !Options.marshal_in marshal_in (Analysis.perform spec) global
  |> opt !Options.marshal_out marshal_out
	|> (fun (global,inputof,outputof) ->
			 let orig_cfg = global.icfg in 
		   let source_name = try List.hd !Frontend.files with _ -> assert false in
			 let log_oc = 
				 let log_file_name = (!Options.marshal_dir ^ "/log_info.txt") in 
				 (try Unix.unlink log_file_name with _ -> ());
    	 	 open_out_gen [Open_text; Open_creat; Open_append] 0o755 log_file_name 
    	 in
			 let n_pid = List.length (InterCfg.pidsof global.icfg) in  
			 let pgm_size = InterCfg.num_of_nonskip_nodes global.icfg in
			 Printf.fprintf log_oc "%s - #func: %d \t #instrs : %d\n" source_name n_pid pgm_size; 
			 let (branch2vals, target_branches, branch2reduction, global, spec, inputof, outputof) = 
				 ui_chisel_init source_name (global,spec,inputof,outputof) 
			 in
			 let n_pid = List.length (InterCfg.pidsof global.icfg) in  
			 let pgm_size = InterCfg.num_of_nonskip_nodes global.icfg in
		   Printf.fprintf log_oc "After initial PE: #func: %d \t #instrs : %d\n" n_pid pgm_size;
			 Printf.fprintf log_oc "Iter\t#Questions\tMax gain\tAvg gain\tQuestion\tAnswer\tReason\t#func\t#instrs\n"; 
			 let _ = 
				ui_chisel log_oc source_name orig_cfg (1, BatSet.empty, branch2vals, target_branches, branch2reduction) (global,spec,inputof,outputof)
			 in
			 close_out log_oc; 
			 exit 0 
		 )
  |> StepManager.stepf true "Generate Alarm Report" (fun (global,spec,inputof,outputof) ->
      (global,inputof,outputof,inspect_alarm global spec inputof outputof))
