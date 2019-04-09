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
open Yojson.Safe
open Vocab
open BasicDom
open InterCfg
open Cil
open Pretty
open Cilglobal

type t = {
  file : Cil.file;
  icfg : InterCfg.t;
  callgraph : CallGraph.t;
  dump : Dump.t;
  mem : ItvDom.Mem.t;
  table : ItvDom.Table.t;
}

let remove_node : InterCfg.node -> t -> t = fun node global ->
  { global with
    icfg = InterCfg.remove_node node global.icfg }

let remove_function : InterCfg.Proc.t -> t -> t = fun pid global ->
  { global with
    icfg = InterCfg.remove_function pid global.icfg;
    callgraph = CallGraph.remove_function pid global.callgraph;
    dump = Dump.remove pid global.dump }

let is_rec : InterCfg.pid -> t -> bool = fun pid global ->
  CallGraph.is_rec global.callgraph pid

let remove_unreachable_nodes : t -> t
=fun global ->
  let nodes_all = InterCfg.nodesof global.icfg in
  let unreachable = InterCfg.unreachable_node global.icfg in
  let global = NodeSet.fold remove_node unreachable global in
  my_prerr_newline ();
  my_prerr_string "#nodes all   : ";
  my_prerr_endline (string_of_int (List.length nodes_all));
  my_prerr_string "#unreachable : ";
  my_prerr_endline (string_of_int (NodeSet.cardinal unreachable));
(*    prerr_string "unreachable nodes : ";
  prerr_endline (string_of_set InterCfg.Node.to_string unreachable); *)
  global


class expVisitor addr_taken_functions_ref = object(self)
  inherit nopCilVisitor
  method vexpr exp =
		(match exp with 
		| AddrOf lv -> 
			(match lv with 
			| (Var vi, _) -> 
				if Cil.isFunctionType vi.vtype then 
					addr_taken_functions_ref := BatSet.add vi.vname !addr_taken_functions_ref
			| _ -> ())   
		| _ -> ());
		DoChildren
end

let get_address_taken_functions : t -> PowProc.t 
= fun global -> 
	let addr_taken_functions_ref = ref BatSet.empty in 
	let nodes = InterCfg.nodesof global.icfg in 
	List.iter (fun node -> 
		let cmd = InterCfg.cmdof global.icfg node in
		match cmd with 
	  | IntraCfg.Cmd.Cset (_, exp, _) -> 
			let _ = Cil.visitCilExpr (new expVisitor addr_taken_functions_ref) exp in ()
		| IntraCfg.Cmd.Ccall (_, exp, exps, _) -> 
			let _ = Cil.visitCilExpr (new expVisitor addr_taken_functions_ref) exp in 
			List.iter (fun exp -> let _ = Cil.visitCilExpr (new expVisitor addr_taken_functions_ref) exp in ()) exps
		| IntraCfg.Cmd.Creturn (e_opt, _) -> 
			(match e_opt with None -> () | Some exp -> let _ = Cil.visitCilExpr (new expVisitor addr_taken_functions_ref) exp in ())  
		| _ -> ()
	) nodes;
	PowProc.of_list (BatSet.elements !addr_taken_functions_ref) 


let remove_unreachable_functions : t -> t
=fun global ->
	let addr_taken_functions = get_address_taken_functions global in
	(*prerr_endline ("addr_taken_functions   : " ^ (PowProc.to_string addr_taken_functions)); *)
  let pids_all = PowProc.of_list (InterCfg.pidsof global.icfg) in
  let reachable = CallGraph.trans_callees InterCfg.global_proc global.callgraph in
	(* add addr_taken_functions *)
	let reachable = PowProc.union reachable addr_taken_functions in 
  let unreachable = PowProc.diff pids_all reachable |> PowProc.remove InterCfg.global_proc in
  let recursive = PowProc.filter (fun pid -> is_rec pid global) reachable in
  let global = if !Options.bugfinder >= 2 then global else PowProc.fold remove_function unreachable global in
  my_prerr_newline ();
  my_prerr_endline ("#functions all : " ^ string_of_int (PowProc.cardinal pids_all));
  my_prerr_endline ("#recursive : " ^ string_of_int (PowProc.cardinal recursive));
  if PowProc.cardinal recursive > 0 then my_prerr_endline (PowProc.to_string recursive);
  my_prerr_endline ("#unreachable   : " ^ string_of_int (PowProc.cardinal unreachable));
  if PowProc.cardinal unreachable > 0 then my_prerr_endline (PowProc.to_string unreachable);
  
	(* let oc = open_out "tmp.c" in                                                         *)
  (* let globals = global.file.globals in *)
	(* (* print #stmts of unreachable *)                                                    *)
	(* let n_total_stmt, n_unreachable_stmt =                                               *)
	(* 	(List.fold_left (fun (n_total_stmt, n_unreachable_stmt) g ->                       *)
	(* 		match g with                                                                     *)
	(* 		| Cil.GFun(fd,_) ->                                                              *)
	(* 			let fid = (fd.svar.vname) in                                                   *)
	(* 			let fd_size = List.fold_left (+) 0 (List.map numstmts_of_stmt fd.sallstmts) in *)
	(* 			let n_total_stmt = n_total_stmt + fd_size in                                   *)
	(* 			let n_unreachable_stmt =                                                       *)
  (* 				if (PowProc.mem fid unreachable) then                                        *)
  (* 					n_unreachable_stmt + fd_size                                               *)
  (* 				else n_unreachable_stmt                                                      *)
	(* 			in                                                                             *)
	(* 			(n_total_stmt, n_unreachable_stmt)                                             *)
	(* 		| _ -> (n_total_stmt, n_unreachable_stmt)                                        *)
	(* 	) (0, 0) globals)                                                                  *)
	(* in                                                                                   *)
	(* Printf.printf "#. total stmts: %d \n" n_total_stmt ;                                 *)
	(* Printf.printf "#. stmts of unreachable funcs: %d \n" n_unreachable_stmt ;            *)
	 
  (* my_prerr_endline ("#old   : " ^ string_of_int (List.length globals)); *)
	
  (* let _,globals =                                                                                    *)
  (*   List.fold_left (fun (set, lst) glob ->                                                           *)
  (*       match glob with                                                                              *)
  (*         Cil.GFun(fundec,_) ->                                                                      *)
  (*         if (PowProc.mem fundec.svar.vname unreachable) || (GlobalSet.mem glob set) then (set, lst) *)
  (*         else (GlobalSet.add glob set, lst@[glob])                                                  *)
  (*       | Cil.GVarDecl(vardecl,_) ->                                                                 *)
  (*         if (PowProc.mem vardecl.vname unreachable) || (GlobalSet.mem glob set) then (set, lst)     *)
  (*         else (GlobalSet.add glob set, lst@[glob])                                                  *)
  (*       | _ -> (GlobalSet.add glob set, lst@[glob])                                                  *)
  (*     ) (GlobalSet.empty,[]) globals                                                                 *)
  (* in                                                                                                 *)
	
  (* let _ = global.file.globals <- globals in  *)
  (* my_prerr_endline ("#new   : " ^ string_of_int (List.length globals));                         *)
  (* let _ = dumpFile (new simpleCilPrinterClass (*Cil.defaultCilPrinterClass*)) oc global.file in *)
  (* close_out oc;                                                                                 *)
	(* print doms *)
	(* let dom_map =                                                                                                        *)
  (* 	PowProc.fold2 (fun pid_d pid_n m ->                                                                                *)
  (*   		let d_doms = BatMap.find pid_d m in                                                                            *)
  (*   		if (CallGraph.is_dom pid_d pid_n global.callgraph) then                                                        *)
	(* 				BatMap.add pid_d (BatSet.add pid_n d_doms) m                                                                 *)
  (*   		else m                                                                                                         *)
  (* 		)	pids_all pids_all (PowProc.fold (fun pid m -> BatMap.add pid (BatSet.singleton pid) m) pids_all BatMap.empty) *)
	(* in                                                                                                                   *)
	(* let pid2size =                                                                                                       *)
	(* 	List.fold_left (fun m g ->                                                                                         *)
  (*     match g with                                                                                                     *)
  (*     | Cil.GFun (fd, _) ->                                                                                            *)
	(* 			(* let _ = prerr_endline (fd.svar.vname) in *)                                                                 *)
	(* 			let fid = (fd.svar.vname) in                                                                                   *)
	(* 			let _ = if (not (BatMap.mem fid dom_map)) then failwith fid in                                                 *)
	(* 			let fd_size = List.fold_left (+) 0 (List.map numstmts_of_stmt fd.sallstmts) in                                 *)
	(* 			BatMap.add fd.svar.vname fd_size m                                                                             *)
  (*     | _ -> m                                                                                                         *)
	(* 	) BatMap.empty globals                                                                                             *)
	(* in                                                                                                                   *)
	(* let dom_lst =                                                                                                        *)
	(* 	let lst =                                                                                                          *)
  (*   	BatMap.foldi (fun pid_d d_doms lst ->                                                                            *)
  (*   		(pid_d, (BatSet.fold (fun pid size -> size + (try BatMap.find pid pid2size with _ -> 0)) d_doms 0)) :: lst     *)
	(* 		) dom_map []                                                                                                     *)
	(* 	in                                                                                                                 *)
	(* 	List.sort (fun (_, n1) (_, n2) -> n2 - n1) lst                                                                     *)
	(* in                                                                                                                   *)
	(* List.iter (fun (pid_d, n_doms) -> Printf.printf "%s: %d\n" pid_d n_doms) dom_lst; *)
	global

let init file =
	let _ = IntraCfg.Node.init () in
  { file = file;
    icfg = InterCfg.init file;
    callgraph = CallGraph.empty;
    dump = Dump.empty;
    mem = ItvDom.Mem.bot;
    table = ItvDom.Table.bot; }
  (* |> remove_unreachable_nodes *)

let is_undef : InterCfg.pid -> t -> bool = fun pid global ->
  InterCfg.is_undef pid global.icfg

let get_leaf_procs : t -> PowProc.t
=fun global ->
  let pids = PowProc.of_list (InterCfg.pidsof global.icfg) in
  PowProc.fold (fun fid ->
        if PowProc.cardinal (CallGraph.trans_callees fid global.callgraph) = 1
        then PowProc.add fid
        else id) pids PowProc.bot

let to_json : t -> json
= fun g ->
  `Assoc
      [ ("callgraph", CallGraph.to_json g.callgraph);
        ("cfgs", InterCfg.to_json g.icfg)
      ]

let print_json : out_channel -> t -> unit
= fun chan g ->
  Yojson.Safe.pretty_to_channel chan (to_json g)
