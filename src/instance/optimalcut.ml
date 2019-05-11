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

let string_of_inter_node pid2id node = 
	let pid = InterCfg.Node.get_pid node in 
	let cfgnode = InterCfg.Node.get_cfgnode node in
	let node_str =
		try  
		(* Printf.sprintf "v_%d_%s" (BatMap.find pid pid2id) (IntraCfg.Node.to_string cfgnode) *)
		Printf.sprintf "%s#%s" pid (IntraCfg.Node.to_string cfgnode)
		with _ -> failwith pid 
	in
	node_str

let string_of_inter_edge pid2id node = Printf.sprintf "e#%s" (string_of_inter_node pid2id node)

let create_cplex gain cost global target_branches nodes pid2id =
	let icfg = global.icfg in 
	let goal_str, all_node_str =
		((string_of_list ~first:"" ~last:"" ~sep:" + " (string_of_inter_node pid2id) nodes),
		 (string_of_list ~first:"" ~last:"" ~sep:"\n" (string_of_inter_node pid2id) nodes))
	in
	let all_edge_str =
		string_of_set ~first:"" ~last:"" ~sep:"\n" (string_of_inter_edge pid2id) target_branches  	 
	in
	(* let k_bound_str =                                                                    *)
	(* 	(string_of_set ~first:"" ~last:"" ~sep:" + " string_of_inter_edge target_branches) *)
	(* 	^ " <= " ^ (string_of_int k)                                                       *)
	(* in                                                                                   *)
	let constr_str =
		let src_node = InterCfg.start_node in
		(* Entry node should be present *)
		let constr_str = Printf.sprintf "%s >= 1\n%s <= 1\n" (string_of_inter_node pid2id src_node) (string_of_inter_node pid2id src_node) in 
		(* call edges *)
		let call_nodes = InterCfg.callnodesof icfg in
		let constr_str = 
  		List.fold_left (fun constr_str call_node ->
				let callees = InterCfg.get_callees call_node icfg in 
				InterCfg.ProcSet.fold (fun proc constr_str -> 
					let callee_node = (InterCfg.Node.make proc IntraCfg.Node.entry) in 
					Printf.sprintf "%s%s - %s <= 0\n" constr_str (string_of_inter_node pid2id call_node) (string_of_inter_node pid2id callee_node)
				) callees constr_str  
  		) constr_str call_nodes
		in  
		(* intra edges *)
		let constr_str = 
			List.fold_left (fun constr_str node ->
				let node_str = (string_of_inter_node pid2id node) in
				let pid = InterCfg.Node.get_pid node in
				let succs = IntraCfg.succ (InterCfg.Node.get_cfgnode node) (InterCfg.cfgof icfg pid) in
				if (BatSet.mem node target_branches) then (* weak_edge *)
					let _ = assert ((List.length succs) = 1) in (* assume node should have a single successor *)
					let edge_str = string_of_inter_edge pid2id node in  
					let succ_node = InterCfg.Node.make pid (List.hd succs) in 
					let succ_str = (string_of_inter_node pid2id succ_node) in 
					if (String.compare node_str succ_str) = 0 then constr_str
					else Printf.sprintf "%s%s - %s - %s <= 0\n" constr_str node_str succ_str edge_str  	 
				else (* strong_edge *) 
					List.fold_left (fun constr_str succ ->
						let succ_node = InterCfg.Node.make pid succ in
						let succ_str = (string_of_inter_node pid2id succ_node) in 
						if (String.compare node_str succ_str) = 0 then constr_str 
						else Printf.sprintf "%s%s - %s <= 0\n" constr_str node_str succ_str	 
					) constr_str succs 
			) constr_str nodes 
		in
		constr_str
	in
	let lowerbound_str =
		(string_of_set ~first:"" ~last:"" ~sep:" + " (string_of_inter_edge pid2id) target_branches) ^ " >= 1 "
	in
	let ratio_str =
		let ratio_str =
			string_of_list ~first:"" ~last:"" ~sep:" + " id  
			(List.map (fun node -> Printf.sprintf "%d %s" cost (string_of_inter_node pid2id node)) nodes) 
		in 
		let ratio_str = 
  		BatSet.fold (fun node ratio_str ->
  			Printf.sprintf "%s + %d %s" ratio_str gain (string_of_inter_edge pid2id node)	 
  		) target_branches ratio_str 
		in  		 
		ratio_str ^ " <= " ^ (string_of_int (cost * (List.length nodes)))
	in
	let int_bounds_str =
		let int_bounds_str = string_of_list ~first:"" ~last:"" ~sep:"" (fun x -> Printf.sprintf "%s <= 1\n" (string_of_inter_node pid2id x)) nodes in 
		let int_bounds_str = int_bounds_str ^ (string_of_list ~first:"" ~last:"" ~sep:"" (fun x -> Printf.sprintf "%s >= 0\n" (string_of_inter_node pid2id x)) nodes) in
		let int_bounds_str = int_bounds_str ^ (string_of_set ~first:"" ~last:"" ~sep:"" (fun x -> Printf.sprintf "%s <= 1\n" (string_of_inter_edge pid2id x)) target_branches) in
		let int_bounds_str = int_bounds_str ^ (string_of_set ~first:"" ~last:"" ~sep:"" (fun x -> Printf.sprintf "%s >= 0\n" (string_of_inter_edge pid2id x)) target_branches) in
		int_bounds_str		 
	in
	Printf.sprintf
   "minimize\n%s\nsubject to\n%s\n%s\n%s\nbounds\n%s\ninteger\n%s\n%s\nend\n"
		goal_str
		lowerbound_str
		ratio_str (*k_bound_str*)
		constr_str
		int_bounds_str
		all_node_str
		all_edge_str

		
let get_optimal_questions orig_icfg global target_branches exclude_nodes =
	let rec gcd a b = let a,b = if a < b then b,a else a,b in if b = 0 then a else gcd b (a mod b) in 
	let entire_nodes = BatSet.elements (BatSet.diff (list2set (InterCfg.nodesof global.icfg)) exclude_nodes) in 
	let gains = BatList.range 0 `To (List.length entire_nodes) in 
	let costs = BatList.range 1 `To (BatSet.cardinal target_branches) in
	let ratios = 
		List.fold_left (fun s gain ->
			List.fold_left (fun s cost ->
				let gcd = gcd gain cost in 
				BatSet.add (gain / gcd, cost / gcd) s 	 
			) s costs 
		) BatSet.empty gains |> BatSet.elements |> List.sort (fun (g1, c1) (g2, c2) -> g1 / c1 - g2 / c2)   
	in
	let i = ref 0 in 
	let k = ref (List.length ratios) in 
	let result = ref BatSet.empty in 
	let last_gain = ref 0 in 
	let last_cost = ref 0 in 
	let pid2id, id2pid, _ =
		List.fold_left (fun (pid2id, id2pid, id) pid ->
			let id = id + 1 in
			let pid2id = BatMap.add pid id pid2id in 
			let id2pid = BatMap.add id pid id2pid in
			(* prerr_endline ("func: " ^ pid);   *)
			(pid2id, id2pid, id)		 
		) (BatMap.empty, BatMap.empty, 0) (InterCfg.pidsof orig_icfg) 
		(* (BatSet.elements (List.fold_left (fun pids node -> BatSet.add (InterCfg.Node.get_pid node) pids) BatSet.empty entire_nodes))   *)
	in
	while !i + 1 < !k do
		let j = (!i + !k) / 2 in 
		let gain, cost = List.nth ratios j in
		prerr_string (Printf.sprintf "Trying gain / cost = %d / %d ... " gain cost);
		let cplex = create_cplex gain cost global target_branches entire_nodes pid2id in
  	let _ = Utils.save_text cplex (!Options.marshal_dir ^ "/lp.lp") in
  	let broken_branches = Utils.lpsolve (!Options.marshal_dir ^ "/lp.lp") in
		if (not (BatSet.is_empty broken_branches)) then (* feasible *) 
			(prerr_endline "feasible";
			 last_gain := gain; 
			 last_cost := cost;     
			 result := broken_branches; 
			 i := j) 
		else 
			(prerr_endline "infeasible";
			 k := j) 
	done;
	prerr_endline (Printf.sprintf "Best gain / cost ratio : %d / %d" !last_gain !last_cost);
	let final_ratio = if !last_cost != 0 then (float_of_int !last_gain) /. (float_of_int !last_cost) else 0. in   
	(!result, final_ratio)
