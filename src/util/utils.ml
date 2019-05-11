open Cil
open Pretty
open Profiler
open Vocab
open Global

let fid_of_fundec fundec = fundec.svar.vname

class simpleCilPrinterClass = object (self)
  inherit Cil.defaultCilPrinterClass as super
  (* method pLineDirective ?(forcefile=false) l = Pretty.nil *)
  method pGlobal () global =
    match global with
    | Cil.GVarDecl (vi, l) when Hashtbl.mem Cil.builtinFunctions vi.vname -> Pretty.nil
    | Cil.GVarDecl (vi, l) ->
      (match vi.vtype with
       | TFun (_, _, _, attr) ->
          if List.mem (Cil.Attr ("missingproto", [])) attr then Pretty.nil
          else (super#pVDecl () vi) ++ text ";\n"
       | _ -> (super#pVDecl () vi) ++ text ";\n")
    | _ -> super#pGlobal () global
	
	(* method pExp () exp = (Pretty.chr '(') ++ (super#pExp () exp) ++ (Pretty.chr ')') *)
		
end

class excludeCilPrinterClass exclude_locs = object (self)
  inherit simpleCilPrinterClass as super
  
	method pStmtKind stmt () skind =
		let loc = (Cil.get_stmtLoc skind) in 
		(* prerr_endline (Printf.sprintf "printing: %s - %b" (CilHelper.s_location loc) (BatSet.mem loc exclude_locs));  *)
		if BatSet.mem loc exclude_locs then (Pretty.chr '{') ++ (super#pStmtKind stmt () (Cil.Instr [])) ++ (Pretty.chr '}') (*Pretty.nil*) 
		else super#pStmtKind stmt () skind
end

								
let save_bin cil fname =
  let oc = open_out (!Options.marshal_dir ^ "/" ^ fname) in
  Marshal.to_channel oc cil [];
  close_out oc

let load_bin fname =
  let ic = open_in (!Options.marshal_dir ^ "/" ^ fname) in
  let cil = Marshal.from_channel ic in
  close_in ic;
  cil

		
let save_global global fname =
	MarshalManager.output (fname ^ ".file") global.file;
  MarshalManager.output (fname ^ ".icfg") global.icfg;
  MarshalManager.output (fname ^ ".callgraph") global.callgraph;
	MarshalManager.output (fname ^ ".dump") global.dump;
	MarshalManager.output (fname ^ ".mem") global.mem;
	MarshalManager.output (fname ^ ".table") global.table

let load_global fname =
  let file = MarshalManager.input (fname ^ ".file") in 
  let icfg = MarshalManager.input (fname ^ ".icfg") in 
  let callgraph = MarshalManager.input (fname ^ ".callgraph") in 
	let dump = MarshalManager.input (fname ^ ".dump") in 
	let mem = MarshalManager.input (fname ^ ".mem") in 
	let table = MarshalManager.input (fname ^ ".table") in 
	{file = file; 
	icfg = icfg; 
	callgraph = callgraph; 
	dump = dump; 
	mem = mem; 
	table = table} 
	
let dumpFile pp out file =
  Pretty.fastMode := true;
  Cil.iterGlobals file (fun g -> Cil.dumpGlobal pp out g);
  flush out

let save cil fname =
  (try Unix.unlink fname with _ -> ());
  let oc = open_out fname in
  dumpFile (new simpleCilPrinterClass) oc cil;
  close_out oc

let save_with_excludes cil fname exclude_locs =
  (try Unix.unlink fname with _ -> ());
  let oc = open_out fname in
  dumpFile (new excludeCilPrinterClass exclude_locs) oc cil;
  close_out oc

let save_text str fname =
  (try Unix.unlink fname with _ -> ());
  let oc = open_out fname in
  output_string oc str;
  close_out oc
												
let debug_out = ref stderr 
(**/**)
(** we copy all debugging output to a file and to stdout *)
let debug fmt = 
  let k result = begin
      output_string !debug_out result ; 
      (* output_string stdout result ;  *)
      (* flush stdout ;  *)
      flush !debug_out;
  end in
    Printf.kprintf k fmt 

(** much like debug, but with ABORT prepending to the message and exits 1 when
    done *)
let abort fmt = 
  let k result = begin
      output_string !debug_out result ; 
      (* output_string stdout result ;  *)
      (* flush stdout ;  *)
      flush !debug_out;
    exit 1 
  end in
		(* load the original source *)
		(* let cil = load_bin (!Frontend.fname ^ ".orig.bin") in *)
		(* let _ = save cil !Frontend.fname in                   *)
    debug "\nABORT:\n\n" ; 
    Printf.kprintf k fmt 

(** return a copy of 'lst' where each element occurs once *)
let uniq lst = 
  let ht = Hashtbl.create 255 in 
  let lst = List.filter (fun elt ->
    if Hashtbl.mem ht elt then false
    else begin
      Hashtbl.add ht elt () ;
      true 
    end 
  ) lst in
    lst 

(** Helper function for generating inclusive ranges *)
let (--) i j = 
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
  in aux j []

let get_lines (filename : string) : string list =
	try  
    let fin = open_in filename in
    let res = ref [] in
      (try
         while true do
           res := (input_line fin) :: !res
         done
       with End_of_file -> close_in fin);
      List.rev !res
	with _ -> [] 


let prdbg_endline s =
  if !Options.debug then prerr_endline s
  else ()


let compile cil =
	assert ((List.length !Frontend.files) > 0);
	save cil (List.hd !Frontend.files);
	let garbage =
		let filename = "/dev/null" in
		Unix.openfile filename [Unix.O_CREAT; Unix.O_WRONLY] 0o640
	in
  let _ = Unix.create_process !Options.compile [|!Options.compile|] Unix.stdin garbage garbage in
  let r = match snd (Unix.wait ()) with
    | Unix.WEXITED 0 -> true
    | _ -> false
  in
	let _ = Unix.close garbage in
  r

let oracle fname =
	let garbage =
		let filename = "/dev/null" in
		Unix.openfile filename [Unix.O_CREAT; Unix.O_WRONLY] 0o640
	in
  let _ = Unix.create_process !Options.oracle [|!Options.oracle|] Unix.stdin garbage garbage in
  let r = match snd (Unix.wait ()) with
    | Unix.WEXITED 0 -> true
    | _ -> false
  in
	let _ = Unix.close garbage in
  r

let lpsolve fname =
	let lpsolver = "glpsol" in 
	let outfile = !Options.marshal_dir ^ "/lp.out" in
	let solfile = !Options.marshal_dir ^ "/lp.sol" in 
	let garbage =
		let filename = "/dev/null" in
		Unix.openfile filename [Unix.O_CREAT; Unix.O_WRONLY] 0o640
	in
  let _ = Unix.create_process lpsolver [|lpsolver; "--tmlim"; (string_of_int !Options.ilp_timeout); "-w"; solfile; "-o"; outfile; "--lp"; fname|] Unix.stdin garbage garbage in
  let r = match snd (Unix.wait ()) with
    | Unix.WEXITED 0 -> true
    | _ -> false
  in
	let _ = Unix.close garbage in
	if (not r) then failwith "Failed to invoke ILP solver!"
	else
		let outlines = get_lines outfile in 
		let sollines = get_lines solfile in
 (* 139 e_main_95    *              1             0             1
    140 e_main_97    *              1             0             1
    141 e_main_99    *              0             0             1
    142 e_main_101   *              0             0             1
	*)
 (* j 12405 1
	  j 12406 1
		j 12407 0 
	*)
		let id2var = 
			List.fold_left (fun id2var line -> 
				try 
  				let tokens = List.filter (fun x -> (String.compare x "") != 0) (Str.split (Str.regexp " ") line) in
  				if (List.length tokens) >= 2 && (String.compare (String.sub (List.nth tokens 1) 0 2) "e#") = 0 then
  					let id = int_of_string (List.nth tokens 0) in
  					let var = (List.nth tokens 1) in  
  					BatMap.add id var id2var 
  				else id2var   	 
				with _ -> id2var
			) BatMap.empty outlines 
		in 
		List.fold_left (fun broken_branches line ->
			let tokens = List.filter (fun x -> (String.compare x "") != 0) (Str.split (Str.regexp " ") line) in
			if (List.length tokens) = 3 && (String.compare (List.nth tokens 0) "j") = 0 
			   && (String.compare (List.nth tokens 2) "1") = 0
			then
				let id = int_of_string (List.nth tokens 1) in
				if not (BatMap.mem id id2var) then broken_branches  (* node *)
				else (* edge *)
  				let var = try BatMap.find id id2var with _ -> assert false in   
  				let tokens' = Str.split (Str.regexp "#") var in
  				let _ = if ((List.length tokens') != 3) then failwith var in
  				let pid = (List.nth tokens' 1) in
  				let nid = int_of_string (List.nth tokens' 2) in
  				BatSet.add (InterCfg.Node.make pid (IntraCfg.Node.from_id nid)) broken_branches
			else broken_branches
		) BatSet.empty sollines 
	
		(* List.fold_left (fun broken_branches line ->                                                           *)
		(* 	let tokens = List.filter (fun x -> (String.compare x "") != 0) (Str.split (Str.regexp " ") line) in *)
		(* 	(* let _ = prerr_endline (string_of_list id tokens) in    *)                                        *)
		(* 	if (List.length tokens) = 6 && ((List.nth tokens 1).[0] = 'e') && (List.nth tokens 3) = "1" then    *)
		(* 		let tokens' = Str.split (Str.regexp "_") (List.nth tokens 1) in                                   *)
		(* 		let _ = assert ((List.length tokens') = 4) in                                                     *)
		(* 		let pid = BatMap.find (int_of_string (List.nth tokens' 2)) id2pid in                              *)
		(* 		let nid = int_of_string (List.nth tokens' 3) in                                                   *)
		(* 		BatSet.add (InterCfg.Node.make pid (IntraCfg.Node.from_id nid)) broken_branches   	              *)
		(* 	else broken_branches                                                                                *)
		(* ) BatSet.empty lines                                                                                  *)