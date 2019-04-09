open Cil
open CilHelper
open Vocab 
open Utils
open Cilglobal
open Sys

let valuation_src = "valuation.c" 
let valuation_out = "valuation.out"
let valuation_prefix = "__valuation"
let outfile_name = "__tmp_file"
let outfile_descr = "__tmp_descr" 
let file_type = ref (TVoid([]))  (* will be set in preprocessors.ml *)
let builtin_args: (string * Cil.formatArg) list ref = ref [] 

class collectVisitor info_map_ref = object(self)
  inherit nopCilVisitor
	
  method vstmt stmt =
		(match stmt.skind with 
		| If (cond_exp, _, _, loc) -> info_map_ref := BatMap.add loc cond_exp !info_map_ref
		| _ -> ());
		DoChildren
end

(* info_map : loc -> exp *)
class valuationVisitor info_map = object(self)
  inherit nopCilVisitor 
	(* fprintf ("%d %s %d %d\n", line file byte (cond_exp)); *)
  method vstmt stmt =
		let curr_fundec =
			match !currentGlobal with
			| GFun (fundec, _) -> fundec
			| _ -> assert false
		in
		ChangeDoChildrenPost (stmt, (fun stmt ->
  		if (BatMap.mem !currentLoc info_map) then
  			let file_ptr_type = TPtr(!file_type, []) in 
    		let file_descr = makeLocalVar curr_fundec ~insert:false outfile_descr file_ptr_type in
  			let cond_exp = try BatMap.find !currentLoc info_map with _ -> assert false in
  			let before_instr_stmts =
  				let print_str, args =
  					let format_str = "%d %s %d %d\n" in
  					let print_str = 
  						"%l:file = fopen(%g:filename, %g:mode);\n" ^
  						"fprintf(%l:file, %g:format_str, %d:line, %g:target_cfilename, %d:byte, %e:vname);\n" ^
  						"fflush(%l:file);\n" ^
  						"fclose(%l:file);\n"
  					in
  					(print_str, [("filename", Fg (!Options.marshal_dir ^ "/" ^ outfile_name)); ("file", Fl(var file_descr));
  											 ("format_str", Fg(format_str)); ("line", Fd !currentLoc.line); ("target_cfilename", Fg !currentLoc.file); ("byte", Fd !currentLoc.byte); 
  											 ("mode", Fg ("a")); ("vname", Fe cond_exp)])
  				in 
  				let before_instr_stmts =
  					Formatcil.cStmts print_str (fun _ -> failwith "no new varinfos!")
  					!currentLoc (args @ !builtin_args)
  				in 
  				before_instr_stmts
  			in	
  			let _ = stmt.skind <- Block (mkBlock (before_instr_stmts @ [mkStmt stmt.skind])) in 
				stmt
			else stmt 
  		))
end

(* class valuationVisitor info_map = object(self)                                                                                                                         *)
(*   inherit nopCilVisitor                                                                                                                                                *)
	
(* 	(* fprintf ("%d %s %d %d\n", line file byte (cond_exp)); *)                                                                                                          *)
(*   method vblock b =                                                                                                                                                    *)
(* 		let curr_fundec, curr_fid =                                                                                                                                        *)
(* 			match !currentGlobal with                                                                                                                                        *)
(* 			| GFun (fundec, _) -> fundec, (fid_of_fundec fundec)                                                                                                             *)
(* 			| _ -> assert false                                                                                                                                              *)
(* 		in                                                                                                                                                                 *)
(* 		ChangeDoChildrenPost(b, (fun b ->                                                                                                                                  *)
(* 			let file_ptr_type = TPtr(!file_type, []) in                                                                                                                      *)
(* 	  	let file_descr = makeLocalVar curr_fundec ~insert:false outfile_descr file_ptr_type in                                                                           *)
(*       let stmts =                                                                                                                                                      *)
(*   			List.fold_left (fun l stmt ->                                                                                                                                  *)
(* 					(* let _ = prerr_endline (fid ^ " , " ^ (string_of_int sid)) in  *)                                                                                          *)
(* 					if not (BatMap.mem !currentLoc info_map) then                                                                                                                *)
(* 						l @ [stmt]                                                                                                                                                 *)
(* 					else                                                                                                                                                         *)
(* 						let cond_exp = try BatMap.find !currentLoc info_map with _ -> assert false in                                                                              *)
(* 						let before_instr_stmts =                                                                                                                                   *)
(* 							let print_str, args =                                                                                                                                    *)
(* 								let format_str = "%d %s %d %d\n" in                                                                                                                    *)
(* 								let print_str =                                                                                                                                        *)
(* 									"%l:file = fopen(%g:filename, %g:mode);\n" ^                                                                                                         *)
(* 									"fprintf(%l:file, %g:format_str, %d:line, %g:target_cfilename, %d:byte, %e:vname);\n" ^                                                              *)
(* 									"fflush(%l:file);\n" ^                                                                                                                               *)
(* 									"fclose(%l:file);\n"                                                                                                                                 *)
(* 								in                                                                                                                                                     *)
(* 								(print_str, [("filename", Fg (!Options.marshal_dir ^ "/" ^ outfile_name)); ("file", Fl(var file_descr));                                               *)
(* 														 ("format_str", Fg(format_str)); ("line", Fd !currentLoc.line); ("target_cfilename", Fg !currentLoc.file); ("byte", Fd !currentLoc.byte);  *)
(* 														 ("mode", Fg ("a")); ("vname", Fe cond_exp)])                                                                                              *)
(* 							in                                                                                                                                                       *)
(*       				let before_instr_stmts =                                                                                                                                 *)
(*       					Formatcil.cStmts print_str (fun _ -> failwith "no new varinfos!")                                                                                      *)
(*       					!currentLoc (args @ !builtin_args)                                                                                                                     *)
(*       				in                                                                                                                                                       *)
(* 							before_instr_stmts                                                                                                                                       *)
(* 						in                                                                                                                                                         *)
(*             (* if stmt has any labels, move them to before stmt, if any *)                                                                                             *)
(*             if (List.length before_instr_stmts) * (List.length stmt.labels) > 0 then                                                                                   *)
(*             (                                                                                                                                                          *)
(*               let first_instr_stmt_skind = (List.hd before_instr_stmts).skind in                                                                                       *)
(*               let orig_skind = stmt.skind in                                                                                                                           *)
(*               stmt.skind <- first_instr_stmt_skind;                                                                                                                    *)
(*               (List.hd before_instr_stmts).skind <- Instr [];                                                                                                          *)
(*               let copy_orig_stmt = Cil.mkStmt orig_skind in                                                                                                            *)
(*               copy_orig_stmt.sid <- stmt.sid;                                                                                                                          *)
(*               l @ before_instr_stmts @ [stmt] @ [copy_orig_stmt]                                                                                                       *)
(*             )                                                                                                                                                          *)
(*             else                                                                                                                                                       *)
(*       				 l @ before_instr_stmts @ [stmt]                                                                                                                         *)
(*   			) [] b.bstmts                                                                                                                                                  *)
(*       in                                                                                                                                                               *)
(*     	b.bstmts <- stmts;                                                                                                                                               *)
(* 			b))                                                                                                                                                              *)
(* end                                                                                                                                                                    *)

(**
  preprocessing IO function signatures and returns required formatargs 
	(for parsing using Formatcil)
  
	vname: target varinfos
		e.g., [ "fclose"; "fflush"; "fopen"; "fprintf"; "stderr"]
*)
let stdio_glob_formatargs () =
	let vnames = ["fclose"; "fflush"; "fopen"; "fprintf"] in   
	let tmp_file = (!Options.marshal_dir ^ "/" ^ "tmp.c") in
	let tmp_preprocessed_file = tmp_file ^ ".i" in 
	let chan = open_out tmp_file in
  Printf.fprintf chan "#include <stdio.h>\n";
  Printf.fprintf chan "int main() { \n";
	Printf.fprintf chan "\t FILE* __file_out;\n ";
	Printf.fprintf chan "\t __file_out = fopen(\"tmp\", \"w\");\n ";
	Printf.fprintf chan "\t fprintf(__file_out, \"hello\");\n ";
	Printf.fprintf chan "\t fflush(__file_out);\n ";
	Printf.fprintf chan "\t fclose(__file_out);\n ";
	Printf.fprintf chan "return 0;}\n"; 
  close_out chan;
	
	(* obtain preprocessed file *)
	if not (Sys.file_exists tmp_preprocessed_file) then 
  	(let stdout_log =
  		let _ = try Unix.unlink tmp_preprocessed_file with _ -> () in
  		Unix.openfile tmp_preprocessed_file [Unix.O_CREAT; Unix.O_WRONLY] 0o640
  	in
  	let _ = Unix.create_process !Options.compile [|!Options.compile; "-E"; tmp_file|] Unix.stdin stdout_log Unix.stderr in
    let _ = match snd (Unix.wait ()) with
      | Unix.WEXITED 0 -> ()
      | _ -> failwith "failed to preprocess tmp.c\n"
    in
  	Unix.close stdout_log);
	let cil = Frontc.parse tmp_preprocessed_file () in 
	(* collect varinfos *)
	let va_table = Hashtbl.create 10 in 
	iterGlobals cil (fun g ->
		match g with
		| GVarDecl(vi,_) | GVar(vi,_,_) when List.mem vi.vname vnames ->   
			let decls = ref [] in
			let visitor = object (self)
				inherit nopCilVisitor
        method private depend t =
          match t with
          | TComp(ci,_) -> decls := GCompTagDecl(ci,locUnknown) :: !decls
          | TEnum(ei,_) -> decls := GEnumTagDecl(ei,locUnknown) :: !decls
          | _ -> ()

        method vtype t =
          match t with
          | TNamed(ti,_) -> 
            ChangeDoChildrenPost(unrollType t, fun t -> self#depend t; t)
          | _ ->
            self#depend t; DoChildren
      end in
      let _ = visitCilGlobal visitor g in
      let decls = g :: !decls in
      Hashtbl.add va_table vi.vname (vi,decls)
    | _ -> ()
  );
	
	(* return formatargs *)
	List.fold_left (fun lst x ->
		try 
			let vi, decls = Hashtbl.find va_table x in
    	(x, Fv vi, decls) :: lst
		with _ -> failwith (Printf.sprintf "not found in va_table %s\n" x);	 
	) [] vnames 



let preprocess cil =
	(* extract global infos necessary for instrumentation *)
	let builtin_globals: (string * Cil.formatArg * Cil.global list) list = stdio_glob_formatargs () in
	(* add builtin globals *)
	let _,globals =
    List.fold_left (fun (set, lst) glob ->
        match glob with
          Cil.GFun(fundec,_) ->
          if (BatSet.mem fundec.svar.vname set) then (set, lst)
          else (BatSet.add fundec.svar.vname set, glob::lst)
        | Cil.GVarDecl(vardecl,_) ->
          if (BatSet.mem vardecl.vname set) then (set, lst)
          else (BatSet.add vardecl.vname set, glob::lst)
        | _ -> (set, glob::lst)
      ) (BatSet.empty,cil.globals) (List.flatten (List.map (fun (_,_,gs) -> gs) builtin_globals))
  in
	cil.globals <- globals; 
	(* extract file type *) 
	iterGlobals cil (fun g ->
  	let visitor = object (self)
  		inherit nopCilVisitor
        method vtype t =
          match t with
          | TNamed (ti,_) -> 
  					if (String.compare ti.tname "FILE") = 0 then file_type := t; DoChildren 
          | _ -> DoChildren
        end 
  	in
    ignore(visitCilGlobal visitor g)
	);
	(* format args for formatcil *)
	let _ = builtin_args := List.map (fun (a,b,_) -> (a,b)) builtin_globals in  
	(* add declarations of local file descriptors (for opening/closing stmts in every function) *)
	let file_ptr_type = TPtr(!file_type, []) in
	Cil.iterGlobals cil (fun g ->
		match g with
		| GFun (fundec, _) ->
			let _ = makeLocalVar fundec ~insert:true outfile_descr file_ptr_type in ()
		| _ -> ()
	);

	cil
