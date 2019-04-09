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
open Vocab
open BasicDom
open Yojson.Safe
open InterCfg

module G =
struct
  include Graph.Persistent.Digraph.Concrete (BasicDom.Proc)
  module PredHash = Hashtbl.Make(BasicDom.Proc)
  let pred_hash = PredHash.create 10000
  let add_edge g s d =
    let old_pred = try PredHash.find pred_hash d with _ -> PowProc.empty in
    let new_pred = PowProc.add s old_pred in
    PredHash.replace pred_hash d new_pred;
    add_edge g s d
  let fold_pred f g v a =
    let preds = try PredHash.find pred_hash v with _ -> PowProc.empty in
    PowProc.fold f preds a
  let succ g pid = try succ g pid with _ -> []
end
module SCC = Graph.Components.Make (G)
module Oper = Graph.Oper.P(G)
	
module GDom = struct
  module V = G.V
  type t = G.t
  let fromG g = g
  let pred = G.pred
  let succ = G.succ
  let fold_vertex = G.fold_vertex
  let iter_vertex = G.iter_vertex
  let iter_succ = G.iter_succ
  let nb_vertex = G.nb_vertex
  let add_edge g a b = ()
  let create ?(size=1) () = G.empty (*G.create*)
end
module Dom = Graph.Dominator.Make_graph (GDom)

type t = {
  graph : G.t ;
  trans_calls : G.t;
	(* dom : G.V.t -> G.V.t -> bool  *)
}

let empty = {
  graph = G.empty;
  trans_calls = G.empty;
	(* dom = fun x y -> false *)
}

let add_edge : BasicDom.Proc.t -> BasicDom.Proc.t -> t -> t
= fun src dst g ->
  {g with graph = G.add_edge g.graph src dst}

let callees : BasicDom.Proc.t -> t -> PowProc.t
= fun pid g -> G.succ g.graph pid |> PowProc.of_list

let trans_callees : BasicDom.Proc.t -> t -> PowProc.t
= fun pid g -> G.succ g.trans_calls pid |> PowProc.of_list

let is_dom : BasicDom.Proc.t -> BasicDom.Proc.t -> t -> bool
= fun pid_d pid_n g -> false 
(* g.dom pid_d pid_n  *)

let compute_trans_calls : t -> t = fun callgraph ->
  let trans_calls = Oper.transitive_closure callgraph.graph in
  { callgraph with trans_calls = trans_calls }

let compute_dominators : t -> t = fun callgraph -> callgraph
	(* let dom_functions = Dom.compute_all callgraph.graph "main" in  *)
	(* { callgraph with dom = dom_functions.Dom.dom }                 *)

let is_rec : t -> InterCfg.pid -> bool = fun callgraph pid ->
  try
    let trans = G.succ callgraph.trans_calls pid in
    List.mem pid trans
  with _ -> true (* conservative answer for exceptional cases (e.g., unreachable functions) *)

let to_json : t -> json
= fun g ->
  let nodes = `List (G.fold_vertex (fun v nodes ->
            (`String (Proc.to_string v))::nodes) g.graph [])
  in
  let edges = `List (G.fold_edges (fun v1 v2 edges ->
            (`List [`String (Proc.to_string v1);
                    `String (Proc.to_string v2)
                   ])::edges) g.graph []) in
  `Assoc [("nodes", nodes); ("edges", edges)]

let remove_function : InterCfg.pid -> t -> t = fun pid callgraph ->
  {callgraph with graph = G.remove_vertex callgraph.graph pid;
  trans_calls = G.remove_vertex callgraph.trans_calls pid}
