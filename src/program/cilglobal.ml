open Cil

type t = Cil.global

let tag_of_t = function
| GType (_, _) -> 0
| GCompTag (_, _) -> 1
| GCompTagDecl (_, _) -> 2
| GEnumTag (_, _) -> 3
| GEnumTagDecl (_, _) -> 4
| GVarDecl (_, _) -> 5
| GVar (_, _, _) -> 6
| GFun (_, _) -> 7
| GAsm (_, _) -> 8
| GPragma (_, _) -> 9
| GText _ -> 10

let s_location loc =
let file = try
let idx = String.rindex loc.file '/' in
let len = String.length loc.file in
String.sub loc.file (idx+1) (len-idx-1)
with _ -> loc.file
in file ^ ":" ^ string_of_int loc.line

let get_location = function
| GType (_, l) -> l
| GCompTag (_, l) -> l
| GCompTagDecl (_, l) -> l
| GEnumTag (_, l) -> l
| GEnumTagDecl (_, l) -> l
| GVarDecl (_, l) -> l
| GVar (_, _, l) -> l
| GFun (_, l) -> l
| GAsm (_, l) -> l
| GPragma (_, l) -> l
| GText _ -> let dummy_location = { line = 0; file = ""; byte = 0 } in dummy_location   

let string_of_tag t = (string_of_int (tag_of_t t)) ^ ", " ^ (s_location (get_location t)) 

let compare x y =
let compare_typeinfo x y =
let c1 = compare x.tname y.tname in
if c1 = 0 then compare (Cil.typeSig x.ttype) (Cil.typeSig y.ttype)
else c1
in
match x, y with
| GType (t1, l1), GType (t2, l2) ->
let c = compare_typeinfo t1 t2 in
if c = 0 then compare l1 l2
else c
| GCompTag (c1, l1), GCompTag (c2, l2)
| GCompTagDecl (c1, l1), GCompTagDecl (c2, l2) ->
let c = compare c1.ckey c2.ckey in
if c = 0 then compare l1 l2
else c
| GEnumTag (e1, l1), GEnumTag (e2, l2)
| GEnumTagDecl (e1, l1), GEnumTagDecl (e2, l2) ->
let c = compare e1 e2 in
if c = 0 then compare l1 l2
else c
| GVarDecl (v1 , _), GVarDecl (v2, _)
| GVar (v1, _, _), GVar (v2, _, _) -> compare v1.vid v2.vid
| GFun (f1, _), GFun (f2, _) -> compare f1.svar.vid f2.svar.vid
| GAsm (_, _), GAsm (_, _) | GPragma (_, _), GPragma (_, _)
| GText _, GText _ -> compare x y
| _, _ -> compare (tag_of_t x) (tag_of_t y)

let to_string x =
let s = Cil.d_global () x in
Escape.escape_string (Pretty.sprint 0 s)

module GlobalSet =
struct
include Set.Make (struct type t = Cil.global let compare = compare end)
let to_string set =
(fold (fun global str -> 
	   Printf.sprintf "%s\n  %s," str (to_string global)
	  ) set "\n{") ^ "\n}\n"
end
