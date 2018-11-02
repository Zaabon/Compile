(* Copyright Per Lindgren 2016-2018, see the file "LICENSE" *)
(* for the full license governing this code.                *)

(* Associative list for identifiers *)
open Common
open State__State

(* map identifier string to id *)
type id_list  = (string * id) list
(* map id to string *)
type id_list' = (id * string) list

(* stateful represenation of id storage *)
let id_nr = ref (Z.of_int 0)
let id_map  = ref ([]:id_list)
let id_map' = ref ([]:id_list')

let add_id (id:string) : id =
  try
    List.assoc id !id_map
  with
  | _ -> (* lookup failed, generate new id *)
    id_nr := Z.add !id_nr (Z.of_int 1);
    id_map := (id, Id !id_nr) :: !id_map;
    id_map' := (Id !id_nr, id) :: !id_map';
    Id !id_nr

let get_id (id : id) : string =
  List.assoc id (!id_map')

let to_string (m : state) =
  let to_str (Id n, s) =
    "Id #" ^ Z.to_string n ^ " " ^ ec ^ s ^ ec ^ " = " ^ Z.to_string (get m (Id n))
  in
  "State:" ^ nl ^ String.concat nl (List.map to_str (List.rev !id_map'))







