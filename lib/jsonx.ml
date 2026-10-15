(** Minimal helpers around [Yojson.Safe]. Integers that may exceed OCaml [int]
    are written as JSON numbers via [`Intlit], never as floats. *)

type json = Yojson.Safe.t

let str s = `String s
let bool b = `Bool b
let int n = `Int n
let i64 n = `Intlit (Int64.to_string n)
let list xs = `List xs
let obj xs = `Assoc xs
let float x = `Float x
let null = `Null

let to_string j = Yojson.Safe.to_string j
let to_string_pretty j = Yojson.Safe.pretty_to_string j
let of_string s = Yojson.Safe.from_string s
let of_file p = Yojson.Safe.from_file p

let member name = function
  | `Assoc xs -> (try Some (List.assoc name xs) with Not_found -> None)
  | _ -> None

let require name j =
  match member name j with
  | Some v -> v
  | None -> failwith ("missing JSON field " ^ name)

let i64_of = function
  | `Intlit s -> Int64.of_string s
  | `Int n -> Int64.of_int n
  | `String s -> Int64.of_string s
  | `Float _ -> failwith "refusing to coerce a JSON float to int64"
  | j -> failwith ("expected int64, got " ^ Yojson.Safe.to_string j)

let string_of = function
  | `String s -> s
  | j -> failwith ("expected string, got " ^ Yojson.Safe.to_string j)

let bool_of = function
  | `Bool b -> b
  | j -> failwith ("expected bool, got " ^ Yojson.Safe.to_string j)

let list_of = function
  | `List xs -> xs
  | j -> failwith ("expected list, got " ^ Yojson.Safe.to_string j)

let assoc_of = function
  | `Assoc xs -> xs
  | j -> failwith ("expected object, got " ^ Yojson.Safe.to_string j)

let opt f name j =
  match member name j with
  | None | Some `Null -> None
  | Some v -> Some (f v)
