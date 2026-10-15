(** Pure SHA-256 (FIPS 180-4). Used for dataset manifests so checksums do not
    depend on an external binary. *)

let hex_val c =
  if c >= '0' && c <= '9' then Char.code c - 48
  else if c >= 'a' && c <= 'f' then Char.code c - 87
  else if c >= 'A' && c <= 'F' then Char.code c - 55
  else failwith "sha256: bad hex"

let of_hex s =
  let r = ref 0L in
  String.iter
    (fun c ->
      r := Int64.logor (Int64.shift_left !r 4) (Int64.of_int (hex_val c)))
    s;
  !r

let i32 s = Int64.to_int32 (of_hex s)

let k =
  Array.map i32
    [|
      "428a2f98"; "71374491"; "b5c0fbcf"; "e9b5dba5"; "3956c25b"; "59f111f1";
      "923f82a4"; "ab1c5ed5"; "d807aa98"; "12835b01"; "243185be"; "550c7dc3";
      "72be5d74"; "80deb1fe"; "9bdc06a7"; "c19bf174"; "e49b69c1"; "efbe4786";
      "0fc19dc6"; "240ca1cc"; "2de92c6f"; "4a7484aa"; "5cb0a9dc"; "76f988da";
      "983e5152"; "a831c66d"; "b00327c8"; "bf597fc7"; "c6e00bf3"; "d5a79147";
      "06ca6351"; "14292967"; "27b70a85"; "2e1b2138"; "4d2c6dfc"; "53380d13";
      "650a7354"; "766a0abb"; "81c2c92e"; "92722c85"; "a2bfe8a1"; "a81a664b";
      "c24b8b70"; "c76c51a3"; "d192e819"; "d6990624"; "f40e3585"; "106aa070";
      "19a4c116"; "1e376c08"; "2748774c"; "34b0bcb5"; "391c0cb3"; "4ed8aa4a";
      "5b9cca4f"; "682e6ff3"; "748f82ee"; "78a5636f"; "84c87814"; "8cc70208";
      "90befffa"; "a4506ceb"; "bef9a3f7"; "c67178f2";
    |]

let rotr x n =
  Int32.logor (Int32.shift_right_logical x n) (Int32.shift_left x (32 - n))

let add a b = Int32.add a b

let get32 buf off =
  let b i = Int32.of_int (Char.code (Bytes.get buf (off + i))) in
  Int32.logor (Int32.shift_left (b 0) 24)
    (Int32.logor (Int32.shift_left (b 1) 16)
       (Int32.logor (Int32.shift_left (b 2) 8) (b 3)))

let process h buf off =
  let w = Array.make 64 0l in
  for i = 0 to 15 do
    w.(i) <- get32 buf (off + (4 * i))
  done;
  for i = 16 to 63 do
    let s0 =
      Int32.logxor
        (rotr w.(i - 15) 7)
        (Int32.logxor (rotr w.(i - 15) 18) (Int32.shift_right_logical w.(i - 15) 3))
    in
    let s1 =
      Int32.logxor
        (rotr w.(i - 2) 17)
        (Int32.logxor (rotr w.(i - 2) 19) (Int32.shift_right_logical w.(i - 2) 10))
    in
    w.(i) <- add (add (add w.(i - 16) s0) w.(i - 7)) s1
  done;
  let a = ref h.(0)
  and b = ref h.(1)
  and c = ref h.(2)
  and d = ref h.(3)
  and e = ref h.(4)
  and f = ref h.(5)
  and g = ref h.(6)
  and hh = ref h.(7) in
  for i = 0 to 63 do
    let s1 =
      Int32.logxor (rotr !e 6) (Int32.logxor (rotr !e 11) (rotr !e 25))
    in
    let ch = Int32.logxor (Int32.logand !e !f) (Int32.logand (Int32.lognot !e) !g) in
    let temp1 = add (add (add (add !hh s1) ch) k.(i)) w.(i) in
    let s0 =
      Int32.logxor (rotr !a 2) (Int32.logxor (rotr !a 13) (rotr !a 22))
    in
    let maj =
      Int32.logxor
        (Int32.logand !a !b)
        (Int32.logxor (Int32.logand !a !c) (Int32.logand !b !c))
    in
    let temp2 = add s0 maj in
    hh := !g;
    g := !f;
    f := !e;
    e := add !d temp1;
    d := !c;
    c := !b;
    b := !a;
    a := add temp1 temp2
  done;
  h.(0) <- add h.(0) !a;
  h.(1) <- add h.(1) !b;
  h.(2) <- add h.(2) !c;
  h.(3) <- add h.(3) !d;
  h.(4) <- add h.(4) !e;
  h.(5) <- add h.(5) !f;
  h.(6) <- add h.(6) !g;
  h.(7) <- add h.(7) !hh

let hex_of_bytes bs =
  let hexd = "0123456789abcdef" in
  let n = Bytes.length bs in
  let out = Bytes.create (2 * n) in
  for i = 0 to n - 1 do
    let v = Char.code (Bytes.get bs i) in
    Bytes.set out (2 * i) hexd.[v lsr 4];
    Bytes.set out ((2 * i) + 1) hexd.[v land 15]
  done;
  Bytes.unsafe_to_string out

let hash (s : string) =
  let msg = Bytes.of_string s in
  let len = Bytes.length msg in
  let bit_len = Int64.mul (Int64.of_int len) 8L in
  let with_one = len + 1 in
  let pad_zeros = (56 - (with_one mod 64) + 64) mod 64 in
  let total = with_one + pad_zeros + 8 in
  let buf = Bytes.make total '\000' in
  Bytes.blit msg 0 buf 0 len;
  Bytes.set buf len '\x80';
  for i = 0 to 7 do
    let shift = 8 * (7 - i) in
    let b =
      Int64.to_int (Int64.logand (Int64.shift_right_logical bit_len shift) 255L)
    in
    Bytes.set buf (total - 8 + i) (Char.chr b)
  done;
  let h =
    [|
      i32 "6a09e667"; i32 "bb67ae85"; i32 "3c6ef372"; i32 "a54ff53a";
      i32 "510e527f"; i32 "9b05688c"; i32 "1f83d9ab"; i32 "5be0cd19";
    |]
  in
  let nblocks = total / 64 in
  for i = 0 to nblocks - 1 do
    process h buf (i * 64)
  done;
  let out = Bytes.create 32 in
  for i = 0 to 7 do
    let x = h.(i) in
    for b = 0 to 3 do
      let shift = 8 * (3 - b) in
      let byte = Int32.to_int (Int32.shift_right_logical x shift) land 255 in
      Bytes.set out ((i * 4) + b) (Char.chr byte)
    done
  done;
  hex_of_bytes out

let hash_file path =
  let ic = In_channel.open_bin path in
  Fun.protect
    ~finally:(fun () -> In_channel.close ic)
    (fun () -> hash (In_channel.input_all ic))
