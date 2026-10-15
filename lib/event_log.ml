let read_lines path =
  let ic = In_channel.open_text path in
  Fun.protect ~finally:(fun () -> In_channel.close ic) @@ fun () ->
  let rec loop acc =
    match In_channel.input_line ic with
    | None -> List.rev acc
    | Some line ->
        let line = String.trim line in
        if line = "" || (String.length line >= 1 && line.[0] = '#') then loop acc
        else loop (line :: acc)
  in
  loop []

let read_events path =
  List.mapi
    (fun i line ->
      let ev = Event.of_json (Jsonx.of_string line) in
      if ev.seq_index = 0L && i <> 0 then { ev with seq_index = Int64.of_int i } else ev)
    (read_lines path)

let write_events path events =
  let oc = Out_channel.open_text path in
  Fun.protect ~finally:(fun () -> Out_channel.close oc) @@ fun () ->
  List.iter
    (fun e ->
      Out_channel.output_string oc (Jsonx.to_string (Event.to_json e));
      Out_channel.output_char oc '\n')
    events

let mkdir_p path =
  let rec go p =
    if p = "" || p = "." || p = "/" then ()
    else (
      go (Filename.dirname p);
      if not (Sys.file_exists p) then Unix.mkdir p 0o755)
  in
  go path
