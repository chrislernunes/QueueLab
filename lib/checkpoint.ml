(** Checkpoint is the depth-sync cursor plus the book. Restoring it and
    replaying the exact remaining event suffix reproduces the full-replay hash.
    The buffered-but-not-yet-snapshotted queue is not part of the checkpoint;
    take checkpoints after a snapshot has been applied. *)

let save path sync =
  Event_log.mkdir_p (Filename.dirname path);
  Out_channel.with_open_text path (fun oc ->
      Out_channel.output_string oc (Jsonx.to_string (Depth_sync.to_json sync)))

let load path = Depth_sync.of_json (Jsonx.of_file path)

let roundtrip sync = Depth_sync.of_json (Depth_sync.to_json sync)
