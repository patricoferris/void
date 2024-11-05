open Eio.Std

let ( / ) = Eio.Path.( / )

let copy_busybox ~fs ~proc =
  let busybox =
    Eio.Process.parse_out proc Eio.Buf_read.take_all [ "which"; "busybox" ]
    |> String.trim
  in
  let tmpdir = Filename.temp_dir "void" "busybox" in
  Eio.Path.with_open_in (fs / busybox) @@ fun bbox ->
  Eio.Path.with_open_out ~create:(`If_missing 0o755) (fs / tmpdir / "busybox")
  @@ fun copy ->
  Eio.Flow.copy bbox copy;
  tmpdir

(* This example read-only mounts a copy of busybox
   into the root-filesystem of the process. *)
let () =
  Eio_linux.run @@ fun env ->
  Switch.run @@ fun sw ->
  let fs = env#fs in
  let proc = env#process_mgr in
  let busybox_dir = copy_busybox ~fs ~proc in
  let open Void in
  let void =
    empty
    |> mount ~mode:R ~src:busybox_dir ~tgt:"bin"
    |> exec [ "/bin/busybox"; "ls"; "-la" ]
  in
  let t = Void.spawn ~sw ~fs void in
  let status = Promise.await (Void.exit_status t) in
  Eio.traceln "Status: %s" (Void.exit_status_to_string status)
