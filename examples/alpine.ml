open Eio.Std

let ( / ) = Eio.Path.( / )

let get_alpine_image ~fs ~proc =
  let tmpdir = Filename.temp_dir "void" "alpine" in
  Eio.traceln "Extracting alpine to %s..." tmpdir;
  let container_id =
    Eio.Process.parse_out proc Eio.Buf_read.take_all
      [ "docker"; "run"; "-d"; "alpine" ]
    |> String.trim
  in
  Eio.traceln "Container %s" container_id;
  let () =
    Eio.Process.run proc
      [
        "docker";
        "export";
        container_id;
        "-o";
        Filename.concat tmpdir "alpine.tar.gz";
      ]
  in
  Eio.traceln "Untarring...";
  Eio.Path.mkdir ~perm:0o777 (fs / tmpdir / "rootfs");
  let () =
    Eio.Process.run proc
      [
        "tar";
        "-xf";
        Filename.concat tmpdir "alpine.tar.gz";
        "-C";
        Filename.concat tmpdir "rootfs";
      ]
  in
  Filename.concat tmpdir "rootfs"

(* This example read-only mounts a copy of busybox
   into the root-filesystem of the process. *)
let () =
  Eio_linux.run @@ fun env ->
  Switch.run @@ fun sw ->
  let fs = env#fs in
  let proc = env#process_mgr in
  let alpine_img = get_alpine_image ~fs ~proc in
  let open Void in
  let void = empty |> rootfs ~mode:R alpine_img |> exec [ "/bin/ls"; "-la" ] in
  let t = Void.spawn ~sw void in
  let status = Promise.await (Void.exit_status t) in
  Eio.traceln "Status: %s" (Void.exit_status_to_string status)
