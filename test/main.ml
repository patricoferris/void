open Eio.Std

let root_filesystem =
  "/obuilder-zfs/result/fe532e693c6a86db16b50547aae1345b3515c727b8ed668b3e0c33c1e9a895f9/rootfs"

let () =
  Eio_linux.run @@ fun env ->
  Switch.run @@ fun sw ->
  let open Void in
  let void =
    empty
    (*  |> rootfs ~mode:R root_filesystem
    *)
    |> exec [ "/usr/bin/curl"; "https//ryan.freumh.org" ]
  in
  let t = Void.spawn ~sw ~fs:env#fs void in
  match Promise.await (Void.exit_status t) with
  | Unix.WEXITED 0 -> print_endline "done"
  | Unix.WEXITED n -> Printf.printf "Exited with %i\n%!" n
  | Unix.WSTOPPED n -> Printf.printf "Stopped with %i\n%!" n
  | Unix.WSIGNALED n -> Printf.printf "Signalled with %i\n%!" n
