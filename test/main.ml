open Eio.Std

let _root_filesystem =
  "/obuilder-zfs/result/fe532e693c6a86db16b50547aae1345b3515c727b8ed668b3e0c33c1e9a895f9/rootfs"

let () =
  Eio_posix.run @@ fun _ ->
  Switch.run @@ fun sw ->
  let open Void in
  let void =
    empty
    |> mount ~mode:R ~src:"/tmp/test" ~tgt:"bin"
    |> exec [ "/bin/busybox"; "ls" ]
  in
  let t = Void.spawn ~sw void in
  match Promise.await (Void.exit_status t) with
  | Unix.WEXITED 0 -> print_endline "done"
  | Unix.WEXITED n -> Printf.printf "Exited with %i\n%!" n
  | Unix.WSTOPPED n -> Printf.printf "Stopped with %i\n%!" n
  | Unix.WSIGNALED n -> Printf.printf "Signalled with %i\n%!" n
