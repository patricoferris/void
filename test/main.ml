open Eio.Std

let rootfs =
  "/obuilder-zfs/result/fe532e693c6a86db16b50547aae1345b3515c727b8ed668b3e0c33c1e9a895f9/rootfs"

let () =
  Eio_linux.run @@ fun _env ->
  Switch.run @@ fun sw ->
  let open Void in
  let t =
    Void.spawn ~sw
      [
        pivot_root rootfs;
        Eio_linux.Low_level.Process.Fork_action.chdir "/";
        Eio_linux.Low_level.Process.Fork_action.execve "/bin/ls" ~env:[||]
          ~argv:[| "/bin/ls"; "-la"; "/home" |];
      ]
  in
  match Promise.await (Void.exit_status t) with
  | Unix.WEXITED 0 -> print_endline "done"
  | Unix.WEXITED n -> Printf.printf "Exited with %i\n%!" n
  | Unix.WSTOPPED n -> Printf.printf "Stopped with %i\n%!" n
  | Unix.WSIGNALED n -> Printf.printf "Signalled with %i\n%!" n
