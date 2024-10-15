open Eio.Std

let rootfs =
  "/obuilder-zfs/result/641b9776730f42ea27bb06bdf5a59a3a0da54e7642b74f497323739b3ab3e55e/rootfs"

let () =
  Eio_linux.run @@ fun _env ->
  Switch.run @@ fun sw ->
  let open Void in
  let t =
    Void.spawn ~sw
      [
        mount ~src:rootfs ~target:"/tmp/void-fs" Mount.Types.auto
          Mount.Flags.ms_bind;
        Eio_linux.Low_level.Process.Fork_action.execve "/bin/ls" ~env:[||]
          ~argv:[| "/bin/ls"; "/tmp/void-fs" |];
      ]
  in
  match Promise.await (Void.exit_status t) with
  | Unix.WEXITED 0 -> print_endline "done"
  | Unix.WEXITED n -> Printf.printf "Exited with %i\n%!" n
  | Unix.WSTOPPED n -> Printf.printf "Stopped with %i\n%!" n
  | Unix.WSIGNALED n -> Printf.printf "Signalled with %i\n%!" n
