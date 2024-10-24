open Eio.Std
module Process = Eio_linux.Low_level.Process
module Trace = Eio.Private.Trace
module Fd = Eio_unix.Fd
module Rcfd = Eio_unix.Private.Rcfd
module Fork_action = Eio_unix.Private.Fork_action

(* Actions for namespacing *)

module Mount = struct
  module Flags = struct
    include Config.Mount_flags

    let ( + ) = ( lor )
  end

  module Types = struct
    type t = string

    let btrfs = "btrfs"
    let ext4 = "ext4"
    let auto = "auto"
  end
end

external action_mount : unit -> Fork_action.fork_fn = "void_fork_mount"

let action_mount = action_mount ()

let mount ~(src : string) ~(target : string) (type_ : Mount.Types.t) (flags : Mount.Flags.t) =
  Fork_action.
    { run = (fun k -> k (Obj.repr (action_mount, src, target, type_, flags))) }

external action_pivot_root : unit -> Fork_action.fork_fn
  = "void_fork_pivot_root"

let action_pivot_root = action_pivot_root ()

let pivot_root (new_root : string) =
  Fork_action.{ run = (fun k -> k (Obj.repr (action_pivot_root, new_root))) }

module Flags = struct
  include Config.Clone_flags

  let ( + ) = ( lor )
end

external eio_spawn :
  Unix.file_descr ->
  Flags.t ->
  Eio_unix.Private.Fork_action.c_action list ->
  int * Unix.file_descr = "caml_void_clone3"

external pidfd_send_signal : Unix.file_descr -> int -> unit
  = "caml_eio_pidfd_send_signal"

type t = {
  pid : int;
  pid_fd : Fd.t;
  exit_status : Unix.process_status Promise.t;
}

let exit_status t = t.exit_status
let pid t = t.pid

(* Read a (typically short) error message from a child process. *)
let rec read_response fd =
  let buf = Cstruct.create 256 in
  match Eio_linux.Low_level.readv fd [ buf ] with
  | len -> Cstruct.to_string buf ~len ^ read_response fd
  | exception End_of_file -> ""

let signal t signum =
  Fd.use t.pid_fd ~if_closed:Fun.id @@ fun pid_fd ->
  pidfd_send_signal pid_fd signum

let rec waitpid pid =
  match Unix.waitpid [] pid with
  | p, status ->
      assert (p = pid);
      status
  | exception Unix.Unix_error (EINTR, _, _) -> waitpid pid

let void_flags = Flags.(clone_pidfd + clone_newns + clone_newnet)

type empty = [ `Empty ]
type partial = [ `Partial ]
type executable = [ `Executable ]

type config = {
  root : string option;
  mounts : string list;
}

let default_config = { root = None; mounts = [] }

type 'a void =
  | Empty : [> empty] void
  | Partial : config -> [> partial] void
  | Executable : (string * string list) * config -> [> executable ] void

let empty = Empty

let rootfs : string -> [ empty | partial ] void -> partial void = fun s v -> match v with
  | Empty -> Partial ({ root = Some s; mounts = [] })
  | Partial c -> Partial ({ c with root = Some s })

let mount : string -> [ empty | partial ] void -> partial void = fun s v -> match (v :> [ empty | partial] void) with
  | Empty -> Partial { default_config with mounts = [ s ] }
  | Partial c -> Partial { c with mounts = s :: c.mounts }

let exec : string list -> [ empty | partial ] void -> executable void = fun s v -> match v with
  | Empty -> Executable ((List.hd s, s), default_config)
  | Partial c -> Executable ((List.hd s, s), c)

let actions (Executable ((e, args), c) : executable void) : Fork_action.t list =
  let mounts = [] in
  let root = match c.root with
    | None -> pivot_root "tmpfs!"
    | Some root -> pivot_root root
  in
  let e = Process.Fork_action.execve e ~env:[||] ~argv:(Array.of_list args) in
  mounts @ [ root; e ]

let spawn ~sw (e : executable void) =
  Switch.run ~name:"spawn_pipe" @@ fun pipe_sw ->
  let errors_r, errors_w = Eio_linux.Low_level.pipe ~sw:pipe_sw in
  Eio_unix.Private.Fork_action.with_actions (actions e) @@ fun c_actions ->
  Switch.check sw;
  let exit_status, set_exit_status = Promise.create () in
  let t =
    Fd.use_exn "errors-w" errors_w @@ fun errors_w ->
    let pid, pid_fd =
      Eio.Private.Trace.with_span "spawn" @@ fun () ->
      eio_spawn errors_w void_flags c_actions
    in
    let pid_fd = Fd.of_unix ~sw ~seekable:false ~close_unix:true pid_fd in
    { pid; pid_fd; exit_status }
  in
  Fd.close errors_w;
  Fiber.fork_daemon ~sw (fun () ->
      let cleanup () =
        Fd.close t.pid_fd;
        Promise.resolve set_exit_status (waitpid t.pid);
        `Stop_daemon
      in
      match Eio_linux.Low_level.await_readable t.pid_fd with
      | () -> Eio.Cancel.protect cleanup
      | exception Eio.Cancel.Cancelled _ ->
          Eio.Cancel.protect (fun () ->
              signal t Sys.sigkill;
              Eio_linux.Low_level.await_readable t.pid_fd;
              cleanup ()));
  (* Check for errors starting the process. *)
  match read_response errors_r with
  | "" -> t (* Success! Execing the child closed [errors_w] and we got EOF. *)
  | err -> failwith err

let exit_status_to_string = function
  | Unix.WEXITED n -> Printf.sprintf "Exited with %i" n
  | Unix.WSTOPPED n -> Printf.sprintf "Stopped with %i" n
  | Unix.WSIGNALED n -> Printf.sprintf "Signalled with %i" n
