(** {1 Void}

Void is a library to provide {e void processes}.
A void process is an {e empty} process, one in which most global resources have been removed.
As a user, you can add back in precisely those pieces you need for your process.

Void uses Eio's [fork_action]s to provide this mechanism, thus it is only available with Eio.
*)

module Mount : sig
  module Flags : sig
    type t = private int

    val ms_remount : t
    val ms_bind : t
    val ms_shared : t
    val ( + ) : t -> t -> t
  end

  module Types : sig
    type t = private string

    val btrfs : t
    val ext4 : t
    val auto : t
  end
end

val mount :
  src:string ->
  target:string ->
  Mount.Types.t ->
  Mount.Flags.t ->
  Eio_unix.Private.Fork_action.t
(** Mount fork action *)

type t
(** A void process *)

val spawn : sw:Eio.Switch.t -> Eio_unix.Private.Fork_action.t list -> t
val pid : t -> int
val exit_status : t -> Unix.process_status Eio.Promise.t
