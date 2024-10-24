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

type t
(** A void process *)

type empty = [ `Empty ]
(** An empty void is exactly that, nothingness *)

type partial = [ `Partial ]
(** A partial void is constructed from an empty or partial void,
    adding mounts, networking, time etc. *)

type executable = [ `Executable ]
(** An executable void is one that is ready to be spawned *)

type _ void
(** A configuration for a Void process *)

val empty : empty void
(** The empty void *)

val mount : string -> [< empty | partial ] void -> partial void
(** Add a mount point *)

val rootfs : string -> [< empty | partial ] void -> partial void
(** Add a new root filesystem *)

val exec : string list -> [< empty | partial ] void -> executable void
(** Make a void configuration ready to be spawned *)

val spawn : sw:Eio.Switch.t -> executable void -> t
(** Spawn an executable void process *)

val pid : t -> int
(** The pid of a running void process *)

val exit_status : t -> Unix.process_status Eio.Promise.t

val exit_status_to_string : Unix.process_status -> string
