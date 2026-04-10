module Make (Blk : Mfat.BLOCK) : sig
  type blk = Blk.t

  val exists : blk Mfat.t -> Fpath.t -> (bool, 'err) result
  (** [exists t p] is [true] if [p] exists for the file system and [false]
      otherwise. *)

  val must_exist :
    blk Mfat.t -> Fpath.t -> (Fpath.t, [> `Msg of string ]) result
  (** [must_exist p] is [Ok p] if [p] exists for the file system and an error
      otherwise. *)

  val delete :
       blk Mfat.t
    -> ?must_exist:bool
    -> ?recurse:bool
    -> Fpath.t
    -> (unit, [> `Msg of string ]) result
  (** [delete ~must_exist ~recurse p] deletes the path [p]. If [must_exist] is
      [true] (defaults to [false]) an error is returned if [p] doesn't exist. If
      [recurse] is [true] (defaults to [false]) and [p] is a directory, no error
      occurs if the directory is non-empty: its contents is recursively deleted
      first. *)

  val stat : blk Mfat.t -> Fpath.t -> (Mfat.entry, [> `Msg of string ]) result
  (** [stat p] is [p]'s file information. *)

  type 'a error =
       Fpath.t
    -> ('a, [ `Msg of string ]) result
    -> (unit, [ `Msg of string ]) result
  (** The type for managing fold errors.

      During the fold, errors may be generated at different points of the
      process. For example, determining traversal with {!traverse}, determining
      folded {!elements} or trying to [readdir(3)] a directory without having
      permissions.

      These errors are given to a function of this type. If the function returns
      [Error _] the fold stops and returns that error. If the function returns
      [Ok ()] the path is ignored for the operation and the fold continues. *)

  type elements =
    [ `Any
    | `Files
    | `Dirs
    | `Sat of blk Mfat.t -> Fpath.t -> (bool, [ `Msg of string ]) result ]
  (** The type for specifying elements being folded over. *)

  type traverse =
    [ `Any
    | `None
    | `Sat of blk Mfat.t -> Fpath.t -> (bool, [ `Msg of string ]) result ]
  (** The type for controlling directory traversals. *)

  val fold :
       ?err:'a error
    -> ?elements:elements
    -> ?traverse:traverse
    -> blk Mfat.t
    -> (Fpath.t -> 'acc -> 'acc)
    -> 'acc
    -> Fpath.t list
    -> ('acc, [ `Msg of string ]) result
  (** [fold err elements traverse fn acc paths] folds over the list of paths
      [paths] traversing directories according to [traverse] (defaults to
      [`Any]) and selecting elements to fold over according to [elements]
      (defaults to [`Any]).

      [err] manages fold errors (see {!type:error}). *)
end
