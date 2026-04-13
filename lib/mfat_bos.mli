module Pat : sig
  type t
  (** The type for named string patterns. *)

  type defs
  (** The type for pattern variable definitions. *)

  val of_string : string -> (t, [> `Msg of string ]) result
  (** [of_string s] parses [s] as a named string pattern. *)

  val v : string -> t
  (** [v s] is like {!of_string} but raises [Invalid_argument] on error. *)

  val to_string : t -> string
  (** [to_string p] is the string representation of [p]. *)

  val matches : t -> string -> bool
  (** [matches p s] is [true] iff [s] matches pattern [p]. *)

  val query : ?init:defs -> t -> string -> defs option
  (** [query ~init p s] matches [s] against [p] and returns the variable
      bindings added to [init]. *)
end

module Make (Blk : Mfat.BLOCK) : sig
  type blk = Blk.t

  val format : blk -> total_sectors:int -> unit
  val create : blk -> (blk Mfat.t, [> `Msg of string ]) result

  (** {1:path Path operations} *)

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
  (** The type for managing fold errors. *)

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
    -> ?dotfiles:bool
    -> ?elements:elements
    -> ?traverse:traverse
    -> blk Mfat.t
    -> (Fpath.t -> 'acc -> 'acc)
    -> 'acc
    -> Fpath.t list
    -> ('acc, [ `Msg of string ]) result
  (** [fold err dotfiles elements traverse fn acc paths] folds over the list of
      paths [paths] traversing directories according to [traverse] (defaults to
      [`Any]) and selecting elements to fold over according to [elements]
      (defaults to [`Any]).

      If [dotfiles] is [false] (default) both elements and directories to
      traverse that start with a [.] are skipped. [.] and [..] entries are
      always skipped.

      [err] manages fold errors (see {!type:error}). *)

  (** {1:pathmatch Matching path patterns against the file system} *)

  val matches :
       blk Mfat.t
    -> ?dotfiles:bool
    -> Fpath.t
    -> (Fpath.t list, [> `Msg of string ]) result
  (** [matches t ~dotfiles pat] is the list of paths in the file system that
      match the path pattern [pat]. If [dotfiles] is [false] (default) elements
      that start with a [.] are skipped. *)

  val query :
       blk Mfat.t
    -> ?dotfiles:bool
    -> ?init:Pat.defs
    -> Fpath.t
    -> ((Fpath.t * Pat.defs) list, [> `Msg of string ]) result
  (** [query t ~dotfiles ~init pat] is like {!matches} except each matching path
      is returned with an environment mapping pattern variables to their matched
      part. *)

  (** {1:file File operations} *)

  module File : sig
    val exists : blk Mfat.t -> Fpath.t -> (bool, 'err) result
    (** [exists t file] is [true] if [file] is a regular file in the file system
        and [false] otherwise. *)

    val must_exist :
      blk Mfat.t -> Fpath.t -> (Fpath.t, [> `Msg of string ]) result
    (** [must_exist t file] is [Ok file] if [file] is a regular file in the file
        system and an error otherwise. *)

    val delete :
         blk Mfat.t
      -> ?must_exist:bool
      -> Fpath.t
      -> (unit, [> `Msg of string ]) result
    (** [delete t ~must_exist file] deletes file [file]. If [must_exist] is
        [true] (defaults to [false]) an error is returned if [file] doesn't
        exist. *)

    val read : blk Mfat.t -> Fpath.t -> (string, [> `Msg of string ]) result
    (** [read t file] is [file]'s content as a string. *)

    val read_lines :
      blk Mfat.t -> Fpath.t -> (string list, [> `Msg of string ]) result
    (** [read_lines t file] is [file]'s content, split at each ['\n'] character.
    *)

    val fold_lines :
         (string -> 'a -> 'a)
      -> 'a
      -> blk Mfat.t
      -> Fpath.t
      -> ('a, [> `Msg of string ]) result
    (** [fold_lines f acc t file] is like
        [List.fold_left f acc (read_lines t file)]. *)

    val write :
      blk Mfat.t -> Fpath.t -> string -> (unit, [> `Msg of string ]) result
    (** [write t file content] outputs [content] to [file]. *)

    val write_lines :
      blk Mfat.t -> Fpath.t -> string list -> (unit, [> `Msg of string ]) result
    (** [write_lines t file lines] writes [lines] joined by ["\n"] to [file]. *)
  end

  (** {1:dir Directory operations} *)

  module Dir : sig
    val exists : blk Mfat.t -> Fpath.t -> (bool, 'err) result
    (** [exists t dir] is [true] if [dir] is a directory in the file system and
        [false] otherwise. *)

    val must_exist :
      blk Mfat.t -> Fpath.t -> (Fpath.t, [> `Msg of string ]) result
    (** [must_exist t dir] is [Ok dir] if [dir] is a directory in the file
        system and an error otherwise. *)

    val create :
      blk Mfat.t -> ?path:bool -> Fpath.t -> (bool, [> `Msg of string ]) result
    (** [create t ~path dir] creates, if needed, the directory [dir]. If [path]
        is [true] (default) intermediate directories are created, otherwise
        missing intermediate directories lead to an error. The result is
        [Ok true] if [dir] did not exist and was created, [Ok false] if [dir]
        already existed as a directory. *)

    val delete :
         blk Mfat.t
      -> ?must_exist:bool
      -> ?recurse:bool
      -> Fpath.t
      -> (unit, [> `Msg of string ]) result
    (** [delete t ~must_exist ~recurse dir] deletes the directory [dir]. If
        [must_exist] is [true] (defaults to [false]) an error is returned if
        [dir] doesn't exist. If [recurse] is [true] (default to [false]) no
        error occurs if the directory is non-empty: its contents is recursively
        deleted first. *)

    val contents :
         blk Mfat.t
      -> ?rel:bool
      -> Fpath.t
      -> (Fpath.t list, [> `Msg of string ]) result
    (** [contents t ~rel dir] is the list of directories and files in [dir]. If
        [rel] is [true] (defaults to [false]) the resulting paths are relative
        to [dir], otherwise they have [dir] prepended. *)

    val fold_contents :
         ?err:'b error
      -> ?dotfiles:bool
      -> ?elements:elements
      -> ?traverse:traverse
      -> blk Mfat.t
      -> (Fpath.t -> 'a -> 'a)
      -> 'a
      -> Fpath.t
      -> ('a, [ `Msg of string ]) result
    (** [fold_contents err dotfiles elements traverse t f acc d] is:
        [contents t d >>= fold err dotfiles elements traverse t f acc]. *)
  end
end
