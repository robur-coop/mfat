type 'blk t
type entry = { name: string; is_dir: bool; size: int32 }

module type BLOCK = sig
  type t

  val pagesize : t -> int
  val read : t -> src_off:int -> ?dst_off:int -> Bstr.t -> unit
  val write : t -> ?src_off:int -> dst_off:int -> Bstr.t -> unit
end

module Make (Blk : BLOCK) : sig
  val create : Blk.t -> (Blk.t t, [> `Msg of string ]) result
  val ls : Blk.t t -> string -> (entry list, [> `Msg of string ]) result
  val read : Blk.t t -> string -> (string, [> `Msg of string ]) result
  val write : Blk.t t -> string -> string -> (unit, [> `Msg of string ]) result
  val mkdir : Blk.t t -> string -> (unit, [> `Msg of string ]) result
  val remove : Blk.t t -> string -> (unit, [> `Msg of string ]) result
  val exists : Blk.t t -> string -> bool
  val stat : Blk.t t -> string -> (entry, [> `Msg of string ]) result
end
