type 'blk t
type entry = { name: string; is_dir: bool; size: int32 }

module type BLOCK = sig
  type t

  val pagesize : t -> int
  val read : t -> src_off:int -> ?dst_off:int -> Bstr.t -> unit
  val write : t -> ?src_off:int -> dst_off:int -> Bstr.t -> unit
end

module type S = sig
  type blk

  val format : blk -> total_sectors:int -> unit
  val create : blk -> (blk t, [> `Msg of string ]) result
  val ls : blk t -> string -> (entry list, [> `Msg of string ]) result
  val read : blk t -> string -> (string, [> `Msg of string ]) result
  val to_seq : blk t -> string -> (string Seq.t, [> `Msg of string ]) result
  val write : blk t -> string -> string -> (unit, [> `Msg of string ]) result
  val of_seq : blk t -> string -> string Seq.t -> (unit, [> `Msg of string ]) result
  val mkdir : blk t -> string -> (unit, [> `Msg of string ]) result
  val remove : blk t -> string -> (unit, [> `Msg of string ]) result
  val exists : blk t -> string -> bool
  val stat : blk t -> string -> (entry, [> `Msg of string ]) result
end

module Make (Blk : BLOCK) : S with type blk = Blk.t
