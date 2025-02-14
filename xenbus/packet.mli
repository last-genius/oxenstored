type t = {tid: int; rid: int; ty: Op.operation; data: string}

exception Error of string

exception DataError of string

external string_of_header : int -> int -> int -> int -> string
  = "stub_string_of_header"

val create : int -> int -> Op.operation -> string -> t

val of_partialpkt : Partial.pkt -> t

val to_string : t -> string

val unpack : t -> int * int * Op.operation * string

val get_tid : t -> int

val get_ty : t -> Op.operation

val get_data : t -> string

val get_rid : t -> int
