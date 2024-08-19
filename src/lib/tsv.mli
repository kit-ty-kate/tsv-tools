type t

val parse_from_string : string -> t
val parse_from_file : string -> t

val to_string : t -> string

module Str : sig
  type t

  val to_string : t -> string
  val insert : t -> int -> Uchar.t -> unit
  val remove : t -> int -> unit
end

module Padded : sig
  type cell = {
    str : Str.t;
    mutable padding : int;
    mutable last : bool;
  }
  type padded

  val create : t -> padded

  val get_cell : int -> int -> padded -> cell

  val number_of_lines : padded -> int
  val number_of_columns : padded -> int

  val insert_row : int -> padded -> unit

  val recompute_padding : padded -> unit

  val to_tsv : padded -> t
end

module Cursor : sig
  type action =
    | Previous
    | Next

  val move : action:action -> sep:int -> int -> int -> Padded.padded -> int
  val get_cell : sep:int -> int -> int -> Padded.padded -> Padded.cell * int
end
