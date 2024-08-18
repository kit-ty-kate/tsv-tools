type t

val parse_from_string : string -> t
val parse_from_file : string -> t

module Padded : sig
  type cell = {
    str : char CCVector.vector;
    mutable padding : int;
    mutable last : bool;
  }
  type padded

  val create : t -> padded

  val get_cell : int -> int -> padded -> cell

  val number_of_lines : padded -> int
  val number_of_columns : padded -> int

  val insert_row : int -> padded -> unit
end

module Cursor : sig
  type action =
    | Previous
    | Next

  val move : action:action -> sep:int -> int -> int -> Padded.padded -> int
  val get_cell : sep:int -> int -> int -> Padded.padded -> Padded.cell * int
end
