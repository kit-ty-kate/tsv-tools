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

  val get_cell : i:int -> j:int -> padded -> cell

  val number_of_lines : padded -> int
  val number_of_columns : padded -> int
end
