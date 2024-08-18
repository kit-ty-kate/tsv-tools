type t

val parse_from_string : string -> t
val parse_from_file : string -> t

val cell_to_string : tab:int -> i:int -> j:int -> t -> string

val number_of_lines : t -> int
val number_of_columns : t -> int
