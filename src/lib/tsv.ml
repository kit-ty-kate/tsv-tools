(* TODO: use Uchar instead *)
type cell = char CCVector.vector
type t = cell CCVector.vector CCVector.vector

let parse_from_string str =
  let lines = String.split_on_char '\n' str in
  let lines = match List.rev lines with
    | ""::xs -> List.rev xs
    | _ -> lines
  in
  let lines = List.map (fun x -> if String.length x > 0 && x.[String.length x - 1] = '\r' then String.sub x 0 (String.length x - 1) else x) lines in
  let lines = List.map (fun x -> String.split_on_char '\t' x) lines in
  CCVector.of_list (List.map (fun row ->
    CCVector.of_list (List.map (fun cell ->
      CCVector.of_seq (String.to_seq cell)) row))
    lines)

let parse_from_file file =
  let ic = Stdlib.open_in_bin file in
  Fun.protect ~finally:(fun () -> Stdlib.close_in ic) @@ fun () ->
  parse_from_string (Stdlib.In_channel.input_all ic)

module Padded = struct
  type cell = {
    str : char CCVector.vector;
    mutable padding : int;
    mutable last : bool;
  }
  type padded = cell CCVector.vector CCVector.vector

  let create tsv =
    CCVector.map (fun row ->
      CCVector.mapi (fun x cell ->
        let max_length =
          CCVector.fold (fun max cell -> Int.max max (CCVector.length (CCVector.get cell x))) 0 tsv
        in
        let length = CCVector.length cell in
        {
          str = cell;
          padding = max_length - length;
          last = Int.equal x (CCVector.length row - 1);
        })
        row)
      tsv

  let get_cell ~i ~j padded_tsv =
    CCVector.get (CCVector.get padded_tsv i) j

  let number_of_lines padded_tsv = CCVector.length padded_tsv
  let number_of_columns padded_tsv = CCVector.length (CCVector.get padded_tsv 0)
end
