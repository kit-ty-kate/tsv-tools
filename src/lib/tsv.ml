let parse_from_string str =
  let lines = String.split_on_char '\n' str in
  let lines = match List.rev lines with
    | ""::xs -> List.rev xs
    | _ -> lines
  in
  let lines = List.map (fun x -> if String.length x > 0 && x.[String.length x - 1] = '\r' then String.sub x 0 (String.length x - 1) else x) lines in
  let lines = List.map (fun x -> String.split_on_char '\t' x) lines in
  let lines = Array.of_list lines in
  Array.map (fun l -> Array.of_list l) lines

let parse_from_file file =
  let ic = Stdlib.open_in_bin file in
  Fun.protect ~finally:(fun () -> Stdlib.close_in ic) @@ fun () ->
  parse_from_string (Stdlib.In_channel.input_all ic)

let get_cell ~tab ~i ~j array =
  let max_length = Array.fold_left (fun max x -> Int.max max (String.length x.(j))) 0 array in
  let x = array.(i).(j) in
  let length = String.length x in
  let last = Int.equal j (Array.length array.(i) - 1) in
  x ^ String.make (max_length - length + if last then 0 else tab) ' '
