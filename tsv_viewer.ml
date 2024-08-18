let print lines =
  for i = 0 to Array.length lines - 1 do
    for j = 0 to Array.length lines.(i) - 1 do
      let max_length = Array.fold_left (fun max x -> Int.max max (String.length x.(j))) 0 lines in
      let x = lines.(i).(j) in
      let length = String.length x in
      print_string x;
      let last = Int.equal j (Array.length lines.(i) - 1) in
      print_string (String.make (max_length - length + if last then 0 else 1) ' ');
    done;
    print_newline ();
  done

let () =
  let ic = Stdlib.open_in_bin Sys.argv.(1) in
  let lines = String.split_on_char '\n' (Stdlib.In_channel.input_all ic) in
  let lines = match List.rev lines with
    | ""::xs -> List.rev xs
    | _ -> lines
  in
  let lines = List.map (fun x -> if String.length x > 0 && x.[String.length x - 1] = '\r' then String.sub x 0 (String.length x - 1) else x) lines in
  let lines = List.map (fun x -> String.split_on_char '\t' x) lines in
  let lines = Array.of_list lines in
  let lines = Array.map (fun l -> Array.of_list l) lines in
  print lines
