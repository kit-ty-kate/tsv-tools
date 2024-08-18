let print lines =
  for i = 0 to Array.length lines - 1 do
    for j = 0 to Array.length lines.(i) - 1 do
      print_string (Tsv.get_cell ~tab:1 ~i ~j lines);
    done;
    print_newline ();
  done

let () =
  let lines = Tsv.parse_from_file Sys.argv.(1) in
  print lines
