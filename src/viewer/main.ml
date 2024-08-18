let print tsv =
  for i = 0 to Tsv.number_of_lines tsv - 1 do
    for j = 0 to Tsv.number_of_columns tsv - 1 do
      let str, padding = Tsv.cell_to_string ~tab:1 ~i ~j tsv in
      print_string str;
      print_string padding;
    done;
    print_newline ();
  done

let () =
  let tsv = Tsv.parse_from_file Sys.argv.(1) in
  print tsv
