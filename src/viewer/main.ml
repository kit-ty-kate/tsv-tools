let print tsv =
  for i = 0 to Tsv.number_of_lines tsv - 1 do
    for j = 0 to Tsv.number_of_columns tsv - 1 do
      print_string (Tsv.cell_to_string ~tab:1 ~i ~j tsv);
    done;
    print_newline ();
  done

let () =
  let tsv = Tsv.parse_from_file Sys.argv.(1) in
  print tsv
