let print tsv =
  for i = 0 to Tsv.Padded.number_of_lines tsv - 1 do
    for j = 0 to Tsv.Padded.number_of_columns tsv - 1 do
      let {Tsv.Padded.str; padding; last} = Tsv.Padded.get_cell ~i ~j tsv in
      print_string (String.init (CCVector.length str) (fun i -> CCVector.get str i));
      if last then
        print_newline ()
      else begin
        print_string (String.make padding ' ');
        print_string " ";
      end
    done;
  done

let () =
  let tsv = Tsv.parse_from_file Sys.argv.(1) in
  let tsv = Tsv.Padded.create tsv in
  print tsv
