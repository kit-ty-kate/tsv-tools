let tsv_to_image tsv =
  Notty.I.tabulate (Tsv.number_of_lines tsv) (Tsv.number_of_columns tsv) (fun j i ->
    Notty.I.string Notty.A.empty (Tsv.cell_to_string ~tab:1 ~i ~j tsv))

let loop term tsv =
  Notty_unix.Term.image term (tsv_to_image tsv);
  let rec wait_for_event () =
    match Notty_unix.Term.event term with
    | `End -> ()
    | `Key _ -> ()
    | `Mouse _ -> assert false
    | `Paste _ -> ()
    | `Resize _ -> wait_for_event ()
  in
  wait_for_event ()

let () =
  let term = Notty_unix.Term.create ~mouse:false () in
  let tsv = Tsv.parse_from_file Sys.argv.(1) in
  Notty_unix.Term.cursor term (Some (0, 0));
  loop term tsv;
  Notty_unix.Term.release term
