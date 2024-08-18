let array_to_image array =
  Notty.I.tabulate (Array.length array.(0)) (Array.length array) (fun j i ->
    Notty.I.string Notty.A.empty (Tsv.get_cell ~tab:1 ~i ~j array))

let loop term array =
  Notty_unix.Term.image term (array_to_image array);
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
  let array = Tsv.parse_from_file Sys.argv.(1) in
  Notty_unix.Term.cursor term (Some (0, 0));
  loop term array;
  Notty_unix.Term.release term
