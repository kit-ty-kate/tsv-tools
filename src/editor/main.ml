open Notty.Infix

let tsv_to_image tsv =
  Notty.I.tabulate (Tsv.Padded.number_of_columns tsv) (Tsv.Padded.number_of_lines tsv) (fun j i ->
    (* TODO: Why is this reversed compared to the viewer? *)
    let {Tsv.Padded.str; padding; last} = Tsv.Padded.get_cell ~i ~j tsv in
    Notty.I.string Notty.A.empty
      (String.init (CCVector.length str) (fun i -> CCVector.get str i)) <|>
    if last then
      Notty.I.empty
    else
      Notty.I.string Notty.A.empty (String.make (padding + 1) ' '))

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
  let tsv = Tsv.Padded.create tsv in
  Notty_unix.Term.cursor term (Some (0, 0));
  loop term tsv;
  Notty_unix.Term.release term
