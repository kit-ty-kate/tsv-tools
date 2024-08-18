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

let loop ~cursor term tsv =
  Notty_unix.Term.image term (tsv_to_image tsv);
  let rec wait_for_event ~cursor =
    Notty_unix.Term.cursor term (Some cursor);
    match Notty_unix.Term.event term with
    | `End -> ()
    | `Key (`Arrow arrow, []) ->
        let cursor =
          let x, y = cursor in
          match arrow with
          | `Up ->
              let y = Int.max (y - 1) 0 in
              let x = Tsv.Padded.move_cursor ~action:Next ~sep:1 x y tsv in
              (x, y)
          | `Down ->
              let y = Int.min (y + 1) (Tsv.Padded.number_of_lines tsv - 1) in
              let x = Tsv.Padded.move_cursor ~action:Next ~sep:1 x y tsv in
              (x, y)
          | `Left ->
              let x = Tsv.Padded.move_cursor ~action:Previous ~sep:1 (x - 1) y tsv in
              (x, y)
          | `Right ->
              let x = Tsv.Padded.move_cursor ~action:Next ~sep:1 (x + 1) y tsv in
              (x, y)
        in
        wait_for_event ~cursor
    | `Key _ -> (* TODO *) ()
    | `Mouse _ -> assert false
    | `Paste _ -> (* TODO *) ()
    | `Resize _ -> (* TODO *) wait_for_event ~cursor
  in
  wait_for_event ~cursor

let () =
  let term = Notty_unix.Term.create ~mouse:false () in
  let tsv = Tsv.parse_from_file Sys.argv.(1) in
  let tsv = Tsv.Padded.create tsv in
  loop ~cursor:(0, 0) term tsv;
  Notty_unix.Term.release term
