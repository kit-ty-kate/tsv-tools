open Notty.Infix

let sep = 1

let is_printable_char c =
  let c = Char.code c in
  32 <= c && c <= 126

let tsv_to_image tsv =
  Notty.I.tabulate (Tsv.Padded.number_of_columns tsv) (Tsv.Padded.number_of_lines tsv) (fun x y ->
    let {Tsv.Padded.str; padding; last} = Tsv.Padded.get_cell x y tsv in
    Notty.I.string Notty.A.empty (Tsv.Str.to_string str) <|>
    if last then
      Notty.I.empty
    else
      Notty.I.string Notty.A.empty (String.make (padding + 1) ' '))

let rec loop ~cursor term tsv =
  Notty_unix.Term.image term (tsv_to_image tsv);
  let rec wait_for_event ~cursor =
    Notty_unix.Term.cursor term (Some cursor);
    let insert_char c =
      let x, y = cursor in
      let cell, offset = Tsv.Cursor.get_cell ~sep x y tsv in
      let {Tsv.Padded.str; padding = _; last = _} = cell in
      Tsv.Str.insert str offset c;
      Tsv.Padded.recompute_padding tsv;
      loop ~cursor:(x + 1, y) term tsv
    in
    match Notty_unix.Term.event term with
    | `End -> ()
    | `Key (`ASCII c, []) when is_printable_char c ->
        insert_char (Uchar.of_char c)
    | `Key (`Uchar c, []) ->
        insert_char c
    | `Key (`Backspace, []) ->
        let x, y = cursor in
        let cell, offset = Tsv.Cursor.get_cell ~sep x y tsv in
        if offset = 0 then
          wait_for_event ~cursor
        else
          let {Tsv.Padded.str; padding = _; last = _} = cell in
          Tsv.Str.remove str (offset - 1);
          Tsv.Padded.recompute_padding tsv;
          loop ~cursor:(x - 1, y) term tsv
    | `Key (`Enter, []) ->
        let _x, y = cursor in
        Tsv.Padded.insert_row y tsv;
        loop ~cursor:(0, y + 1) term tsv
    | `Key (`Arrow arrow, []) ->
        let cursor =
          let x, y = cursor in
          match arrow with
          | `Up ->
              let y = Int.max (y - 1) 0 in
              let x = Tsv.Cursor.move ~action:Next ~sep x y tsv in
              (x, y)
          | `Down ->
              let y = Int.min (y + 1) (Tsv.Padded.number_of_lines tsv - 1) in
              let x = Tsv.Cursor.move ~action:Next ~sep x y tsv in
              (x, y)
          | `Left ->
              let x = Tsv.Cursor.move ~action:Previous ~sep (x - 1) y tsv in
              (x, y)
          | `Right ->
              let x = Tsv.Cursor.move ~action:Next ~sep (x + 1) y tsv in
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
