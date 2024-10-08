type cell = Uchar.t CCVector.vector
type t = cell CCVector.vector CCVector.vector

let parse_from_string str =
  let lines = String.split_on_char '\n' str in
  let lines = match List.rev lines with
    | ""::xs -> List.rev xs
    | _ -> lines
  in
  let lines = List.map (fun x -> if String.length x > 0 && x.[String.length x - 1] = '\r' then String.sub x 0 (String.length x - 1) else x) lines in
  let lines = List.map (fun x -> String.split_on_char '\t' x) lines in
  (* TODO: Transform to UTF-8 before this point *)
  CCVector.of_list (List.map (fun row ->
    CCVector.of_list (List.map (fun cell ->
      let decoder = Uutf.decoder ~encoding:`UTF_8 (`String cell) in
      let str = CCVector.create_with ~capacity:(String.length cell) Uchar.min in
      let rec loop () = match Uutf.decode decoder with
        | `Await | `Malformed _ -> assert false
        | `Uchar c -> CCVector.push str c; loop ()
        | `End -> str
      in
      loop ()) row))
    lines)

let parse_from_file file =
  let ic = Stdlib.open_in_bin file in
  Fun.protect ~finally:(fun () -> Stdlib.close_in ic) @@ fun () ->
  parse_from_string (Stdlib.In_channel.input_all ic)

module Str = struct
  type t = Uchar.t CCVector.vector

  let to_string str =
    let length = CCVector.length str in
    let buf = Buffer.create length in
    let encoder = Uutf.encoder `UTF_8 (`Buffer buf) in
    let rec loop i =
      if (i : int) < length then
        match Uutf.encode encoder (`Uchar (CCVector.get str i)) with
        | `Ok -> loop (i + 1)
        | `Partial -> assert false
      else
        match Uutf.encode encoder `End with
        | `Ok -> Buffer.contents buf
        | `Partial -> assert false
    in
    loop 0

  let insert str i c =
    CCVector.insert str i c

  let remove str i =
    CCVector.remove_and_shift str i
end

let to_string tsv =
  let buf = Buffer.create 512 in
  let len = CCVector.length tsv in
  for y = 0 to len - 1 do
    let row = CCVector.get tsv y in
    let len = CCVector.length row in
    for x = 0 to len - 1 do
      Buffer.add_string buf (Str.to_string (CCVector.get row x));
      if (x : int) < (len - 1 : int) then
        Buffer.add_char buf '\t';
    done;
    (* TODO: Line ending character of TSV files is not defined.
       Maybe we should store the current line endings, or ask the user. *)
    Buffer.add_string buf "\r\n";
  done;
  Buffer.contents buf

module Padded = struct
  type cell = {
    str : Str.t;
    mutable padding : int;
    mutable last : bool;
  }
  type padded = cell CCVector.vector CCVector.vector

  let compute_padding get_length cell x tsv f =
    let max_length =
      CCVector.fold (fun max cell -> Int.max max (get_length (CCVector.get cell x))) 0 tsv
    in
    let length = get_length cell in
    f (max_length - length)

  let recompute_padding tsv =
    CCVector.iter (fun row ->
      CCVector.iteri (fun x cell ->
        let get_length cell = CCVector.length cell.str in
        compute_padding get_length cell x tsv (fun padding ->
          cell.padding <- padding))
        row)
      tsv

  let create tsv =
    CCVector.map (fun row ->
      CCVector.mapi (fun x cell ->
        compute_padding CCVector.length cell x tsv (fun padding ->
          {
            str = cell;
            padding;
            last = Int.equal x (CCVector.length row - 1);
          }))
        row)
      tsv

  let get_cell x y padded_tsv =
    CCVector.get (CCVector.get padded_tsv y) x

  let number_of_lines padded_tsv = CCVector.length padded_tsv
  let number_of_columns padded_tsv = CCVector.length (CCVector.get padded_tsv 0)

  let insert_row y padded_tsv =
    CCVector.insert padded_tsv (y + 1)
      (CCVector.init (number_of_columns padded_tsv)
         (fun x ->
            let {str; padding; last} = get_cell x y padded_tsv in
            {
              str = CCVector.create ();
              padding = CCVector.length str + padding;
              last;
            }))

  let to_tsv tsv =
    CCVector.map (fun row ->
      CCVector.map (fun cell -> cell.str) row)
      tsv
end

module Cursor = struct
  type action =
    | Previous
    | Next

  let move ~action ~sep x y padded_tsv =
    let line = CCVector.get padded_tsv y in
    let length = CCVector.length line in
    let rec loop size i =
      if (i : int) < (length : int) then
        let {Padded.str; padding; last} = CCVector.get line i in
        match action with
        | Previous ->
            if (x : int) <= size + CCVector.length str then
              Int.max x 0
            else
              let next_size = size + CCVector.length str + if last then 0 else padding + sep in
              if (x : int) <= (next_size : int) then
                size + CCVector.length str
              else
                loop next_size (i + 1)
        | Next ->
            if (x : int) <= (size : int) then
              size
            else if (x : int) <= size + CCVector.length str then
              Int.max x 0
            else
              let next_size = size + CCVector.length str + if last then 0 else padding + sep in
              loop next_size (i + 1)
      else
        size
    in
    loop 0 0

  let get_cell ~sep x y tsv =
    let line = CCVector.get tsv y in
    let length = CCVector.length line in
    let rec loop size i =
      if (i : int) < (length : int) then
        let {Padded.str; padding; last} as cell = CCVector.get line i in
        if (x : int) <= size + CCVector.length str then
          (cell, x - size)
        else
          let next_size = size + CCVector.length str + if last then 0 else padding + sep in
          loop next_size (i + 1)
      else
        raise Not_found
    in
    loop 0 0
end
