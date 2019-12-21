let (--) lwr upr =
  let rec aux n acc =
    if n < lwr then acc else aux (n - 1) (n :: acc)
  in
  aux upr []

let head_list l = if l = [] then [] else [List.hd l]

let some = function
  | Some s -> s
  | None -> failwith "Expected optional with Some value"

module Accidental = struct
  type t =
  | Natural
  | Flat
  | Sharp
  [@@deriving show { with_path = false }]
end

module Note = struct
  type t =
  | A
  | Bb
  | B
  | C
  | Db
  | D
  | Eb
  | E
  | F
  | Gb
  | G
  | Ab
  [@@deriving enum, show { with_path = false }]

  let of_enum n = n |> of_enum |> some

  let transpose note = function
  | `Up n   -> to_enum note + n |> of_enum
  | `Down n -> to_enum note - n |> of_enum

end

module Pitch = struct
  open Note
  open Accidental

  type t = Note.t * Accidental.t
  [@@deriving show { with_path = false }]

  let transpose_down = function
    | (n, Natural) -> (n, Flat)
    | (n, Flat)    -> (`Down 1 |> transpose n, Natural)
    | (n, Sharp)   -> (n, Natural)

  let transpose_up = function
    | (n, Natural) -> (n, Sharp)
    | (n, Flat)    -> (n, Natural)
    | (n, Sharp)   -> (`Up 1 |> transpose n, Natural)

  let flip_accidental = function
    | (n, Natural) -> (n, Natural)
    | (n, Flat)    -> (`Down 1 |> transpose n, Sharp)
    | (n, Sharp)   -> (`Up 1 |> transpose n, Flat)

  let transpose pitch = function
    | `Up n   ->
      0 -- n |> List.fold_left (fun acc _ -> transpose_up acc) pitch
    | `Down n ->
      0 -- n |> List.fold_left (fun acc _ -> transpose_down acc) pitch

  let rec normalize pitch =
    match pitch with
    | (note, Natural) -> to_enum note
    | (_, Sharp)      -> transpose_up pitch   |> normalize
    | (_, Flat)       -> transpose_down pitch |> normalize
end

module Interval = struct
  type quality =
  | Minor
  | Major
  | Perfect
  | Augmented
  | Diminished
  [@@deriving show { with_path = false }]

  type simple_number =
  | Unison
  | Second
  | Third
  | Fourth
  | Fifth
  | Sixth
  | Seventh
  | Octave
  [@@deriving enum, show { with_path = false }]

  type complex_number =
  | Ninth
  | Tenth
  | Eleventh
  | Twelfth
  | Thirteenth
  | Fourteenth
  | Fifteenth
  [@@deriving enum, show { with_path = false }]

  type t =
  | Simple  of quality * simple_number
  | Complex of quality * complex_number
  [@@deriving show { with_path = false }]

  let simple_of_complex = function
    | Unison  -> Fifteenth
    | Second  -> Ninth
    | Third   -> Tenth
    | Fourth  -> Eleventh
    | Fifth   -> Twelfth
    | Sixth   -> Thirteenth
    | Seventh -> Fourteenth
    | Octave  -> Fifteenth

  let complex_of_simple = function
    | Ninth       -> Second
    | Tenth       -> Third
    | Eleventh    -> Fourth
    | Twelfth     -> Fifth
    | Thirteenth  -> Sixth
    | Fourteenth  -> Seventh
    | Fifteenth   -> Octave

  let find_interval p1 p2 =
    let open Pitch in
    let e1 = normalize p1 in
    let e2 = normalize p2 in
    match e1 - e2 |> abs with
    | 0   -> Perfect, Unison
    | 1   -> Minor, Second
    | 2   -> Major, Second
    | 3   -> Minor, Third
    | 4   -> Major, Third
    | 5   -> Perfect, Fourth
    | 6   -> Diminished, Fourth
    | 7   -> Perfect, Fifth
    | 8   -> Minor, Sixth
    | 9   -> Major, Sixth
    | 10  -> Minor, Seventh
    | 11  -> Major, Seventh
    | 12  -> Perfect, Octave
    | _   -> failwith "Fatal Error: Interval.find_interval"

  let transpose_simple number = function
    | `Up n   -> simple_number_to_enum number + n |> simple_number_of_enum |> some
    | `Down n -> simple_number_to_enum number - n |> simple_number_of_enum |> some

  let transpose_complex number = function
    | `Up n   -> complex_number_to_enum number + n |> complex_number_of_enum |> some
    | `Down n -> complex_number_to_enum number - n |> complex_number_of_enum |> some

  let flip_quality fn = function
    | Diminished, number -> Augmented,  `Up 1   |> fn number
    | Augmented, number  -> Diminished, `Down 1 |> fn number
    | ow -> ow

  let flip_interval_quality interval =
    match interval with
    | Simple  (quality, number) ->
      let (quality', number') = flip_quality transpose_simple (quality, number) in
      Simple (quality', number')
    | Complex (quality, number) ->
      let (quality', number') = flip_quality transpose_complex (quality, number) in
      Complex (quality', number')

end

module Chord = struct
  type t = Pitch.t list

  let rec get_chord_intervals pitches =
    let open Interval in
    match pitches with
    | []      -> []
    (* No harmonic information *)
    | _ :: [] -> []
    | p1 :: p2 :: tl -> find_interval p1 p2 :: get_chord_intervals (p2 :: tl)

  (* let identify pitches = *)
    (* let intervals = get_chord_intervals pitches in *)
    (* List.iter (fun (q, n) -> Interval.show (Simple (q, n)) |> print_endline) intervals *)
    (* enums = [0; 5; 8] | [5; 8; 0] | etc. (A major triad) *)

end

