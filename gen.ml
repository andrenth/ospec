type 'a t = unit -> 'a

let bool () =
  Random.bool ()

let float () =
  Random.float 1.0

let int () =
  Random.int max_int

let int_range lb ub () =
  lb + Random.int (ub - lb + 1)

let int32 () =
  Random.int32 Int32.max_int

let int32_range lb ub () =
  Int32.add lb (Random.int32 (Int32.succ (Int32.sub ub lb)))

let int64 () =
  Random.int64 Int64.max_int

let int64_range lb ub () =
  Int64.add lb (Random.int64 (Int64.succ (Int64.sub ub lb)))

let nativeint () =
  Random.nativeint Nativeint.max_int

let nativeint_range lb ub () =
  Nativeint.add lb (Random.nativeint (Nativeint.succ (Nativeint.sub ub lb)))

let char () =
  Char.chr (int_range 0 255 ())

let char_range lc uc () =
  let lb = Char.code lc in
  let ub = Char.code uc in
  Char.chr (int_range lb ub ())

let ascii () =
  Char.chr (int_range 0 127 ())

let digit = char_range '0' '9'
let lowercase = char_range 'a' 'z'
let uppercase = char_range 'A' 'Z'

let alphanumeric () =
  (* 10 digits + 26 uppercase characters + 26 lowercase characters *)
  let r = int_range 1 62 () in
  if r <= 10 then
    Char.chr (r - 1 + Char.code '0')
  else if r <= 36 then
    Char.chr (r - 11 + Char.code 'A')
  else
    Char.chr (r - 37 + Char.code 'a')

let string_of ?(length = int_range 0 100) gen () =
  let n = length () in
  let s = String.create n in
  for i = 0 to pred n do
    s.[i] <- gen ()
  done;
  s

let string ?(length = int_range 0 100) () =
  string_of ~length:length ascii ()

let array_of ?(length = int_range 0 100) gen () =
  let n = length () in
  Array.init n (fun _ -> gen ())

let array ?(length = int_range 0 100) =
  array_of ~length:length (fun () -> ())

let list_of ?(length = int_range 0 100) gen () =
  let n = length () in
  let rec mklist k l =
    if k = n then l else mklist (succ k) (gen () :: l) in
  mklist 0 []

let list ?(length = int_range 0 100) =
  list_of ~length:length (fun () -> ())

let queue_of ?(length = int_range 0 100) gen () =
  let q = Queue.create () in
  let n = length () in
  for i = 1 to n do
    Queue.push (gen ()) q
  done;
  q

let queue ?(length = int_range 0 100) =
  queue_of ~length:length (fun () -> ())

let stack_of ?(length = int_range 0 100) gen () =
  let s = Stack.create () in
  let n = length () in
  for i = 1 to n do
    Stack.push (gen ()) s
  done;
  s

let stack ?(length = int_range 0 100) =
  stack_of ~length:length (fun () -> ())

let hashtbl_of ?(length = int_range 0 100) (kgen, vgen) () =
  let n = length () in
  let h = Hashtbl.create n in
  for i = 1 to n do
    Hashtbl.replace h (kgen ()) (vgen ())
  done;
  h

let () = Random.self_init ()
