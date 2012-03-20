let rec until p f x =
  if p x then x else until p f (f x)

let until_ p f x =
  ignore (until p f x)

let from_option z opt =
  match opt with
  | Some x -> x
  | None -> z

let for_all gen instances given prop =
  let n = from_option 100 instances in
  let constr = from_option (fun _ -> true) given in
  let all_tests_done k = k = n in
  let test k =
    let x = gen () in
    if constr x then begin prop x; succ k end else k in
  until_ all_tests_done test 0
