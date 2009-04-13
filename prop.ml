let rec until p f x =
  if p x then x else until p f (f x)

let until_ p f x =
  ignore (until p f x)

let for_all gen instances given prop =
  let n =
    match instances with
    | Some x -> x
    | None -> 100 in
  let constr =
    match given with
    | Some f -> f
    | None -> fun _ -> true in
  let all_tests_done k = k = n in
  let test k =
    let x = gen () in
    if constr x then begin prop x; succ k end else k in
  until_ all_tests_done test 0
