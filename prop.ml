let rec until p f x =
  if p x then x else until p f (f x)

let until_ p f x =
  ignore (until p f x)

let forall gen ?(given = fun _ -> true) f =
  let all_tests_done k = k = 100 in
  let test k =
    let x = gen () in
    if given x then begin f x; succ k end else k in
  until_ all_tests_done test 0
