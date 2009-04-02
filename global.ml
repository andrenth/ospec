open Spec_types

let specs : spec Queue.t = Queue.create ()
let examples : example Queue.t = Queue.create ()
let results : result Queue.t = Queue.create ()
let failures : failure Queue.t = Queue.create ()
let failure_id = ref 1
let before_each = ref (fun () -> ())
let after_each = ref (fun () -> ())
let after_all = ref (fun () -> ())
let total_pending = ref 0

let new_spec name =
  { name = name; examples = Queue.create () }

let new_example description =
  { description = description; results = Queue.create () }

let incr_failure_id () =
  incr failure_id

let curr_failure_id () =
  !failure_id

let set_before_each hook =
  before_each := hook

let run_before_each_hook () =
  !before_each ()

let set_after_each hook =
  after_each := hook

let run_after_each_hook () =
  !after_each ()

let set_after_all hook =
  after_all := hook

let run_after_all_hook () =
  !after_all ()

let incr_total_pending () =
  incr total_pending

let cleanup () =
  Queue.clear specs;
  Queue.clear examples;
  Queue.clear results;
  Queue.clear failures;
  failure_id := 1;
  total_pending := 0

let add_success () =
  Queue.push Ok results

let add_failure operation result expected kind =
  let id = curr_failure_id () in
  let failure =
    { id = id
    ; operation = operation
    ; result    = result
    ; expected  = expected
    ; kind      = kind
    } in
  Queue.push (Failed id) results;
  Queue.push failure failures;
  incr_failure_id ()
