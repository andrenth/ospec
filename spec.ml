open Spec_types

type result = Ok | Failed of int | Pending

type example =
  { mutable description : string
  ; results             : result Queue.t
  }

type expectation_kind = Positive | Negative (* should or should not *)

type failure =
  { id        : int
  ; operation : string
  ; result    : string
  ; expected  : string option
  ; kind      : expectation_kind
  }

type t =
  { mutable name : string
  ; examples     : example Queue.t
  ; failures     : failure Queue.t
  }

let spec_queue = Queue.create ()
let example_queue = Queue.create ()
let result_queue = Queue.create ()
let failure_queue = Queue.create ()
let curr_failure_id = ref 1
let before_each = ref (fun () -> ())
let after_each = ref (fun () -> ())
let after_all = ref (fun () -> ())
let total_pending = ref 0

let name spec =
  spec.name

let example_description example =
  example.description

let specs () =
  spec_queue

let failures () =
  failure_queue

let iter_results f example =
  Queue.iter f example.results

let iter_examples f spec =
  Queue.iter f spec.examples

let iter_failures f spec =
  Queue.iter f spec.failures

let num_examples spec =
  Queue.length spec.examples

let num_failures spec =
  Queue.length spec.failures

let num_pending () =
  !total_pending

let failure_id failure =
  failure.id

let failure_operation failure =
  failure.operation

let failure_expected_result failure =
  failure.expected

let failure_actual_result failure =
  failure.result

let positive_failure failure =
  match failure.kind with
  | Positive -> true
  | Negative -> false

let transfer_results example =
  Queue.transfer result_queue example.results

let add_example example =
  Queue.push example example_queue

let add_result result example =
  Queue.push result example.results

let transfer_examples spec =
  Queue.transfer example_queue spec.examples

let transfer_failures spec =
  Queue.transfer failure_queue spec.failures

let add_spec spec =
  Queue.push spec spec_queue

let new_spec name =
  { name = name; examples = Queue.create (); failures = Queue.create () }

let new_example description =
  { description = description; results = Queue.create () }

let incr_failure_id () =
  incr curr_failure_id

let current_failure_id () =
  !curr_failure_id

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
  Queue.clear spec_queue;
  Queue.clear example_queue;
  Queue.clear result_queue;
  Queue.clear failure_queue;
  curr_failure_id := 1;
  total_pending := 0

let add_success () =
  Queue.push Ok result_queue

let add_failure operation result expected kind =
  let id = current_failure_id () in
  let failure =
    { id = id
    ; operation = operation
    ; result    = result
    ; expected  = expected
    ; kind      = kind
    } in
  Queue.push (Failed id) result_queue;
  Queue.push failure failure_queue;
  incr_failure_id ()
