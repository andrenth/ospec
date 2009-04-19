type result = Ok | Fail of int * int | Err of int | Pending

type example =
  { mutable description : string
  ; result              : result
  }

type expectation_kind = Positive | Negative (* should or should not *)

type failure =
  { failure_id : int
  ; operation  : string
  ; actual     : string
  ; expected   : string option
  ; kind       : expectation_kind
  }

type error =
  { error_id : int
  ; message  : string
  }

type problem = Failure of failure | Error of error

type t =
  { name                : string
  ; examples            : example Queue.t
  ; subspecs            : t Queue.t
  ; mutable before_each : (unit -> unit) Queue.t 
  ; mutable before_all  : (unit -> unit) Queue.t
  ; mutable after_each  : (unit -> unit) Queue.t
  ; mutable after_all   : (unit -> unit) Queue.t
  }

type root_spec =
  { mutable spec         : t
  ; mutable num_examples : int
  ; mutable num_pending  : int
  ; problems             : problem Queue.t
  }

let spec_stack = Stack.create ()
let spec_queue = Queue.create ()
let curr_example_result = ref Ok
let problem_queue = Queue.create ()
let curr_problem_id = ref 1

let name spec =
  spec.name

let subspecs spec =
  spec.subspecs

let example_description example =
  example.description

let example_result example =
  example.result

let specs () =
  spec_queue

let spec root =
  root.spec

let problems root =
  root.problems

let new_spec name =
  { name = name
  ; examples    = Queue.create ()
  ; subspecs    = Queue.create ()
  ; before_each = Queue.create ()
  ; before_all  = Queue.create ()
  ; after_each  = Queue.create ()
  ; after_all   = Queue.create ()
  }

let new_root_spec spec =
  { spec         = spec
  ; num_examples = 0
  ; num_pending  = 0
  ; problems     = Queue.create ()
  }

let curr_root_spec = ref (new_root_spec (new_spec ""))

let iter_examples f spec =
  Queue.iter f spec.examples

let iter_subspecs f spec =
  Queue.iter f spec.subspecs

let num_examples root =
  root.num_examples

let num_pending root =
  root.num_pending

let failure_id failure =
  failure.failure_id

let failure_operation failure =
  failure.operation

let failure_expected_result failure =
  failure.expected

let failure_actual_result failure =
  failure.actual

let positive_failure failure =
  match failure.kind with
  | Positive -> true
  | Negative -> false

let error_id error =
  error.error_id

let error_message error =
  error.message

let add_example example =
  let spec = Stack.top spec_stack in
  Queue.push example spec.examples

let inherit_hooks spec parent =
  spec.before_each <- Queue.copy parent.before_each;
  spec.before_all  <- Queue.copy parent.before_all;
  spec.after_each  <- Queue.copy parent.after_each;
  spec.after_all   <- Queue.copy parent.after_all

let start spec =
  if Stack.is_empty spec_stack then begin
    curr_root_spec := new_root_spec spec;
    Stack.push spec spec_stack;
    None
  end else begin
    let parent = Stack.top spec_stack in
    inherit_hooks spec parent;
    Stack.push spec spec_stack;
    Some parent
  end

let remove_spec spec parent =
  ignore (Stack.pop spec_stack);
  match parent with
  | Some p ->
      Queue.push spec p.subspecs;
  | None ->
      !curr_root_spec.spec <- spec;
      Queue.push !curr_root_spec spec_queue

let new_example description =
  let ex = { description = description; result = !curr_example_result } in
  !curr_root_spec.num_examples <- !curr_root_spec.num_examples + 1;
  curr_example_result := Ok;
  ex

let new_pending_example description =
  let ex = { description = description; result = Pending } in
  !curr_root_spec.num_pending <- !curr_root_spec.num_pending + 1;
  ex

let incr_problem_id () =
  incr curr_problem_id

let current_problem_id () =
  !curr_problem_id

let add_before_each hook =
  let spec = Stack.top spec_stack in
  Queue.push hook spec.before_each

let run_before_each_hooks () =
  let spec = Stack.top spec_stack in
  Queue.iter (fun f -> f ()) spec.before_each

let add_after_each hook =
  let spec = Stack.top spec_stack in
  Queue.push hook spec.after_each

let run_after_each_hooks () =
  let spec = Stack.top spec_stack in
  Queue.iter (fun f -> f ()) spec.after_each

let add_after_all hook =
  let spec = Stack.top spec_stack in
  Queue.push hook spec.after_all

let run_after_all_hooks () =
  let spec = Stack.top spec_stack in
  Queue.iter (fun f -> f ()) spec.after_all

let cleanup () =
  Queue.clear spec_queue;
  Queue.clear problem_queue;
  curr_example_result := Ok;
  curr_problem_id := 1

let set_example_failure id =
  curr_example_result :=
    match !curr_example_result with
    | Ok -> Fail (id, id)
    | Fail (i, _) -> Fail (i, id)
    | Err _ -> failwith "Shouldn't fail after an error"
    | Pending -> failwith "A Pending example can't fail"

let set_example_error id =
  curr_example_result := Err id

let add_failure operation actual expected kind =
  let id = current_problem_id () in
  let failure =
    { failure_id = id
    ; operation  = operation
    ; actual     = actual
    ; expected   = expected
    ; kind       = kind
    } in
  set_example_failure id;
  Queue.push (Failure failure) !curr_root_spec.problems;
  incr_problem_id ()

let add_error err =
  let id = current_problem_id () in
  let error =
    { error_id = id
    ; message  = err
    } in
  set_example_error id;
  Queue.push (Error error) !curr_root_spec.problems;
  incr_problem_id ()
