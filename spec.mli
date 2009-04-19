type t
type example
type failure
type expectation_kind = Positive | Negative (* should or should not *)
type result = Ok | Failed of int * int | Pending
type root_spec

val name : t -> string
val example_description : example -> string
val example_result : example -> result
val specs : unit -> root_spec Queue.t
val spec : root_spec -> t
val failures : root_spec -> failure Queue.t
val iter_examples : (example -> unit) -> t -> unit
val iter_subspecs : (t -> unit) -> t -> unit
val num_examples : t -> int
val num_pending : unit -> int
val failure_id : failure -> int
val failure_operation : failure -> string
val failure_expected_result : failure -> string option
val failure_actual_result : failure -> string
val positive_failure : failure -> bool
val start : t -> t option
val remove_spec : t -> t option -> unit
val add_example : example -> unit
val add_failure : string -> string -> string option -> expectation_kind -> unit
val new_spec : string -> t
val new_example : string -> example
val new_pending_example : string -> example
val add_before_each : (unit -> unit) -> unit
val add_after_each : (unit -> unit) -> unit
val add_after_all : (unit -> unit) -> unit
val run_after_each_hooks : unit -> unit
val run_before_each_hooks : unit -> unit
val run_after_all_hooks : unit -> unit
val cleanup : unit -> unit
