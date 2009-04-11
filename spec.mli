type t
type example
type failure
type expectation_kind = Positive | Negative (* should or should not *)
type result = Ok | Failed of int * int | Pending

val name : t -> string
val example_description : example -> string
val example_result : example -> result
val specs : unit -> t Queue.t
val iter_examples : (example -> unit) -> t -> unit
val iter_failures : (failure -> unit) -> t -> unit
val num_examples : t -> int
val num_failures : t -> int
val num_pending : unit -> int
val failure_id : failure -> int
val failure_operation : failure -> string
val failure_expected_result : failure -> string option
val failure_actual_result : failure -> string
val positive_failure : failure -> bool
val transfer_examples : t -> unit
val transfer_failures : t -> unit
val add_spec : t -> unit
val add_example : example -> unit
val add_failure : string -> string -> string option -> expectation_kind -> unit
val new_spec : string -> t
val new_example : string -> example
val new_pending_example : string -> example
val set_before_each : (unit -> unit) -> unit
val set_after_each : (unit -> unit) -> unit
val set_after_all : (unit -> unit) -> unit
val run_after_each_hook : unit -> unit
val run_before_each_hook : unit -> unit
val run_after_all_hook : unit -> unit
val cleanup : unit -> unit
