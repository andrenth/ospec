type result = Ok | Failed of int | Pending

type example =
  { mutable description : string
  ; results             : result Queue.t
  }

type spec =
  { mutable name : string
  ; examples     : example Queue.t
  }

type expectation_kind = Positive | Negative (* should or should not *)

type failure =
  { id        : int
  ; operation : string
  ; result    : string
  ; expected  : string option
  ; kind      : expectation_kind
  }
