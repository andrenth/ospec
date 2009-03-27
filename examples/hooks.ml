let h = Queue.create () in

describe "My queue" do
  before all do
    Queue.push "foo" h;
    Queue.push "bar" h
  done;

  before each do
    Queue.push "blah" h
  done;

  after each do
    ignore (Queue.pop h)
  done;

  after all do
    Queue.clear h
  done;

  it "should have three elements" do
    (Queue.length h) should = 3
  done;

  (* The tests below rely on test running order, which you shouldn't do. *)

  it "should have \"bar\" as the first element" do
    (Queue.peek h) should = "bar"
  done;

  it "should only have \"blah\" as elements" do
    Queue.iter (fun s -> s should = "blah") h
  done
done;

describe "My other queue" do
  it "should be empty" do
    h should Queue.is_empty
  done
done
