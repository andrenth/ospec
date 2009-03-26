describe "The number one" do
  it "should equal 2 when added to itself" do
    (1 + 1) should = 2  (* anything 'a -> 'a -> bool should work *)
  done;

  it "should be positive" do
    let positive x = x > 0 in
    1 should be positive  (* 'a -> bool should work too *)
  done;

  it "should be negative when multiplied by -1" do
    let x = 1 * (-1) in
    x should be < 0;      (* "be" is optional *)
    x should not be >= 0
  done;

  it "should raise when divided by 0" do
    (* For exception tests, wrap it in a fun *)
    let f = (fun () -> 1 / 0) in
    f should raise_an_exception;
    f should raise_exception Division_by_zero;
    f should not raise_exception Exit
  done;

  it "should match ^[0-9]+$ when converted to a string" do
    (string_of_int 1) should match_regexp "^[0-9]+$"
  done
done
