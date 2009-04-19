describe "The number one" do
  it "should equal 2 when added to itself" do
    (1 + 1) should = 2
  done;

  it "should be positive" do
    let positive x = x > 0 in
    1 should be positive;
    1 should be (fun x -> x > 0);
    1 should be (fun x y -> x > y) 0
  done;

  it "should be negative when multiplied by -1" do
    let x = 1 * (-1) in
    x should be < 0;
    x should not be >= 0
  done;

  it "should raise when divided by 0" do
    let f = (fun () -> 1 / 0) in
    f should raise_an_exception;
    f should raise_exception Division_by_zero;
    f should not raise_exception Exit
  done;

  it "should match ^[0-9]+$ when converted to a string" do
    (string_of_int 1) should match_regexp "^[0-9]+$"
  done;

  it "should be cool"
done
