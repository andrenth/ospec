describe "Failures" do
  it "infix failure" do
    1 should != 1
  done;

  it "infix neg failure" do
    1 should not = 1
  done;

  it "ident failure" do
    let greater_than x y = x > y in
    1 should be greater_than 2
  done;

  it "ident neg failure" do
    let greater_than x y = x > y in
    1 should not be greater_than 0
  done;

  it "one arg ident failure" do
    let negative x = x < 0 in
    1 should be negative
  done;

  it "one arg ident neg failure" do
    let positive x = x > 0 in
    1 should not be positive
  done;

  it "fun failure" do
    1 should (fun x y -> x > y) 2
  done;

  it "fun neg failure" do
    1 should not (fun x y -> x < y) 2
  done;
done
