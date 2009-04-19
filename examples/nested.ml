type person = { name : string; age : int };;

let p = { name = "John Doe"; age = 42 } in

describe "A person" do
  it "should be cool";

  describe "name" do
    it "should be valid" do
      (p.name) should match_regexp "^[A-Za-z- ]+$"
    done;

    it "should be long enough" do
      (String.length p.name) should be > 0
    done
  done;

  describe "age" do
    it "should be positive" do
      (p.age) should be > 0
    done;

    it "should reasonable" do
      (p.age) should be < 130
    done
  done;
done
