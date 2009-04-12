describe "A list" do
  it "should equal itself when reversed twice" do
    forall (list_of int) (fun l -> (List.rev (List.rev l)) should = l)
  done;

  it "should match head and tail if non-empty" do
    forall (list_of char) ~given:(fun l -> List.length l > 0)
           (fun l -> l should match x::xs)
  done
done;

describe "A bool" do
  it "should be true if all samples are true" do
    forall bool ~given:((=) true) (fun b -> b should = true)
  done
done;

describe "A string" do
  it "should match /^[a-zA-Z0-9]+$/ when alphanumeric" do
    forall (string_of alphanumeric)
           (fun s -> s should match_regexp "^[a-zA-Z0-9]+$")
  done
done
