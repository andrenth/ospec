describe "A list" do
  it "should equal itself when reversed twice" do
    forall list l . (List.rev (List.rev l)) should = l
  done;

  it "should match head and tail if non-empty" do
    forall list l . List.length l > 0 -> l should match x::xs
  done
done;

describe "A bool" do
  it "should be true if all samples are true" do
    forall 42 bool b . b = true -> b should = true
  done
done;

describe "A string" do
  it "should match /^[a-zA-Z0-9]+$/ when alphanumeric" do
    forall (string_of alphanumeric) s .
           String.length s > 0 -> s should match_regexp "^[a-zA-Z0-9]+$"
  done
done
