import Std.Data.List.Basic

def test : String :=
"2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"
-- only need to know values between spaces 11 and 15 (11,14]

#eval test

inductive item where
  | one
  | two
  | three
  | four
  | five
  | six
  | seven
  | eight
  | nine
  | Null 
deriving Repr

def inputs : String → item := by
  intro a
  match a with
  | "1" => exact .one
  | "2" => exact .two
  | "3" => exact .three
  | "4" => exact .four
  | "5" => exact .five
  | "6" => exact .six
  | "7" => exact .seven
  | "8" => exact .eight
  | "9" => exact .nine
  | _ => exact .Null

def value (i : item) : Nat :=
  match i with
  | .one => 1
  | .two => 2
  | .three => 3
  | .four => 4
  | .five => 5
  | .six => 6
  | .seven => 7
  | .eight => 8
  | .nine => 9
  | .Null => 0

def cut (s : String) : List String := s.splitOn "\x0d\n"
def cuttwice (s : String) : List String := s.splitOn "\x0d\n\x0d\n"
def dash (s : String) : List String := s.splitOn "-"
def divide (l : List a) : List (List a) :=
match l with
| [] => []
| a::as =>
match divide as with
| [] => [[a]]
| l::ls => if l.length = 1 then [a]::(l::ls) else (a::l)::ls

def calories (s : String) : List Nat := 
  let l' := (s.splitOn "\x0d\n")
  l'.map (fun s => s.toNat!) 

def comma (s : String) : String × String := 
  let l' := (s.splitOn ",")
  l'.map (fun s => s.toNat!) 

def twos (s : String) : String :=
  let parts := s.splitOn "\x0d\n"
  String.join [parts.get! 0, parts.get! 1]

def halves (s : String) : String × String :=
  let first := s.data.take (s.length / 2) 
  let second := s.data.drop (s.length / 2)
  (⟨first⟩, ⟨second⟩)

#eval ((cuttwice test).map twos).map halves

#eval test
#eval (dash test)
#eval dash "1-2, 3-4"
#eval (dash test).map calories
#eval cut test
#eval (cuttwice test)
#eval (cuttwice test).map calories
#eval divide (cut test)


-- need to make a list of all the numbers between two inputs.
-- attach each Nat.succ until last number
-- where start of first list is or is greater than start or 2nd lists, record that.
-- same for end of the list, but for the end. Must be both





def repeateditem (s : String) : List String :=
  let k := cuttwice s 
  let t := k.map twos
  let l' := t.map halves 
  let k := l'.map (fun (p,q) => List.bagInter p.data q.data)
  k.map fun l => l[0]!.toString

#eval repeateditem test

def List.contSublist {α : Type} [BEq α] (l₁ l₂ : List α) : Bool :=
  let b := l₂.tails.filter (fun l => l₁.isPrefixOf l)|>.isEmpty
  !b

def List.eitherContSublist {α : Type} [BEq α] (l₁ l₂ : List α) : Bool :=
  l₁.contSublist l₂ || l₂.contSublist l₁

#eval List.contSublist [1,2] [0,1,2,3,4] 
#eval List.eitherContSublist [1,2] [0,1,2,3,4] 


structure Interval where
  startpt : Nat
  endpt : Nat
deriving Inhabited, Repr

def parse (s : String) : Interval :=
  let l := s.splitOn "-"
  ⟨l[0]!.toNat!,l[1]!.toNat!⟩


def Interval.contain (i₁ i₂ : Interval) : Bool :=
  i₁.startpt ≥ i₂.startpt && i₁.endpt ≤ i₂.endpt

def Interval.disjoint (i₁ i₂ : Interval) : Bool :=
  i₁.startpt > i₂.endpt || i₁.endpt < i₂.startpt

def containedIntervals (s : List String) : List Bool :=
  let intervals := s.map parse
  intervals.map (fun i₁ => intervals.any (fun i₂ => i₁ ≠ i₂ && i₁.contain i₂))

def Int (i₁ i₂ : Interval) : Bool

#eval test
#eval parse test
#eval parseNat "2-4"
#eval Interval.contain (parse test)
#eval (parse test).map Interval.contain
#eval Interval.disjoint (parse test)


def countTrue (l : List Bool) : Nat :=
  l.filter id |>.length

#eval (List.eitherContSublist [1,2] [0,1,2,3,4]).toLBool

def itemtovalue2 (s : String) : List Nat :=
  let t := commonLetters s
  t.map (fun s => value (inputs s))

#eval addUp (itemtovalue2 test)

def total2 (s : String) : Nat :=
  let t := itemtovalue2 s
  addUp t

def contents2 : IO Unit := do
  let file ← IO.FS.readFile path
-- IO.println file.data.getLast! 
  IO.println (total2 file)
  return () 



--stuff that doesn't work




