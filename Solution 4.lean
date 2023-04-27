import Std.Data.List.Basic

def test : String :=
".234.....  2-4
.....678.  6-8

.23......  2-3
...45....  4-5

....567..  5-7
......789  7-9

.2345678.  2-8
..34567..  3-7

.....6...  6-6
...456...  4-6

.23456...  2-6
...45678.  4-8"
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
def dash (s : String) : List String := s.splitOn "."
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

#eval dash (test)
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

#eval List.contSublist [12] [01234] 
#eval List.eitherContSublist [1,2] [0,1,2,3,4] 
#eval List.eitherContSublist [.234.....  2-4] [.....678.  6-8]
#eval List.contSublist (cut test)
#eval List.eitherContSublist [.234.....  2-4] [.....678.  6-8]
#eval List.contSublist (divide (cut test))
#eval List.eitherContSublist (divide (cuttwice test))

def countTrue (l : List Bool) : Nat :=
  l.filter id |>.length

#eval (List.eitherContSublist [1,2] [0,1,2,3,4]).toLBool

def countTrues (s : String) : List Bool :=
  let t := cut s 
  t.map.toBool



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

def twos (s : String) : String :=
  let parts := s.splitOn "\x0d\n"
  String.join [parts.get! 0, parts.get! 1]

def halves (s : String) : String × String :=
  let first := s.data.take (s.length / 2) 
  let second := s.data.drop (s.length / 2)
  (⟨first⟩, ⟨second⟩)

#eval ((cuttwice test).map twos).map halves



