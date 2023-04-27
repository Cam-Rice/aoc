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

def cut (s : String) : List String := s.splitOn "\x0d\n"

def pair (s : String) : List Nat := 
  let l' := (s.splitOn "\x0d\n")
  l'.map (fun s => s.toNat!)

#eval cut test
#eval (cut test).map pair

-- need to make a list of all the numbers between two inputs.
-- attach each Nat.succ until last number
-- where start of first list is or is greater than start or 2nd lists, record that.
-- same for end of the list, but for the end. Must be both

inductive number where
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

def inputs : String → number := by
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

def value (n : number) : Nat :=
  match n with
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

def divide (l : List a) : List (List a) :=
match l with
| [] => []
| a::as =>
match divide as with
| [] => [[a]]
| l::ls => if l.length = 2 then [a]::(l::ls) else (a::l)::ls

#eval divide (cut test)

--(List.tails l2).filter (fun l => l1.isPrefix l)

def List.contSublist {α : Type} [BEq α] (l₁ l₂ : List α) : Bool :=                                              
let b := l₂.tails.filter (fun l => l₁.isPrefixOf l)|>.isEmpty
!b

def List.eitherContSublist {α : Type} [BEq α] (l₁ l₂ : List α) : Bool :=
l₁.contSublist l₂ || l₂.contSublist l₁

example {α : Type} (a : α) (l: List α) : (a::l).length = 1 + l.length := by 
  match l with 
  | [] => simp  
  | a'::as => 
    simp only [List.length]
    ac_rfl



