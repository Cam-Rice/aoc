import Std.Data.List.Basic


def test : String :=
"vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"

#eval test.data


def cut (s : String) : List String := s.splitOn "\x0d\n"
#eval cut test
#eval (cut test).map fun s => s.data 

def halves (s : String) : String × String :=
  let first := s.data.take (s.length / 2) 
  let second := s.data.drop (s.length / 2)
  (⟨first⟩, ⟨second⟩)

#eval (halves "ghjkoiuytghj")
#eval ((cut test).map halves)
#eval ((cut test).map halves).map (fun (p,q) => List.bagInter p.data q.data)
#eval (List.bagInter "abc".data "cde".data)
-- need to make code that deletes everything up until first non " ," entry

inductive item where
  | a
  | b
  | c
  | d
  | e
  | f
  | g
  | h
  | i
  | j
  | k
  | l
  | m
  | n
  | o
  | p
  | q
  | r
  | s
  | t
  | u
  | v
  | w
  | x
  | y
  | z
  | A
  | B
  | C
  | D
  | E
  | F
  | G
  | H
  | I
  | J
  | K
  | L
  | M
  | N
  | O
  | P
  | Q
  | R
  | S
  | T
  | U
  | V
  | W
  | X
  | Y
  | Z
  | Null
deriving Repr

def inputs : String → item := by
  intro a
  match a with
  | "a" => exact .a
  | "b" => exact .b
  | "c" => exact .c
  | "d" => exact .d
  | "e" => exact .e
  | "f" => exact .f
  | "g" => exact .g
  | "h" => exact .h
  | "i" => exact .i
  | "j" => exact .j
  | "k" => exact .k
  | "l" => exact .l
  | "m" => exact .m
  | "n" => exact .n
  | "o" => exact .o
  | "p" => exact .p
  | "q" => exact .q
  | "r" => exact .r
  | "s" => exact .s
  | "t" => exact .t
  | "u" => exact .u
  | "v" => exact .v
  | "w" => exact .w
  | "x" => exact .x
  | "y" => exact .y
  | "z" => exact .z
  | "A" => exact .A
  | "B" => exact .B
  | "C" => exact .C
  | "D" => exact .D
  | "E" => exact .E
  | "F" => exact .F
  | "G" => exact .G
  | "H" => exact .H
  | "I" => exact .I
  | "J" => exact .J
  | "K" => exact .K
  | "L" => exact .L
  | "M" => exact .M
  | "N" => exact .N
  | "O" => exact .O
  | "P" => exact .P
  | "Q" => exact .Q
  | "R" => exact .R
  | "S" => exact .S
  | "T" => exact .T
  | "U" => exact .U
  | "V" => exact .V
  | "W" => exact .W
  | "X" => exact .X
  | "Y" => exact .Y
  | "Z" => exact .Z
  | _ => exact .Null

def value (i : item): Nat := 
  match i with
  | .a => 1
  | .b => 2
  | .c => 3
  | .d => 4
  | .e => 5
  | .f => 6
  | .g => 7
  | .h => 8
  | .i => 9
  | .j => 10
  | .k => 11
  | .l => 12
  | .m => 13
  | .n => 14
  | .o => 15
  | .p => 16
  | .q => 17
  | .r => 18
  | .s => 19
  | .t => 20
  | .u => 21
  | .v => 22
  | .w => 23
  | .x => 24
  | .y => 25
  | .z => 26
  | .A => 27
  | .B => 28
  | .C => 29
  | .D => 30
  | .E => 31
  | .F => 32
  | .G => 33
  | .H => 34
  | .I => 35
  | .J => 36
  | .K => 37
  | .L => 38
  | .M => 39
  | .N => 40
  | .O => 41
  | .P => 42
  | .Q => 43
  | .R => 44
  | .S => 45
  | .T => 46
  | .U => 47
  | .V => 48
  | .W => 49
  | .X => 50
  | .Y => 51
  | .Z => 52
  | .Null => 0

def repeateditem (s : String) : List String :=
  let t := cut s 
  let l' := t.map halves 
  let k := l'.map (fun (p,q) => List.bagInter p.data q.data)
  k.map fun l => l[0]!.toString

#eval repeateditem test

def addUp (l : List Nat) : Nat :=
  l.foldl (· + ·) 0

def itemtovalue (s : String) : List Nat :=
  let t := repeateditem s
  t.map (fun s => value (inputs s))

#eval addUp (itemtovalue test)

def total (s : String) : Nat :=
  let t := itemtovalue s
  addUp t

#eval total test

def foo {α : Type} [DecidableEq α] (a : α) (as : List α ) : Bool := 
  match as with 
  | [] => false
  | a'::as' => if a == a' then true else foo a as'

def List.dups {α : Type} [DecidableEq α] (as : List α) : List α := 
  as.filter (fun a => foo a as)

def path := System.FilePath.mk "C:\\Users\\camri\\aoc\\input3"

def contents : IO Unit := do
  let file ← IO.FS.readFile path
-- IO.println file.data.getLast! 
  IO.println (total file)
  return () 

#eval contents

-- need function that take finds each successive string and then another function
--      to fine repeated value repeated value
-- need function to take the repeated value and use "value" to assign a value
-- l[0]!

def myHead { α : Type} [Inhabited α] (l : List α) : α := 
  match l with 
  | [] => default 
  | a::_ => a 

/-
namespace Notes

variable {α : Type} (f : α → α → Bool) (r : α → α → Prop) 


inductive Sorted (f : α → α → Bool) : List α → Prop where 
  | nil : Sorted f []
  | single {a : α} : Sorted f [a]
  | longer {a₁ a₂ : α} {as : List α} (h : f a₁ a₂) 
    (h' : Sorted f (a₂ :: as)) : Sorted f (a₁::a₂::as)

open Sorted

example : Sorted (·≤·) [1,2,3] := by 
  apply longer
  · simp 
  · apply longer 
    · simp 
    · apply single

theorem sorted_tail_of_sorted (a : α) (as : List α) 
    (h : Sorted f (a::as)) : Sorted f as := by
  match h with 
  | single => apply nil  
  | longer _ h'' => exact h''


def insert (a : α) (l : List α) : List α :=
  match l with 
  | [] => [a] 
  | a'::as => 
    match f a a' with 
    | true => a::a'::as 
    | false => a'::insert a as 

#check insert 

@[simp]
theorem len_insert_eq_succ_len {a : α} {l : List α} : 
    (insert f a l).length = l.length + 1 := by 
  match l with 
  | [] => simp [insert]
  | a'::as =>
    match h : f a a' with 
    | true => simp [insert, h]
    | false => simp [insert, h]; apply len_insert_eq_succ_len 

def insertSort (l : List α) : List α :=
  match l with 
  | [] => [] 
  | a::as => insert f a <| insertSort as

#check insertSort

#eval insertSort (·≤·) [4,5,2,4,5,6]
#eval insertSort (fun (b b' : Bool) => b && b') [true,false,false]
#eval insertSort (fun _ _ => true) [4,5,2,4,5,6]

class Asymmetric (f : α → α → Bool) where
  asym {a a'} : !f a a' → f a' a

open Asymmetric

def Sorted.cons {a : α} {l : List α} (h₁ : Sorted f l) 
    (h₂ : l.length > 0) (h₃ : f a l[0]) : Sorted f (a::l) :=
  match h₁ with 
  | nil => single
  | single => longer h₃ single
  | longer h h' => longer h₃ <| longer h h'

theorem ordered_of_sorted {a a' : α} {as : List α} 
    (h : Sorted f (a::a'::as)) : f a a' :=
  match h with 
  | longer h' _ => h'

theorem ordered_cons_insert_of_unordered {a a' : α} {as : List α}
    (h : Sorted f (a'::as)) (h' : f a' a) : f a' (insert f a as)[0] :=
  match as with 
  | [] => by simpa [insert]
  | a''::as' => 
  match h'' : f a a'' with 
    | true => by simpa [insert, h'']
    | false => by simp [insert, h'']; apply ordered_of_sorted f h

theorem insert_sorted_of_sorted {a : α} {l : List α} [Asymmetric f] 
    (h : Sorted f l) : Sorted f <| insert f a l :=
  match l with 
  | [] => single
  | a'::as =>
    match h' : f a a' with 
    | true => by simp [insert, h']; apply longer h' h 
    | false => by
      simp [insert, h']
      apply cons f
      · apply insert_sorted_of_sorted <| sorted_tail_of_sorted f a' as h   
      · apply ordered_cons_insert_of_unordered f h 
        · apply asym; simp; assumption

theorem sorted_of_insertSort (l : List α) [Asymmetric f] : 
    Sorted f <| insertSort f l :=
  match l with 
  | [] => nil
  | a::as => by
    dsimp [insertSort]
    apply insert_sorted_of_sorted f <| sorted_of_insertSort as


#eval Sorted [1,2,3]

end Notes
-/


/-def thirdline (s : String) : String × String :=
  let first := 
  let second := 
  let third := 
  (⟨first⟩, ⟨second⟩)
-/
-- List α → List (α × α × α) 
-- ℕ → List α → List (List α)

/-def joinList (a : α) (l1 : List α) (l2 : List α) (l3 : List α) : List α :=
  let t := l1 ++ (l2 ++ l3)
  t.map List a 
-/

def joinListNat (l1 : List Nat) (l2 : List Nat) (l3 : List Nat) : List Nat :=
  l1 ++ l2 ++ l3
def joinListChar (l1 : List Char) (l2 : List Char) (l3 : List Char) : List Char :=
  l1 ++ l2 ++ l3

#eval joinListNat [1,2,3] [2,3] [6,7]

-- def joinListListNat (l : List Nat) : List (List Nat)
--#eval joinList ["vJrwpWtwJgWrhcsFMMfFFhFp", "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", "PmmdzqPrVvPwwTWBwg", "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
--"ttgJtRGJQctTZtZT", "CrZsJsPPZsGzwwsLwLmpwMDw"]

def joinListStrings (list1 : List String) (list2 : List String) (list3 : List String) : List String :=
  list1 ++ (list2 ++ list3)

def divide (l : List a) : List (List a) :=
match l with
| [] => []
| a::as =>
match divide as with
| [] => [[a]]
| l::ls => if l.length = 3 then [a]::(l::ls) else (a::l)::ls



--def StringtoListString (s : String) : List String
#eval List.bagInter (List.bagInter "abc".data "cde".data) "cgh".data
#eval divide (cut test)
#eval (divide ((cut test).map fun s => s.data))
#eval (divide (cut test)).map (fun s => s.toString)
#eval ((divide (cut test)).map fun s => s.toString).map fun s=> s.data
#eval ((cut test).map fun s => s.data).map divide
--#eval (((cut test).map fun s => s.data).map divide).map List.bagInter 

def comma : String := ", "
def commas (ls : List String): String :=
String.intercalate comma ls
#eval divide ((commas (cut test)).toList)

def linebreak : String := "\x0d\n"

def breakLines (s : String) : List String :=
s.splitOn linebreak

def groupByThrees {α : Type} (l : List α) : List (List α) :=
  match l with
  | [] => []
  | s::ss =>
  match groupByThrees ss with
  | [] => [[s]]
  | l::ls => if l.length = 3 then [s]::(l::ls) else (s::l)::ls

def inter (l : List String) : String :=
  match l with
  | [] => ""
  | [s] => s
  | s::ss => ⟨s.data.bagInter (inter ss).data⟩

def commonLetters (s : String) : List String :=
  groupByThrees (breakLines s)|>.map inter

#eval commonLetters test

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
  
#eval contents2
--#eval joinList (cut test)
--#eval joinListStrings (cut test)

def List.intersect {α : Type} [DecidableEq α] : List α → List α → List α
| [], _ => []
| _, [] => []
| (x :: xs), ys => if List.elem x ys then x :: List.intersect xs ys else List.intersect xs ys


def find_common_elements (s1 : String) (s2 : String) (s3 : String) : List Char :=
  let lst1 := s1.toList
  let lst2 := s2.toList
  let lst3 := s3.toList
  List.intersect (List.intersect lst1 lst2) lst3




--#eval findcommonelements ((cut test).map fun s => s.data)

--def thirds (s : String) : String =
  --let first := s.data.take (s.length / 2) 
  --let second := s.data.drop (s.length / 2)