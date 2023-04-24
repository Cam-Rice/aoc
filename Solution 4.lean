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

#eval test

def cut (s : String) : List String := s.splitOn "\x0d\n"

#eval cut test 

-- need to make a list of all the numbers between two inputs.
-- attach each Nat.succ until last number
-- where start of first list is or is greater than start or 2nd lists, record that.
-- same for end of the list, but for the end. Must be both

inductive Nats
| zero : Nats
| succ (n : Nat) : Nats
def n : Nat := Nat.succ (Nat.succ Nat.zero)
def add (m n : Nat) : Nat :=
  match m with
  | Nat.zero => n
  | Nat.succ m' => Nat.succ (add m' n) 



inductive number where
  | One : 1














example {α : Type} (a : α) (l: List α) : (a::l).length = 1 + l.length := by 
  match l with 
  | [] => simp  
  | a'::as => 
    simp only [List.length]
    ac_rfl



