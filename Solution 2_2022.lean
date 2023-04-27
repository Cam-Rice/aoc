/-def path1 : System.FilePath := System.mkFilePath ["input 2"]

#eval path1


def main : IO Unit :=
  IO.println s!"Hello, {hello}!"
-/
def test : String :=
"A Y
B X
C Z"

#eval test

def game (s : String) : List String := s.splitOn "\x0d\n"
def comma : String := ", "
def commas : String := String.intercalate comma (game test)  


#eval game test
#eval commas

--def RPS (s : String) : String := s.splitOn "\x0d\n" sorry

inductive play where 
  | Rock
  | Paper
  | Scissors
  | Null
deriving Repr

def parce : String → (play × play) := by
  intro a
  match a with 
  | "A X" => exact (.Rock, .Rock)
  | "A Y" => exact (.Rock, .Paper)
  | "A Z" => exact (.Rock, .Scissors)
  | "B X" => exact (.Paper, .Rock)
  | "B Y" => exact (.Paper, .Paper)
  | "B Z" => exact (.Paper, .Scissors)
  | "C X" => exact (.Scissors, .Rock)
  | "C Y" => exact (.Scissors, .Paper)
  | "C Z" => exact (.Scissors, .Scissors)
  | _ => exact (.Null, .Null)


#eval (game test).map parce

def score (opp you : play) : Nat :=
  match opp, you with 
  | .Rock, .Rock => 4
  | .Rock, .Paper => 8
  | .Rock, .Scissors => 3
  | .Paper, .Paper => 5
  | .Paper, .Scissors => 9
  | .Paper, .Rock => 1
  | .Scissors, .Scissors => 6
  | .Scissors, .Rock => 7
  | .Scissors, .Paper => 2
  | .Null, .Null => 0
  | .Null, .Scissors => 0
  | .Null, .Paper => 0
  | .Null, .Rock => 0
  | .Scissors, .Null => 0
  | .Paper, .Null => 0
  | .Rock, .Null => 0

#eval ((game test).map parce).map (fun (p,q) => score p q)

def addUp (l : List Nat) : Nat :=
  l.foldl (· + ·) 0

#eval addUp (((game test).map parce).map (fun (p,q) => score p q))
#eval addUp (((game "input2").map parce).map (fun (p,q) => score p q))

def total (s : String) : Nat :=
  let t := game s 
  let l' := t.map parce 
  let k := l'.map (fun (p,q) => score p q)
  addUp k 
  

#eval total test

def path := System.FilePath.mk "C:\\Users\\camri\\aoc\\input2"

def contents : IO Unit := do
  let file ← IO.FS.readFile path
-- IO.println file.data.getLast! 
  IO.println (total file)
  return () 

#eval contents

namespace Part2

inductive play1 where 
  | Rock
  | Paper
  | Scissors
  | Null
  | Lose
  | Draw
  | Win
deriving Repr

-- X = lose ; Y = draw ; Z = win

def parce1 : String → (play1 × play1) := by
  intro a
  match a with 
  | "A X" => exact (.Rock, .Lose)
  | "A Y" => exact (.Rock, .Draw)
  | "A Z" => exact (.Rock, .Win)
  | "B X" => exact (.Paper, .Lose)
  | "B Y" => exact (.Paper, .Draw)
  | "B Z" => exact (.Paper, .Win)
  | "C X" => exact (.Scissors, .Lose)
  | "C Y" => exact (.Scissors, .Draw)
  | "C Z" => exact (.Scissors, .Win)
  | _ => exact (.Null, .Null)

def score1 (opp you : play1) : Nat :=
  match opp, you with 
  | .Rock, .Draw => 4
  | .Rock, .Win => 8
  | .Rock, .Lose => 3
  | .Paper, .Draw => 5
  | .Paper, .Win => 9
  | .Paper, .Lose => 1
  | .Scissors, .Draw => 6
  | .Scissors, .Win => 7
  | .Scissors, .Lose => 2
  | .Null, .Null => 0
  | .Null, .Win => 0
  | .Null, .Draw => 0
  | .Null, .Lose => 0
  | .Win, .Null => 0
  | .Draw, .Null => 0
  | .Lose, .Null => 0
  | .Win, .Rock => 7
  | .Win, .Paper => 8
  | .Win, .Scissors => 9
  | .Lose, .Rock => 1
  | .Lose, .Paper => 2
  | .Lose, .Scissors => 3
  | .Draw, .Rock => 4
  | .Draw, .Paper => 5
  | .Draw, .Scissors => 6
  | play1.Win, play1.Win => 0
  | play1.Win, play1.Draw  => 0
  | play1.Win, play1.Lose => 0
  | play1.Draw, play1.Win => 0
  | play1.Draw, play1.Draw => 0
  | play1.Draw, play1.Lose => 0
  | play1.Lose, play1.Win => 0
  | play1.Lose, play1.Draw => 0
  | play1.Lose, play1.Lose => 0
  | play1.Null, play1.Scissors => 0
  | play1.Null, play1.Paper => 0
  | play1.Null, play1.Rock => 0
  | play1.Scissors, play1.Null => 0
  | play1.Scissors, play1.Scissors => 0
  | play1.Scissors, play1.Paper => 0
  | play1.Scissors, play1.Rock => 0
  | play1.Paper, play1.Null => 0
  | play1.Paper, play1.Scissors => 0
  | play1.Paper, play1.Paper => 0
  | play1.Paper, play1.Rock => 0
  | play1.Rock, play1.Null => 0
  | play1.Rock, play1.Scissors => 0
  | play1.Rock, play1.Paper => 0
  | play1.Rock, play1.Rock => 0

#eval ((game test).map parce1).map (fun (p,q) => score1 p q)

def addUp (l : List Nat) : Nat :=
  l.foldl (· + ·) 0

#eval addUp (((game test).map parce1).map (fun (p,q) => score1 p q))
#eval addUp (((game "input2").map parce1).map (fun (p,q) => score1 p q))

def total1 (s : String) : Nat :=
  let t := game s 
  let l' := t.map parce1
  let k := l'.map (fun (p,q) => score1 p q)
  addUp k 
  

#eval total test

def path := System.FilePath.mk "C:\\Users\\camri\\aoc\\input2"

def contents : IO Unit := do
  let file ← IO.FS.readFile path
-- IO.println file.data.getLast! 
  IO.println (total1 file)
  return () 

#eval contents
/-The score for a single round is the score for the
 shape you selected (1 for Rock, 2 for Paper, and 3
  for Scissors) plus the score for the outcome of the round
   (0 if you lost, 3 if the round was a draw, and 6 if you won). -/

/- A, X = Rock
   B, Y = Paper
   C, Z = Scissors -/


-- example : Type := { Rock : choice // Rock = .A } 
