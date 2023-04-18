def path1 : System.FilePath := System.mkFilePath ["input 2"]

#eval path1


def main : IO Unit :=
  IO.println s!"Hello, {hello}!"

def test : String :=
"A Y
B X
C Z"

#eval test

def game (s : String) : List String := s.splitOn "\x0d\n" 

def games (s : String) : List Nat := 
  let l' := (s.splitOn "\x0d\n")
  l'.map (fun s => s.toType)

#eval game test 

inductive play where 
  | Rock
  | Paper
  | Scissors

def parce : String → (play × play) := by
  intro s
  match s with 
  | "A X" => exact (.Rock, .Rock)
  | "A Y" => exact (.Rock, .Paper)
  | "A Z" => exact (.Rock, .Scissors)
  | "B X" => exact (.Paper, .Rock)
  | "B Y" => exact (.Paper, .Paper)
  | "B Z" => exact (.Paper, .Scissors)
  | "C X" => exact (.Scissors, .Rock)
  | "C Y" => exact (.Scissors, .Paper)
  | "C Z" => exact (.Scissors, .Scissors)
  | " s " => exact []

#eval parce (game test)

def score (opp you : play) : Nat :=
  match opp, you with 
  | .Rock, .Rock => 4
  | .Rock, .Paper => 1
  | .Rock, .Scissors => 7
  | .Paper, .Paper => 5
  | .Paper, .Scissors => 2
  | .Paper, .Rock => 8
  | .Scissors, .Scissors => 6
  | .Scissors, .Rock => 3
  | .Scissors, .Paper => 9


def totalCalList (s : String) : List Nat := 
  let t := game s
  let l' := t.map calories
  l'.map addUp
  sorry

/-The score for a single round is the score for the
 shape you selected (1 for Rock, 2 for Paper, and 3
  for Scissors) plus the score for the outcome of the round
   (0 if you lost, 3 if the round was a draw, and 6 if you won). -/

/- A, X = Rock
   B, Y = Paper
   C, Z = Scissors -/

/- def contents : IO Unit := do
  let file ← IO.FS.readFile path
  IO.println file.data.getLast! 
  IO.println (answer file)
  return () -/

-- example : Type := { Rock : choice // Rock = .A } 

/-def addUp (l : List Nat) : Nat := where
  l.foldl (· + ·) 0


#eval addUp [1,2,3] -/

