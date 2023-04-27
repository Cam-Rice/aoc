import «Aoc»

def path : System.FilePath := System.mkFilePath ["input1_2021"]

#eval path


def main : IO Unit :=
  IO.println s!"Hello, {hello}!"

def test: String := 
"199
200
208
210
200
207
240
269
260
263"

#eval test 

def elves (s : String) : List String := s.splitOn "\x0d\n\x0d\n"

#eval elves test

def calories (s : String) : List Nat := 
  let l' := (s.splitOn "\x0d\n")
  l'.map (fun s => s.toNat!) 

#eval (elves test).map calories 

def increase (l : List Nat) : Nat :=
  match l with
  | [] => 0
  | [_] => 0
  | (a :: b :: as) => (if b > a then 1 else 0) + increase (b :: as)

#eval ((elves test).map calories).map increase 

def total (s : String) : List Nat :=
  let t := elves s 
  let l := t.map calories
  l.map increase

def contents : IO Unit := do
  let file ← IO.FS.readFile path
  --IO.println file.data.getLast! 
  IO.println (total file)
  return ()

def addUp (l : List Nat) : Nat :=
  l.foldl (· + ·) 0

def addUptotal (s : String) : List Nat :=
  let t := elves s 
  let l := t.map calories
  l.map addUp

#eval contents