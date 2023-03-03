import «Aoc»

def path1 : System.FilePath := System.mkFilePath ["input"]

#eval path1


def main : IO Unit :=
  IO.println s!"Hello, {hello}!"

def test: String := 
"1000
2000
3000

4000

5000
6000

7000
8000
9000

10000" 

#eval test 


def elves (s : String) : List String := s.splitOn "\x0d\n\x0d\n"

#eval elves test

def calories (s : String) : List Nat := 
  let l' := (s.splitOn "\x0d\n").dropLast
  l'.map (fun s => s.toNat!) 

#eval (elves test).map calories

def addUp (l : List Nat) : Nat :=
  l.foldl (· + ·) 0

#eval addUp [1,2,3,4,5]

#eval (elves test).map calories

def totalCalList (s : String) : List Nat := 
  let t := elves s
  let l' := t.map calories
  l'.map addUp

#eval totalCalList test

def answer (s : String) : Nat :=
  totalCalList s|>.foldl max 0

#eval answer test 

#check IO.FS.readFile

#print System.FilePath

def path := System.FilePath.mk "C:\\Users\\camri\\aoc\\input"

def contents : IO Unit := do
  let file ← IO.FS.readFile path
  IO.println file.data.getLast! 
  IO.println (answer file) 
  return ()

#check Array.insertionSort 

#eval contents
