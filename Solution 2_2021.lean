import «Aoc»

def path : System.FilePath := System.mkFilePath ["input1_2021"]

#eval path


def main : IO Unit :=
  IO.println s!"Hello, {hello}!"

def test: String := 
"forward 5
down 5
forward 8
up 3
down 8
forward 2"

#eval test

def cut  (s : String) : List String := s.splitOn "\x0d\n"

#eval cut test

#eval (cut test).map fun s => s.data 

inductive play where 
  | forward
  | backward
  | up
  | down
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

def parce : String → (play × play) := by
  intro a
  match a with 
  | "forward 1" => exact (.forward, .one)
  | "forward 2" => exact (.forward, .two)
  | "forward 3" => exact (.forward, .three)
  | "forward 4" => exact (.forward, .four)
  | "forward 5" => exact (.forward, .five)
  | "forward 6" => exact (.forward, .six)
  | "forward 7" => exact (.forward, .seven)
  | "forward 8" => exact (.forward, .eight)
  | "forward 9" => exact (.forward, .nine)
  | "backward 1" => exact (.backward, .one)
  | "backward 2" => exact (.backward, .two)
  | "backward 3" => exact (.backward, .three)
  | "backward 4" => exact (.backward, .four)
  | "backward 5" => exact (.backward, .five)
  | "backward 6" => exact (.backward, .six)
  | "backward 7" => exact (.backward, .seven)
  | "backward 8" => exact (.backward, .eight)
  | "backward 9" => exact (.backward, .nine)
  | "down 1" => exact (.down, .one)
  | "down 2" => exact (.down, .two)
  | "down 3" => exact (.down, .three)
  | "down 4" => exact (.down, .four)
  | "down 5" => exact (.down, .five)
  | "down 6" => exact (.down, .six)
  | "down 7" => exact (.down, .seven)
  | "down 8" => exact (.down, .eight)
  | "down 9" => exact (.down, .nine)
  | "up 1" => exact (.up, .one)
  | "up 2" => exact (.up, .two)
  | "up 3" => exact (.up, .three)
  | "up 4" => exact (.up, .four)
  | "up 5"=> exact (.up, .five)
  | "up 6" => exact (.up, .six)
  | "up 7" => exact (.up, .seven)
  | "up 8" => exact (.up, .eight)
  | "up 9" => exact (.up, .nine)
  | _ => exact (.Null, .Null)


#eval (cut test).map parce

def score (opp you : play) : Nat :=
  match opp, you with 
  | .forward, .one => 1
  | .forward, .two => 2
  | .forward, .three => 3
  | .forward, .four => 4
  | .forward, .five => 5
  | .forward, .six => 6
  | .forward, .seven => 7
  | .forward, .eight => 8
  | .forward, .nine => 9
  | .backward, .one => 1
  | .backward, .two => 2
  | .backward, .three => 3
  | .backward, .four => 4
  | .backward, .five => 5
  | .backward, .six => 6
  | .backward, .seven => 7
  | .backward, .eight => 8
  | .backward, .nine => 9
  | .down, .one => 1
  | .down, .two => 2
  | .down, .three => 3
  | .down, .four => 4
  | .down, .five => 5
  | .down, .six => 6
  | .down, .seven => 7
  | .down, .eight => 8
  | .down, .nine => 9
  | .up, .one => 1
  | .up, .two => 2
  | .up, .three => 3
  | .up, .four => 4
  | .up, .five => 5
  | .up, .six => 6
  | .up, .seven => 7
  | .up, .eight => 8
  | .up, .nine => 9
  | .Null, .Null => 0
  | play.Null, play.nine => 0
  | play.Null, play.eight => 0
  | play.Null, play.seven => 0
  | play.Null, play.six => 0
  | play.Null, play.five => 0
  | play.Null, play.four => 0
  | play.Null, play.three => 0
  | play.Null, play.two => 0
  | play.Null, play.one => 0
  | play.Null, play.down => 0
  | play.Null, play.up => 0
  | play.Null, play.backward => 0
  | play.Null, play.forward => 0
  | play.nine, play.Null => 0
  | play.nine, play.nine => 0
  | play.nine, play.eight => 0
  | play.nine, play.seven => 0
  | play.nine, play.six => 0
  | play.nine, play.five => 0
  | play.nine, play.four => 0
  | play.nine, play.three => 0
  | play.nine, play.two => 0
  | play.nine, play.one => 0
  | play.nine, play.down => 0
  | play.nine, play.up => 0
  | play.nine, play.backward => 0
  | play.nine, play.forward => 0
  | play.eight, play.Null => 0
  | play.eight, play.nine => 0
  | play.eight, play.eight => 0
  | play.eight, play.seven => 0
  | play.eight, play.six => 0
  | play.eight, play.five => 0
  | play.eight, play.four => 0
  | play.eight, play.three => 0
  | play.eight, play.two => 0
  | play.eight, play.one => 0
  | play.eight, play.down => 0
  | play.eight, play.up => 0
  | play.eight, play.backward => 0
  | play.eight, play.forward => 0
  | play.seven, play.Null => 0
  | play.seven, play.nine => 0
  | play.seven, play.eight => 0
  | play.seven, play.seven => 0
  | play.seven, play.six => 0
  | play.seven, play.five => 0
  | play.seven, play.four => 0
  | play.seven, play.three => 0
  | play.seven, play.two => 0
  | play.seven, play.one => 0
  | play.seven, play.down => 0
  | play.seven, play.up => 0
  | play.seven, play.backward => 0
  | play.seven, play.forward => 0
  | play.six, play.Null => 0
  | play.six, play.nine => 0
  | play.six, play.eight => 0
  | play.six, play.seven => 0
  | play.six, play.six => 0
  | play.six, play.five => 0
  | play.six, play.four => 0
  | play.six, play.three => 0
  | play.six, play.two => 0
  | play.six, play.one => 0
  | play.six, play.down => 0
  | play.six, play.up => 0
  | play.six, play.backward => 0
  | play.six, play.forward => 0
  | play.five, play.Null => 0
  | play.five, play.nine => 0
  | play.five, play.eight => 0
  | play.five, play.seven => 0
  | play.five, play.six => 0
  | play.five, play.five => 0
  | play.five, play.four => 0
  | play.five, play.three => 0
  | play.five, play.two => 0
  | play.five, play.one => 0
  | play.five, play.down => 0
  | play.five, play.up => 0
  | play.five, play.backward => 0
  | play.five, play.forward => 0
  | play.four, play.Null => 0
  | play.four, play.nine => 0
  | play.four, play.eight => 0
  | play.four, play.seven => 0
  | play.four, play.six => 0
  | play.four, play.five => 0
  | play.four, play.four => 0
  | play.four, play.three => 0
  | play.four, play.two => 0
  | play.four, play.one => 0
  | play.four, play.down => 0
  | play.four, play.up => 0
  | play.four, play.backward => 0
  | play.four, play.forward => 0
  | play.three, play.Null => 0
  | play.three, play.nine => 0
  | play.three, play.eight => 0
  | play.three, play.seven => 0
  | play.three, play.six => 0
  | play.three, play.five => 0
  | play.three, play.four => 0
  | play.three, play.three => 0
  | play.three, play.two => 0
  | play.three, play.one => 0
  | play.three, play.down => 0
  | play.three, play.up => 0
  | play.three, play.backward => 0
  | play.three, play.forward => 0
  | play.two, play.Null => 0
  | play.two, play.nine => 0
  | play.two, play.eight => 0
  | play.two, play.seven => 0
  | play.two, play.six => 0
  | play.two, play.five => 0
  | play.two, play.four => 0
  | play.two, play.three => 0
  | play.two, play.two => 0
  | play.two, play.one => 0
  | play.two, play.down => 0
  | play.two, play.up => 0
  | play.two, play.backward => 0
  | play.two, play.forward => 0
  | play.one, play.Null => 0
  | play.one, play.nine => 0
  | play.one, play.eight => 0
  | play.one, play.seven => 0
  | play.one, play.six => 0
  | play.one, play.five => 0
  | play.one, play.four => 0
  | play.one, play.three => 0
  | play.one, play.two => 0
  | play.one, play.one => 0
  | play.one, play.down => 0
  | play.one, play.up => 0
  | play.one, play.backward => 0
  | play.one, play.forward => 0
  | play.down, play.Null => 0
  | play.down, play.down => 0
  | play.down, play.up => 0
  | play.down, play.backward => 0
  | play.down, play.forward => 0
  | play.up, play.Null => 0
  | play.up, play.down => 0
  | play.up, play.up => 0
  | play.up, play.backward => 0
  | play.up, play.forward => 0
  | play.backward, play.Null => 0
  | play.backward, play.down => 0
  | play.backward, play.backward => 0
  | play.backward, play.forward => 0
  | play.forward, play.Null => 0
  | play.forward, play.down => 0
  | play.forward, play.up => 0
  | play.forward, play.backward => 0
  | play.forward, play.forward => 0
  | play.backward, play.up => 0

def addUp (l : List Nat) : Nat :=
  l.foldl (· + ·) 0

#eval ((cut test).map parce).map (fun (p,q) => score p q)
#eval addUp (((cut test).map parce).map (fun (p,q) => score p q))