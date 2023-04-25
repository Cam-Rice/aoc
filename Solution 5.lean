def test : String :=
"    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
"

#eval test

def cut (s : String) : List String := s.splitOn "\x0d\n"

#eval cut test 
-- define a structure and use Depriving Repr
-- need to make a list of moves. 
-- need to move specific string in list string to specific spot in list string based on moves
-- need to record top string of each list string