import Lake
open Lake DSL

package «aoc» {
  -- add package configuration options here
}

lean_lib «Aoc» {
  -- add library configuration options here
}

@[default_target]
lean_exe «aoc» {
  root := `Main
}
