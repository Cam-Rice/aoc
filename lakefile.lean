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
--  root := `Main
}

require mathlib from git
  "https://github.com/leanprover-community/mathlib4.git"@"master"

--require std from git "https://github.com/leanprover/std4"