import Lake
open Lake DSL

package «feline» {
  -- add package configuration options here
}

lean_lib «Feline» where
  -- add library configuration options here

@[default_target]
lean_exe «feline» {
  root := `Main
}
