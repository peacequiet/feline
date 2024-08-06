import «Feline»
-- based on Unix program `cat`


def bufsize : USize := 20 * 1024

-- partial function because it is not clear that this function terminates
partial def dump (stream : IO.FS.Stream ) : IO Unit := do
  let buf ← stream.read bufsize       -- action to create buffer (?)
  if buf.isEmpty then                 -- if out of buffer space
    pure ()                           -- terminate
  else
    let stdout ← IO.getStdout         -- waits for Stdout
    stdout.write buf                  -- writes to buffer (?)
    dump stream                       -- indefinite recursive call



def fileStream (filename : System.FilePath) : IO (Option IO.FS.Stream) := do
  let fileExists ← filename.pathExists
  if not fileExists then                                      -- simple checking if file exists
    let stderr ← IO.getStderr
    stderr.putStrLn s!"File not found: {filename}"
    pure none                                                 -- use of Option monad
  else
    let handle ← IO.FS.Handle.mk filename IO.FS.Mode.read
    pure (some (IO.FS.Stream.ofHandle handle))

def process (exitCode : UInt32) (args : List String) : IO UInt32 := do
  match args with
  | [] => pure exitCode                         -- empty args, exist
  | "-" :: args =>                              -- dash case, read user input
    let stdin ← IO.getStdin
    dump stdin
    process exitCode args
  | filename :: args =>                         -- filename case, read file
    let stream ← fileStream ⟨filename⟩
    match stream with
    | none =>
      process 1 args
    | some stream =>
      dump stream
      process exitCode args

-- note implicit do in all match-cases

def main (args : List String) : IO UInt32 :=
  match args with
  | [] => process 0 ["-"]
  | _ =>  process 0 args
