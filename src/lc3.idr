module Lc3

import System
import System.File.ReadWrite

data Command = Disassemble String | Help

Show Command where
  show (Disassemble file) = "Disassemble \{show file}"
  show Help = "Help"

parseCommand : List String -> Command
parseCommand [_, "disassemble", file] = Disassemble file
parseCommand _ = Help

readFile : String -> IO (Maybe String)
readFile path = do
  Right content <- System.File.ReadWrite.readFile path
    | Left err => pure Nothing
  pure (Just content)

executeCommand : Command -> IO ()
executeCommand (Disassemble file) = do
  Just fileContent <- readFile file
    | Nothing => putStrLn "Could not read file \{show file}"
  putStrLn "Read file \{show file}:\n\{fileContent}"

executeCommand Help = do
  putStrLn "Supported commands\n\nlc3 disassemble [file]"

main : IO ()
main = do
  args <- System.getArgs
  command <- pure (parseCommand args)
  executeCommand command
