module Lc3

import System
import Data.Bits
import Data.Buffer
import Data.IOArray

import System.File.Mode
import System.File.Buffer
import System.File.Handle

memorySize : Int
memorySize = shiftL 1 16

record Memory where
  constructor MkMemory
  array : IOArray Int

readImage : String -> IO (Maybe Memory)
readImage path = do
  Just originBuffer <- Buffer.newBuffer 2
    | Nothing => pure Nothing

  Right _ <- Handle.withFile path Read
              (\err => pure err)
              (\file => Buffer.readBufferData file originBuffer 0 2)
    | Left err => pure Nothing

  origin <- Buffer.getBits16 originBuffer 0
  maxRead <- pure (memorySize - (cast origin))

  Just memoryBuffer <- Buffer.newBuffer memorySize
    | Nothing => pure Nothing

  Right readBytes <- Handle.withFile path Read
              (\err => pure err)
              (\file => Buffer.readBufferData file memoryBuffer 0 maxRead)
    | Left err => pure Nothing

  memory <- IOArray.newArray memorySize
  bufferToArray (cast origin) 0 (div maxRead 2) memoryBuffer memory

  pure $ Just $ MkMemory memory
  where
    bufferToArray : (origin: Int) -> (pos : Int) -> (max : Int) -> Buffer -> IOArray Int -> IO ()
    bufferToArray origin pos max buf arr =
      if pos == max then
        pure ()
      else do
        n <- Buffer.getBits16 buf (pos * 2)
        True <- IOArray.writeArray arr (origin + pos) (cast n)
          | False => pure ()
        bufferToArray origin (pos + 1) max buf arr

data Command = Disassemble String | Help

Show Command where
  show (Disassemble file) = "Disassemble \{show file}"
  show Help = "Help"

parseCommand : List String -> Command
parseCommand [_, "disassemble", file] = Disassemble file
parseCommand _ = Help

readFile : String -> IO (Maybe Buffer)
readFile path = do
  Just buffer <- Buffer.newBuffer memorySize
    | Nothing => pure Nothing

  Right _ <- Handle.withFile path Read
                      (\err => pure err)
                      (\file => Buffer.readBufferData file buffer 0 memorySize)
    | Left err => pure Nothing
  pure (Just buffer)

executeCommand : Command -> IO ()
executeCommand (Disassemble file) = do
  Just buffer <- readFile file
    | Nothing => putStrLn "Could not read file \{show file}"

  origin <- Buffer.getBits16 buffer 0
  putStrLn "read buffer"

executeCommand Help = do
  putStrLn "Supported commands\n\nlc3 disassemble [file]"

main : IO ()
main = do
  args <- System.getArgs
  executeCommand $ parseCommand args
