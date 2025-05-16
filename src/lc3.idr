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

bits: (pos : Nat) -> (width : Nat) -> Int16 -> Maybe Int16
bits pos width num =
  let
    pos     = natToFin pos 16
    width   = natToFin width 16
    mask    = (1 `shiftL` !width) - 1
    shifted = num `shiftR` !pos
  in pure (shifted .&. mask)

signExtend : Int16 -> (bitCount: Nat) -> Maybe Int16
signExtend num bitCount =
  let
    signBit     = bits (pred bitCount) 1 num
    bitCount    = natToFin bitCount 16
    signMask    = 0xffff `shiftL` !bitCount
    extendedNum = num .|. signMask
  in
  if !signBit == 0 then
    pure num
  else
    pure extendedNum

signedBits : (pos: Nat) -> (width: Nat) -> Int16 -> Maybe Int16
signedBits pos width num =
  let bitField = bits pos width num in
  signExtend !bitField width

data Register = R_R0 | R_R1 | R_R2 | R_R3 | R_R4 | R_R5 | R_R6 | R_R7

record OpBr where
  constructor MkOpBr
  pcOffset : Int16
  condFlag : Int16

data OpCode =
 OP_BR OpBr

parseOpBr : Int16 -> Maybe OpCode
parseOpBr instr =
  let pcOffset = signedBits 0 9 instr in
  let condFlag = bits 9 3 instr in
  pure $ OP_BR (MkOpBr !pcOffset !condFlag)

parseOpCode : (instr: Int16) -> Maybe OpCode
parseOpCode instr =
  let op = bits 12 4 instr in
  case !op of
    0 => parseOpBr instr
    _ => Nothing

record Memory where
  constructor MkMemory
  array : IOArray Int16

toString : Memory -> IO String
toString (MkMemory arr) = aux arr 0 ""
  where
    aux : IOArray Int16 -> Int -> String -> IO String
    aux arr pos acc =
      if pos + 1 == memorySize then
        pure acc
      else do
        opcode <- IOArray.readArray arr pos
        case opcode of
          Nothing => aux arr (pos + 1) acc
          Just opcode =>
            if opcode == 0 then
              aux arr (pos + 1) acc
            else
              aux arr (pos + 1) ("\{acc}\n\{show opcode}")

readImage : String -> IO (Maybe Memory)
readImage path = do
  Just originBuffer <- Buffer.newBuffer 2
    | Nothing => pure Nothing

  Right _ <- Handle.withFile path Read
              (\err => pure err)
              (\file => Buffer.readBufferData file originBuffer 0 2)
    | Left err => pure Nothing

  origin <- Buffer.getInt16 originBuffer 0
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
    bufferToArray : (origin: Int) -> (pos : Int) -> (max : Int) -> Buffer -> IOArray Int16 -> IO ()
    bufferToArray origin pos max buf arr =
      if pos == max then
        pure ()
      else do
        n <- Buffer.getInt16 buf (pos * 2)
        True <- IOArray.writeArray arr (origin + pos) n
          | False => pure ()
        bufferToArray origin (pos + 1) max buf arr

data Command = Disassemble String | Help

Show Command where
  show (Disassemble file) = "Disassemble \{show file}"
  show Help = "Help"

parseCommand : List String -> Command
parseCommand [_, "disassemble", file] = Disassemble file
parseCommand _ = Help

executeCommand : Command -> IO ()
executeCommand (Disassemble file) = do
  Just image <- readImage file
    | Nothing => putStrLn "Could not read file \{show file}"

  out <- toString image

  putStrLn out

executeCommand Help = do
  putStrLn "Supported commands\n\nlc3 disassemble [file]"

main : IO ()
main = do
  args <- System.getArgs
  executeCommand $ parseCommand args
