module Lc3

import Data.Bits
import Data.Buffer
import Data.String
import Data.IOArray

import System
import System.File.Mode
import System.File.Buffer
import System.File.Handle

memorySize : Int
memorySize = shiftL 1 16

hexDigit : Int -> String
hexDigit n = if n < 10 then singleton $ chr (n + ord '0')
                       else singleton $ chr (n - 10 + ord 'a')

byteToHex : Int -> String
byteToHex n =
  let hi = (n `shiftR` 4) .&. 0xF
      lo = n .&. 0xF
  in hexDigit hi ++ hexDigit lo

-- Convert Int16 to hex string (big-endian by default)
int16ToHex : Int16 -> String
int16ToHex n =
  let raw : Int = cast n -- widen to Int for safe bit ops
      hi = (raw `shiftR` 8) .&. 0xFF
      lo = raw .&. 0xFF
  in byteToHex hi ++ byteToHex lo

toHexString : Int16 -> String
toHexString n =
  let n = int16ToHex n in
  "0x" ++ n

take : (fn : a -> Maybe b) -> Maybe a -> Maybe b
take fn (Just x) = fn x
take fn Nothing  = Nothing

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

asRegister : Int16 -> Maybe Register
asRegister 0 = Just R_R0
asRegister 1 = Just R_R1
asRegister 2 = Just R_R2
asRegister 3 = Just R_R3
asRegister 4 = Just R_R4
asRegister 5 = Just R_R5
asRegister 6 = Just R_R6
asRegister 7 = Just R_R7
asRegister _ = Nothing

Show Register where
  show R_R0 = "R0"
  show R_R1 = "R1"
  show R_R2 = "R2"
  show R_R3 = "R3"
  show R_R4 = "R4"
  show R_R5 = "R5"
  show R_R6 = "R6"
  show R_R7 = "R7"

data RegisterOrValue = Val Int16 | Reg Register

Show RegisterOrValue where
  show (Val int) = toHexString int
  show (Reg reg) = show reg

record OpBr where
  constructor MkOpBr
  pcOffset : Int16
  condFlag : Int16

record TwoOperators where
  constructor MkTwoOperators
  dr  : Register
  sr1 : Register
  sr2 : RegisterOrValue

record LoadRegister where
  constructor MkLoadRegister
  dr       : Register
  pcOffset : Int16

record OpJsr where
  constructor MkOpJsr
  sr       : RegisterOrValue

record RegOffset where
  constructor MkRegOffset
  dr     : Register
  sr     : Register
  offset : Int16

record OpNot where
  constructor MkOpNot
  dr     : Register
  sr     : Register

data OpCode =
   OP_BR  OpBr
 | OP_ADD TwoOperators
 | OP_LD  LoadRegister
 | OP_ST  LoadRegister
 | OP_JSR OpJsr
 | OP_AND TwoOperators
 | OP_LDR RegOffset
 | OP_STR RegOffset
 | OP_RTI
 | OP_NOT OpNot
 | OP_LDI LoadRegister

Show OpCode where
  show (OP_BR (MkOpBr pcOffset condFlag)) =
    let
      n = if (condFlag .&. 4) /= 0 then "n" else ""
      z = if (condFlag .&. 2) /= 0 then "z" else ""
      p = if (condFlag .&. 1) /= 0 then "p" else ""
      pcOffset = toHexString pcOffset
    in
    "BR\{n}\{z}\{p} \{pcOffset}"

  show (OP_ADD (MkTwoOperators dr sr1 sr2)) =
    "ADD \{show dr} \{show sr1} \{show sr2}"

  show (OP_LD (MkLoadRegister dr pcOffset)) =
    "LD \{show dr} \{toHexString pcOffset}"

  show (OP_ST (MkLoadRegister dr pcOffset)) =
    "ST \{show dr} \{toHexString pcOffset}"

  show (OP_JSR (MkOpJsr sr )) =
    "JSR \{show sr}"

  show (OP_AND (MkTwoOperators dr sr1 sr2)) =
    "AND \{show dr} \{show sr1} \{show sr2}"

  show (OP_LDR (MkRegOffset dr sr offset)) =
    "LDR \{show dr} \{show sr} \{toHexString offset}"

  show (OP_STR (MkRegOffset dr sr offset)) =
    "STR \{show dr} \{show sr} \{toHexString offset}"

  show (OP_RTI) = "RTI"

  show (OP_NOT (MkOpNot dr sr)) =
    "NOT \{show dr} \{show sr}"

  show (OP_LDI (MkLoadRegister dr pcOffset)) =
    "LDI \{show dr} \{toHexString pcOffset}"

parseOpBr : Int16 -> Maybe OpCode
parseOpBr instr =
  let pcOffset = signedBits 0 9 instr in
  let condFlag = bits 9 3 instr in
  pure $ OP_BR (MkOpBr !pcOffset !condFlag)

parseTwoOperators : Int16 -> Maybe TwoOperators
parseTwoOperators instr =
  let
    immFlag = bits 5 1 instr
    dr      = take asRegister $ bits 9 3 instr
    sr1     = take asRegister $ bits 6 3 instr
  in
  if !immFlag == 0 then
    let r = take asRegister $ bits 0 3 instr in
    Just (MkTwoOperators !dr !sr1 (Reg !r))
  else
    let v = signedBits 0 5 instr in
    Just (MkTwoOperators !dr !sr1 (Val !v))

parseOpAdd : Int16 -> Maybe OpCode
parseOpAdd instr =
  Just $ OP_ADD $ !(parseTwoOperators instr)

parseLoadRegister : Int16 -> Maybe LoadRegister
parseLoadRegister instr =
  let
    dr       = take asRegister $ bits 9 3 instr
    pcOffset = signedBits 0 9 instr
  in
  Just $ MkLoadRegister !dr !pcOffset

parseOpLd : Int16 -> Maybe OpCode
parseOpLd instr =
  Just $ OP_LD $ !(parseLoadRegister instr)

parseOpSt : Int16 -> Maybe OpCode
parseOpSt instr =
  Just $ OP_ST $ !(parseLoadRegister instr)

parseOpJsr : Int16 -> Maybe OpCode
parseOpJsr instr =
  let longFlag = bits 11 1 instr in
  if !longFlag == 0 then
    let r = take asRegister $ bits 6 3 instr in
    Just $ OP_JSR $ MkOpJsr $ Reg $ !r
  else
    let longPcOffset = signedBits 0 11 instr in
    Just $ OP_JSR $ MkOpJsr $ Val $ !longPcOffset

parseOpAnd : Int16 -> Maybe OpCode
parseOpAnd instr =
  Just $ OP_AND $ !(parseTwoOperators instr)

parseRegOffset : Int16 -> Maybe RegOffset
parseRegOffset instr =
  let
    dr     = take asRegister $ bits 9 3 instr
    sr     = take asRegister $ bits 6 3 instr
    offset = signedBits 0 6 instr
  in
  Just $ MkRegOffset !dr !sr !offset

parseOpLdr : Int16 -> Maybe OpCode
parseOpLdr instr =
  Just $ OP_LDR $ !(parseRegOffset instr)

parseOpStr : Int16 -> Maybe OpCode
parseOpStr instr =
  Just $ OP_STR $ !(parseRegOffset instr)

parseOpRti : Int16 -> Maybe OpCode
parseOpRti _ = Just OP_RTI

parseOpNot : Int16 -> Maybe OpCode
parseOpNot instr =
  let
    dr = take asRegister $ bits 9 3 instr
    sr = take asRegister $ bits 6 3 instr
  in
  Just $ OP_NOT $ MkOpNot !dr !sr

parseOpLdi : Int16 -> Maybe OpCode
parseOpLdi instr =
  Just $ OP_LDI $ !(parseLoadRegister instr)

parseOpCode : (instr: Int16) -> Maybe OpCode
parseOpCode instr =
  let op = bits 12 4 instr in
  case !op of
    0  => parseOpBr  instr
    1  => parseOpAdd instr
    2  => parseOpLd  instr
    3  => parseOpSt  instr
    4  => parseOpJsr instr
    5  => parseOpAnd instr
    6  => parseOpLdr instr
    7  => parseOpStr instr
    8  => parseOpRti instr
    9  => parseOpNot instr
    10 => parseOpLdi instr
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
          Just 0 => aux arr (pos + 1) acc
          Nothing => aux arr (pos + 1) acc

          Just opcode =>
            case parseOpCode opcode of
              Just op => aux arr (pos + 1) ("\{acc}\{show op}\n")
              Nothing => aux arr (pos + 1) acc

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
  image <- toString image
  putStrLn image

executeCommand Help = do
  putStrLn "Supported commands\n\nlc3 disassemble [file]"

main : IO ()
main = do
  args <- System.getArgs
  executeCommand $ parseCommand args
