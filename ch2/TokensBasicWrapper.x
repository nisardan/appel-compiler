{
module Lexer (tigerLex) where

import Data.Char (isDigit)
import Data.Word (Word8)

import qualified Data.Bits
}

$digit = [0-9]
@escape = \\(\\ | n | t | $digit{3} | ")

tokens :-

$white ;

\/\*       { \_ -> OpenComment }
\*\/       { \_ -> CloseComment }

-- Punctuation
<0> \,     { \_ -> Comma }
<0> :      { \_ -> Colon }
<0> \;     { \_ -> SemiColon }
<0> \(     { \_ -> Lparen }
<0> \)     { \_ -> Rparen }
<0> \[     { \_ -> Lbracket }
<0> \]     { \_ -> Rbracket }
<0> \{     { \_ -> Lbrace }
<0> \}     { \_ -> Rbrace }
<0> \.     { \_ -> Period }
<0> \+     { \_ -> Plus }
<0> \-     { \_ -> Minus }
<0> \*     { \_ -> Times }
<0> \/     { \_ -> Div }
<0> =      { \_ -> Eq }
<0> \<>    { \_ -> Ne }
<0> \<     { \_ -> Lt }
<0> \<=    { \_ -> Lte }
<0> >      { \_ -> Gt }
<0> >=     { \_ -> Gte }
<0> &      { \_ -> And }
<0> \|     { \_ -> Or }
<0> :=     { \_ -> Def }

-- Reserved Words
<0> while  { \_ -> While }
<0> for    { \_ -> For }
<0> to     { \_ -> To }
<0> break  { \_ -> Break }
<0> let    { \_ -> Let }
<0> in     { \_ -> In }
<0> end    { \_ -> End }
<0> function { \_ -> Function }
<0> var    { \_ -> Var }
<0> type   { \_ -> Type }
<0> array  { \_ -> Array }
<0> if     { \_ -> If }
<0> then   { \_ -> Then }
<0> else   { \_ -> Else }
<0> do     { \_ -> Do }
<0> of     { \_ -> Of }
<0> nil    { \_ -> Nil }

-- Identifiers
<0> [a-zA-Z][0-9a-zA-Z_]*  { Id }

-- Literals
<0> $digit+  { \s -> IntLiteral (read s :: Int) }
<0> \" ($printable # [\\\"] | @escape)+ \" { \s -> StringLiteral $ unescape s }

-- Comments
<comment> [.$white]      { \_ -> SkipComment }

{

data Token =
    -- Punctuation
    Comma | Colon | SemiColon | Lparen | Rparen | Lbracket | Rbracket
  | Lbrace | Rbrace | Period | Plus | Minus | Times | Div | Eq | Ne | Lt
  | Lte | Gt | Gte | And | Or | Def
  
    -- Reserved Words
  | While | For | To | Break | Let | In | End | Function | Var | Type | Array
  | If | Then | Else | Do | Of | Nil

    -- Identifiers
  | Id String

    -- Literals
  | IntLiteral Int | StringLiteral String

    -- Comments (not actually returned)
  | OpenComment | CloseComment | SkipComment
  deriving (Show)


unescape :: String -> String
unescape [] = []
unescape ('\\':s:s':s'':ss)
  |  isDigit s
  && isDigit s'
  && isDigit s'' = (toEnum (read [s,s',s''] :: Int) :: Char): unescape ss
unescape ('\\':s:ss)
  | s == 'n' = '\n' : unescape ss
  | s == 't' = '\t' : unescape ss
  | s == '"' = '\"' : unescape ss
  | s == 'r' = '\r' : unescape ss
  | otherwise = '\\' : unescape ss
unescape (s:ss) = s : unescape ss


-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

type Byte = Word8

type AlexInput = (Char,[Byte],String)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (c,_,_) = c

-- alexScanTokens :: String -> [token]
alexScanTokens str = go ('\n',[],str) 0 0
  where
    go _ _ n | n < 0 = error "can't close unopened comment"

    -- "n" is the number of comment nestings (we do not need a full blown
    -- stack for nested comments).
    go inp@(_,_bs,s) sc n =
      case alexScan inp sc of
        AlexEOF -> if sc == 0 then [] else error "comments not closed"
        AlexError _ -> error "lexical error"
        AlexSkip inp' len -> go inp' sc n
        AlexToken inp' len act -> case act (take len s) of
          OpenComment -> if sc == 0 then go inp' 1 1 else go inp' 1 (n+1)
          CloseComment -> if n == 1 then go inp' 0 0 else go inp' 1 (n-1)
          SkipComment -> go inp' sc n
          tok -> tok : go inp' sc n

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (c,(b:bs),s) = Just (b,(c,bs,s))
alexGetByte (c,[],[])    = Nothing
alexGetByte (_,[],(c:s)) = case utf8Encode c of
                             (b:bs) -> Just (b, (c, bs, s))
                             [] -> Nothing

tigerLex = alexScanTokens
}
