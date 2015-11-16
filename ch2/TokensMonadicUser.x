{
module Lexer (tigerLex) where

import Data.Char (ord)
}

%wrapper "monadUserState"

$digit = [0-9]

tokens :-

$white ;

-- Punctuation
<0> \,     { mkT Comma }
<0> :      { mkT Colon }
<0> \;     { mkT SemiColon }
<0> \(     { mkT Lparen }
<0> \)     { mkT Rparen }
<0> \[     { mkT Lbracket }
<0> \]     { mkT Rbracket }
<0> \{     { mkT Lbrace }
<0> \}     { mkT Rbrace }
<0> \.     { mkT Period }
<0> \+     { mkT Plus }
<0> \-     { mkT Minus }
<0> \*     { mkT Times }
<0> \/     { mkT Div }
<0> =      { mkT Eq }
<0> \<>    { mkT Ne }
<0> \<     { mkT Lt }
<0> \<=    { mkT Lte }
<0> >      { mkT Gt }
<0> >=     { mkT Gte }
<0> &      { mkT And }
<0> \|     { mkT Or }
<0> :=     { mkT Def }

-- Reserved Words
<0> while    { mkT While }
<0> for      { mkT For }
<0> to       { mkT To }
<0> break    { mkT Break }
<0> let      { mkT Let }
<0> in       { mkT In }
<0> end      { mkT End }
<0> function { mkT Function }
<0> var      { mkT Var }
<0> type     { mkT Type }
<0> array    { mkT Array }
<0> if       { mkT If }
<0> then     { mkT Then }
<0> else     { mkT Else }
<0> do       { mkT Do }
<0> of       { mkT Of }
<0> nil      { mkT Nil }

-- Identifiers
<0> [a-zA-Z][0-9a-zA-Z_]*  { mkId }

-- Literals
<0> $digit+  { mkInt }

-- Strings
<0> \"             { openStr `andBegin` str }
<str> \"           { closeStr `andBegin` 0 }
<str> \\\\         { addCharToStr '\\' }
<str> \\n          { addCharToStr '\n' }
<str> \\t          { addCharToStr '\t' }
<str> \\\"         { addCharToStr '\"' }
<str> \\$digit{3}  { addAsciiToStr }
<str> \\$white+\\  ;
<str> \\.          { badEscape }
<str> $printable   { addCurrentToStr }

-- Comments
<0> \/\*             { openComment `andBegin` comment }
<comment> \/\*       { openComment }
<comment> \*\/       { closeComment }
<comment> [.$white]  ;

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

  | Eof
  deriving (Show)

data Lexeme = Lexeme AlexPosn Token
  deriving (Show)

mkT :: Token -> AlexAction Lexeme
mkT t (p, _, _, _) _ = return $ Lexeme p t

mkInt :: AlexAction Lexeme
mkInt (p, _, _, str) len = return $ Lexeme p (IntLiteral val)
  where val = read $ take len str

mkId :: AlexAction Lexeme
mkId (p, _, _, str) len = return $ Lexeme p (Id $ take len str)

openComment :: AlexAction Lexeme
openComment inp len = do
  modifyCommentDepth (+1)
  skip inp len

closeComment :: AlexAction Lexeme
closeComment inp len = do
  modifyCommentDepth (+ (-1))
  d <- getCommentDepth
  if d == 0 then alexSetStartCode 0 else return ()
  skip inp len

openStr :: AlexAction Lexeme
openStr _ _ = do
  setStrVal ""
  alexMonadScan

closeStr :: AlexAction Lexeme
closeStr (p, _, _, _) _ = do
  ss <- getStrVal
  return $ Lexeme p (StringLiteral $ reverse ss)

addCharToStr :: Char -> AlexAction Lexeme
addCharToStr c (p, _, _, _) _ = do
  modifyStrVal (c:)
  alexMonadScan

addAsciiToStr :: AlexAction Lexeme
addAsciiToStr (p, _, _, (_:ds)) len = do
  let
    n = read (take 3 ds) :: Int
    c = toEnum n :: Char
  modifyStrVal (c:)
  alexMonadScan 
 
addCurrentToStr :: AlexAction Lexeme
addCurrentToStr (_, _, _, (c:_)) _ = do
  modifyStrVal (c:)
  alexMonadScan

badEscape :: AlexAction Lexeme
badEscape (AlexPn offset line column, _, _, _) _ =
  error $ "Bad escape sequence at"
    ++ " offset=" ++ show offset
    ++ " line=" ++ show line
    ++ " column=" ++ show column

alexEOF :: Alex Lexeme
alexEOF = return $ Lexeme (AlexPn (-1) 0 0) Eof

data AlexUserState = AlexUserState
  { commentDepth :: !Int
  , strVal :: String
  }

alexInitUserState = AlexUserState
  { commentDepth = 0
  , strVal = ""
  }

modifyCommentDepth :: (Int -> Int) -> Alex ()
modifyCommentDepth f = do
  d <- getCommentDepth
  setCommentDepth $ f d

getCommentDepth :: Alex Int
getCommentDepth = Alex $ \s@AlexState{alex_ust=ust } -> Right (s, commentDepth ust)

setCommentDepth :: Int -> Alex ()
setCommentDepth d = Alex $ \s@AlexState{alex_ust=ust } -> Right (s{alex_ust=ust{commentDepth=d}}, ())

modifyStrVal :: (String -> String) -> Alex ()
modifyStrVal f = do
  ss <- getStrVal
  setStrVal $ f ss

getStrVal :: Alex String
getStrVal = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, strVal ust)

setStrVal :: String -> Alex ()
setStrVal ss = Alex $ \s@AlexState{alex_ust=ust} -> Right (s{alex_ust=ust{strVal=ss}}, ())

tigerLex :: String -> Either String [Lexeme]
tigerLex ss = runAlex ss go
  where
    go :: Alex [Lexeme]
    go = do
      l@(Lexeme _ token) <- alexMonadScan
      sc <- alexGetStartCode
      case (token,sc) of
        (Eof, _)
          | sc == comment -> alexError "Encountered EOF with unclosed comment"
          | sc == str -> alexError "Encountered EOF with unclosed string"
          | otherwise -> return []

        _ -> do
          ls <- go
          return $ l:ls
}
