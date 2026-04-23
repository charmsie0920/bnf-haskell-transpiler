module Assignment
  ( bnfParser
, getTime
  , firstParserName
  , generateHaskellCode
  , Symbol(..)
  , Node(..)
  , ADT(..)
  , Rule(..)
  , RuleHeader(..)
  , Seq(..)
  , Alt(..)
  ) where

import Instances (Parser (..), ParseResult(..))
import Parser
    ( inlineSpaces
    , inlineSpace
    , is
    , noneof
    , eof
    , string
    , alpha
    , digit
    , lower
    )
import GenerateHaskell(genDataType, genParser, joinBlocks)

import Control.Applicative (Alternative (..), some, many)
import Data.Time (formatTime, defaultTimeLocale, getCurrentTime)
import Types
    ( ADT (..)
    , Rule (..)
    , RuleHeader (..)
    , Alt (..)
    , Seq (..)
    , Node (..)
    , Symbol (..)
    , Macro (..)
    , Name
    )

-- | Parses an entire BNF grammar into an ADT structure.
-- Input: a text containing grammar rules.
-- Output: an ADT containing all parsed rules.
bnfParser :: Parser ADT
bnfParser = do 
    blankLines -- skip leading blank lines
    rules <- some (ruleParser <* blankLines) --parse one or more rules
    eof --ensure end of input
    pure (ADT rules) --return ADT


--Generates the full Haskell code (types + parsers) for the entire ADT.
-- Input: ADT (list of all rules).
-- Output: full combined Haskell code as a string.
generateHaskellCode :: ADT -> String
generateHaskellCode (ADT rules) = 
  -- ghenerate type blocks (data/newtype) and parser blocks for each rule
    let typeBlocks = map genDataType rules 
        parserBlocks = map genParser rules 
        allBlocks = typeBlocks ++ parserBlocks
    in joinBlocks allBlocks ++ "\n"

-- | Extracts the name of the first parser rule in the ADT.
-- Input: ADT structure.
-- Output: name of the first rule or "Output" if empty.
firstParserName :: ADT -> String
firstParserName (ADT (Rule (RuleHeader name _params) _rhs : _)) = name
firstParserName _ = "Output"


getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getCurrentTime

-- trims spaces before and after a parser
-- Input: a parser p.
-- Output: the same parser but ignoring spaces before and after it.
trim0 :: Parser a -> Parser a
trim0 p = inlineSpaces *> p <* inlineSpaces

-- parse a single newline (either \n or \r\n)
newline :: Parser()
newline = (is '\r' *> is '\n' *> pure()) <|> (is '\n' *> pure())

-- | Accepts either a newline or end-of-input.
-- Used at the end of grammar rules.
endOfInput :: Parser ()
endOfInput = newline <|> eof


--consume zero or more blank lines (lines with only spaces or empty)
-- >> parse blankLines "\n\n  \nabc"
-- Result >abc<
blankLines :: Parser ()
blankLines = many (inlineSpaces *> newline) *> pure ()

-- parse a non terminal name (without angle brackets)
-- uses lower, alpha, digit, is'_', many from Parser.hs
-- >> parse nonTerminal "expr1_2 rest"
-- Result > rest< "expr1_2"
nonTerminal :: Parser Name
nonTerminal = (:) <$> lower <*> many (alpha <|> digit <|> is '_')


-- parse a macro [int], [alpha], [newline]
-- uses string from Parser.hs
-- >> parse macroParcer "[int]"
-- Result >< MInt
macroParcer :: Parser Macro
macroParcer = (string "[int]" *> pure MInt)
            <|> (string "[alpha]" *> pure MAlpha)
            <|> (string "[newline]" *> pure MNewline)

-- parse a quoted terminal string (e.g. "+" or "if")
-- uses is'"', noneof, many from Parser.hs
-- >> parse quote "\"+\""
-- Result >< "+"
quote :: Parser String
quote = (is '"' *> many (noneof "\"\n") <* is '"')

-- | Peeks at the next character in the input without consuming it.
-- Output: Maybe Char (Nothing if at end of file).
nextChar :: Parser (Maybe Char)
nextChar = Parser $ \input ->
    case input of
        [] -> Result "" Nothing
        (c:_) -> Result input (Just c)


-- helper: parse one-or-more items separated by a separator
-- >> let nameBar = sepBy1 parseName (trim0 (is '|'))
-- >> parse nameBar "<a>|<b>|  <c>  rest"
-- Result >rest< ["a","b","c"]
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)


-- parse a sequence of one-or-more symbols separated by spaces
-- uses sepBy1 and some inlineSpaces
-- >> parse seqParser "\"(\" <expr> \")\""
-- Result >< [Terminal "(",NonTerminal "expr",Terminal ")"]
seqParser :: Parser [Node]
seqParser = do 
    xs <- sepBy1 nodeWithModifier (some inlineSpace)
    pure xs

-- parse one-or-more sequences separated by '|'
-- uses sepBy1, trim0, and seqParser
-- >> parse altParse "\"(\" <expr> \")\""
-- Result >< [[Terminal "(",NonTerminal "expr",Terminal ")"]]
altParse :: Parser [[Node]]
altParse = sepBy1 seqParser (trim0 (is '|'))

-- parse one complete grammar rule of the form <name> ::= alternatives
-- ends with newline or EOF (handled by endOfInput)
-- >> parse ruleParser "<expr> ::= <term> | <expr> \"+\" <term>\n"
-- Result >< Rule {ruleName = "expr", ruleDef = Alt [Seq [...], Seq [...]]}
ruleParser :: Parser Rule
ruleParser =
  Rule <$> (inlineSpaces *> ruleHeaderParser)
       <*> (inlineSpaces *> string "::=" *> inlineSpaces *> (Alt . map Seq <$> altParse)
            <* inlineSpaces <* endOfInput)


--Part C: Modifiers
-- parse a parameter char [a]
-- results is the char inside the brackets
paramChar :: Parser Char
paramChar= do
  _ <- is '['
  c <- lower
  _ <- is ']'
  pure c

--comma with optional spaces around
commaTrim :: Parser()
commaTrim = inlineSpaces *> is ',' *> inlineSpaces *> pure ()

--at least one non-newline space white space after tok
nonNewlineSpace1 :: Parser()
nonNewlineSpace1 = do
  _ <- some inlineSpace
  pure ()

-- | Parses a rule header of the form <name> or <name(a,b,...)].
-- Input: a nonterminal name possibly followed by parameter list.
-- Output: a RuleHeader with name and list of parameters.
ruleHeaderParser :: Parser RuleHeader
ruleHeaderParser =
  do
    _ <- is '<'
    name <- nonTerminal
    params <- paramList
    _ <- is '>'
    let dupes = [p | (i, p) <- zip [0..] params, p `elem` take i params]
    if null dupes
      then pure (RuleHeader name params)
      else empty
  where
    -- parses parameter list if present, otherwise empty
    paramList =
      (inlineSpaces *> is '(' *> inlineSpaces *>
        sepBy1 (trim0 lower) commaTrim
        <* inlineSpaces <* is ')')
      <|> pure []

-- | Parses a nonterminal use, possibly with parameters.
-- Example: <expr(a,b)> or <expr>
-- Output: NodeSymbol (NonTerminal name) or NodeApply name args. 
ntNode :: Parser Node
ntNode = do
  _ <- is '<'
  name <- nonTerminal
  args <- (do
    _ <- inlineSpaces
    _ <- is '('
    _ <- inlineSpaces
    as <- sepBy1 (trim0 nodeWithModifier) commaTrim
    _ <- inlineSpaces
    _ <- is ')'
    pure as) <|> pure []
  _ <- is '>'
  pure $ if null args
    then NodeSymbol (NonTerminal name)
    else NodeApply name args

-- | Parses a single atomic node unit: could be
-- a terminal, macro, nonterminal, or parameter.
-- Input: one symbol.
-- Output: corresponding Node constructor.
singleNode :: Parser Node
singleNode = 
  (NodeSymbol . Terminal <$> quote)
  <|> (NodeSymbol . SMacro <$> macroParcer)
  <|> ntNode
  <|> (NodeParam <$> paramChar)

-- | Parses a 'tok' keyword followed by a single node.
-- Example: tok "a"
-- Output: NodeTok wrapping the parsed node.
tokNode :: Parser Node
tokNode =
  NodeTok <$> (string "tok" *> nonNewlineSpace1 *> singleNode)


-- | Parses a single node unit for grammar definitions.
-- Includes terminals, macros, nonterminals, parameters, or 'tok'.
-- Output: Node constructor representing that symbol.
baseNode :: Parser Node
baseNode = 
  (NodeSymbol . Terminal <$> quote) 
  <|> (NodeSymbol . SMacro <$> macroParcer) 
  <|> ntNode
  <|> (NodeParam <$> paramChar)
  <|> tokNode

-- | Parses modifiers applied to nodes (*, +, ?).
-- Input: a base node followed by an optional modifier.
-- Output: modified Node (NodeStar, NodePlus, NodeQuestion) or unchanged.
nodeWithModifier :: Parser Node
nodeWithModifier =
  baseNode >>= \n ->
        (NodeStar n <$ is '*')
    <|> (NodePlus n <$ is '+')
    <|> (NodeQuestion n <$ is '?')
    <|> pure n

