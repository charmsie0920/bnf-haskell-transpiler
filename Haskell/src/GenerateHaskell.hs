module GenerateHaskell (cap, genDataType, genParser, joinBlocks, ) where
import Types (ADT(..), Rule(..), RuleHeader(..), Alt(..), Seq(..), Node(..), Symbol(..), Macro(..))
import Data.Char (toUpper, toLower)
import Data.Monoid((<>))
import Data.List (intercalate)

--Capitalize the first letter of a string.
-- Input: string like "expr"
-- Output: "Expr"
cap :: String -> String
cap [] = []
cap (c:cs) = toUpper c : cs

-- Lowercase the first letter of a string.
-- Input: string like "Expr"
-- Output: "expr"
low :: String -> String
low [] = []
low (c : cs) = toLower c : cs

-- Map a Symbol to its Haskell type
sym2Type :: Symbol -> String
sym2Type (NonTerminal n) = cap n
sym2Type (Terminal _) = "String"
sym2Type (SMacro MInt) = "Int"
sym2Type (SMacro MAlpha) = "String"
sym2Type (SMacro MNewline)= "Char"

-- Map a Symbol to its parser expr
sym2Parser :: Symbol -> String
sym2Parser (NonTerminal n) = low n
sym2Parser (Terminal s) = "(string " ++ show s ++ ")"
sym2Parser (SMacro MInt) = "int"
sym2Parser (SMacro MAlpha) = "(some alpha)"
sym2Parser (SMacro MNewline) = "(is '\\n')"

-- Render a space-separated list of types/terms
--foldr1 uses last element as starting point for accumulation
joinWords :: [String] -> String
joinWords [] = "" --empty list case (just in case)
joinWords words = foldr1 (\a b -> a ++ " " ++ b) words

-- Common string for deriving Show
derivingShow :: String 
derivingShow = "    deriving Show"

-- Add parentheses around a type if it contains spaces or
parentheses :: String -> String
parentheses t = if any (`elem` t) " ()" then "(" ++ t ++ ")" else t

--Adds spaces for alignment when creating multiple constructors.
-- Used to make "data" type definitions look neat.
alignConstructor :: String -> String -> String
alignConstructor typeName typeVars = 
  replicate (length ("data" ++ typeName ++ typeVars ++ " = ")) ' ' 

--Generates a Haskell 'data' or 'newtype' definition for a grammar rule.
-- Input: one Rule (with name, params, and alternatives).
-- Output: String containing the generated Haskell type definition.
genDataType :: Rule -> String
genDataType (Rule (RuleHeader ruleName params) (Alt alternatives)) =
  case alternatives of
    -- Special case: if only one constructor and no parameters -> use newtype
    [Seq [singleNode]] | null params -> 
      unlines
        [ "newtype " ++ typeName ++ " = " ++ typeName ++ " " ++ parentheses (nodeType singleNode)
        , derivingShow]
     --otherwise, build a normal 'data' type with multiple constructors
    _ ->
      --add type variables if present (<pair(a,b)> -> data Pair a b)
      let typeVars = if null params then "" else " " ++ joinWords (map (:[]) params)
      --generates each constructor for each alternative (rule version)
          genConstructor :: Int -> Seq -> String
          genConstructor constructorNum (Seq nodeSeq) =
            let constructor = typeName ++ show constructorNum
                header = if constructorNum == 1
                         then "data " ++ typeName ++ typeVars ++ " = " ++ constructor
                         else alignConstructor typeName typeVars ++ "| " ++ constructor
                --build field types inside constructor, like Expr1 Int Strin
                fields = case nodeSeq of
                           [] -> ""
                           _  -> " " ++ joinWords (map (parentheses . nodeType) nodeSeq)
            in header ++ fields
      in unlines (zipWith genConstructor [1..] alternatives ++ [derivingShow])
  where
    typeName = cap ruleName

-- generates the parser function for one rule.
-- Input: one Rule.
-- Output: Haskell parser code (String) that parses that rule.
genParser :: Rule -> String
genParser (Rule (RuleHeader ruleName params) (Alt alternatives)) =
  --combines type sig line and all parser body lines in one block
  unlines $ typeSignatureLine : functionBodyLines
  where
    typeName = cap ruleName
    parserName = low ruleName
    typeVariables = if null params then "" else " " <> unwords (map (:[]) params)

    -- build the type signature line
    -- if no params: expr :: Parser Expr
    -- if params exist: pair :: Parser a -> Parser b -> Parser (Pair a b)
    typeSignatureLine
      | null params = parserName <> " :: Parser " <> typeName
      | otherwise =
          let parameterParsers = unwords (map (\p -> "Parser " <> [p] <> " ->") params)
          in parserName <> " :: " <> parameterParsers <> " Parser (" <> typeName <> typeVariables <> ")"
    -- append parameters to function call, like " expr a b"
    parserParameters
      | null params = ""
      | otherwise = " " <> unwords (map (:[]) params)

    alternativeIndent = replicate (length (parserName <> parserParameters <> " = ")) ' '
    isNewtypeCase = null params && case alternatives of { [Seq [_]] -> True; _ -> False }

    getConstructorName constructorNumber
      | isNewtypeCase = typeName
      | otherwise= typeName <> show constructorNumber
    --build one alternative parser expression, 
    -- Expr1 <$> term <*> plus <*> factor
    createAlternativeParser constructorNumber (Seq nodesInSequence) =
      let constructorToBuild = getConstructorName constructorNumber
          parserLogic = case nodesInSequence of
              [] -> constructorToBuild
              -- otherwise, build an applicative chain like:
              -- Constructor <$> node1 <*> node2 ...
              (firstNode:restOfNodes) ->
                  mconcat [ constructorToBuild
                          , " <$> ", nodeParser firstNode
                          , concatMap (\node -> " <*> " <> nodeParser node) restOfNodes
                          ]
          -- first line uses "=", others use "<|>" for alternatives        
          parserLineStart
            | constructorNumber == 1 = parserName <> parserParameters <> " = "
            | otherwise = alternativeIndent <> "<|> "
      in parserLineStart <> parserLogic

    functionBodyLines
      | null alternatives = [parserName <> parserParameters <> " = empty"]
      | otherwise = zipWith createAlternativeParser [1..] alternatives

-- Removes extra newlines from the end of a string.
-- Input: "text\n\n"
-- Output: "text"
strip :: String -> String 
strip = reverse . dropWhile (== '\n') . reverse

--Joins multiple code blocks together with spacing between them.
-- Input: ["data Expr ...", "expr :: Parser Expr ..."]
-- Output: "data Expr ...\n\nexpr :: Parser Expr ..."
joinBlocks :: [String] -> String
joinBlocks [] = ""
joinBlocks [x] = x
joinBlocks (x:xs) = strip x ++ "\n\n" ++ joinBlocks xs



-- converts a Node into its Haskell type representation.
-- Example: NodeStar -> [Type], NodeQuestion -> Maybe Type
-- Input: Node data type.
-- Output: String representation of the Haskell type
nodeType :: Node -> String
nodeType n = case n of
  NodeSymbol (Terminal _) -> "String"
  NodeSymbol (NonTerminal nm) -> cap nm
  NodeSymbol (SMacro MInt) -> "Int"
  NodeSymbol (SMacro MAlpha) -> "String"
  NodeSymbol (SMacro MNewline) -> "Char"
  NodeParam x -> [x] 
  NodeTok a -> nodeType a
  NodeStar a -> "[" ++ nodeType a ++ "]"
  NodePlus a -> "[" ++ nodeType a ++ "]"
  NodeQuestion a -> "Maybe " ++ parentheses (nodeType a)
  NodeApply nm args -> cap nm ++ (if null args then "" else " " ++ joinWords (map nodeType args))


-- vconverts a Node into its parser expression.
-- example: NodeSymbol (Terminal "+") -> (string "+")
-- Input: Node data type.
-- Output: String representation of parser code.
nodeParser :: Node -> String
nodeParser n = case n of 
    NodeSymbol (Terminal s) -> "(string " ++ show s ++ ")"
    NodeSymbol (NonTerminal name) -> low name
    NodeSymbol (SMacro MInt) -> "int"
    NodeSymbol (SMacro MAlpha) -> "(some alpha)"
    NodeSymbol (SMacro MNewline) -> "(is '\\n')"

    NodeParam x -> [x]

    NodeTok (NodeSymbol (Terminal s)) -> "(stringTok " ++ show s ++ ")"
    NodeTok (NodeSymbol (NonTerminal name)) -> "(tok " ++ low name ++ ")"
    NodeTok (NodeSymbol (SMacro MInt)) -> "(tok int)"
    NodeTok (NodeSymbol (SMacro MAlpha)) -> "(tok (some alpha))"
    NodeTok (NodeSymbol (SMacro MNewline)) -> "(tok (is '\\n'))" 
    NodeTok (NodeParam x) -> "(tok " ++ [x] ++ ")"

    NodeTok other -> "(tok " ++ nodeParser other ++ ")"
    NodeStar a -> "(many " ++ nodeParser a ++ ")"
    NodePlus a -> "(some " ++ nodeParser a ++ ")"
    NodeQuestion a -> "(optional " ++ nodeParser a ++ ")"
    NodeApply name args -> "(" ++ low name ++ concatMap ((" " ++) . nodeParser) args ++ ")"
