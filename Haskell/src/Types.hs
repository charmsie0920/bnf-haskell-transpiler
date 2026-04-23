module Types
  (ADT(..)
  , Rule(..)
  , RuleHeader(..)
  , Alt(..)
  , Seq(..)
  , Node(..)
  , Symbol(..)
  , Macro(..)
  , Name
  ) where

-- names
type Name = String

-- Macro lists the built-in token classes allowed in the grammar
-- used inside 'Symbol' and then inside 'Node' on the right-hand side of rules.
data Macro = MInt | MAlpha | MNewline
    deriving (Eq, Show)

-- Symbol represents the basic building blocks of the grammar:
-- 'Terminal String'      — a quoted token, e.g. Terminal "+" or Terminal "("
-- 'NonTerminal Name'   — a reference to another rule, e.g. NonTerminal "expr"
-- 'SMacro Macro'         — one of the built-in macros, e.g. SMacro MInt
data Symbol = Terminal String | NonTerminal Name | SMacro Macro
    deriving (Eq, Show)

--Node is a right-hand side element, which can be:
-- a Symbol, a parameter, a modifier (tok, *, +, ?), or a rule call with arguments.
data Node =
    NodeSymbol Symbol -- plain symbol (terminal / nonterminal / macro)
    | NodeTok Node -- tok x: x parsed with tok
    | NodeStar Node -- x*: zero or more of x
    | NodePlus Node -- x+: one or more of x
    | NodeParam Char -- [x]: x is a parameter
    | NodeQuestion Node -- x?: zero or one of x
    | NodeApply Name [Node] -- <name>(args...): application of a rule
    deriving (Eq, Show)

--ADT is a list of grammar rules
--used newtype to make it distinct from other types
newtype ADT = ADT [Rule]
    deriving (Show)

--Seq is a sequence of symbols
newtype Seq = Seq [Node] -- sequence of symbols
    deriving (Eq, Show)

newtype Alt = Alt [Seq] -- sequence separated by |
    deriving (Eq, Show)

-- a grammar rule: name ::= alt
data Rule = Rule
    { ruleHead :: RuleHeader
    , ruleDef :: Alt
    } -- name ::= alt
    deriving (Eq, Show)

data RuleHeader = RuleHeader
    { headName :: Name
    , headParams :: [Char] -- unique single lowercase letters
    }
    deriving (Eq, Show)