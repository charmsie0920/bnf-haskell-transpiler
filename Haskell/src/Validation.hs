module Validation (validate, getValidADT) where

import Types(RuleHeader(..), Rule(..), ADT(..), Alt(..), Seq(..), Node(..), Symbol(..))
-- import Data.Map (Map, fromList, lookup)
import qualified Data.Map as Map

-- Main validation function that runs all error checks on the ADT.
-- Input: ADT (the entire parsed grammar).
-- Output: list of error messages (duplicates, undefined rules, left recursion, wrong param count)
validate :: ADT -> [String]
validate (ADT rules) =
    --call findAndRemoveDuplicates and pass rules as arg, return a tuple
    let (duplicateErrors, noDuplicates) = findAndRemoveDuplicates rules
        names = map getRuleName noDuplicates --map takes a function and a list, applies function to each element in list
        undefinedErrors = findUndefinedErrors noDuplicates names --list of errors stored in undefinedErrors
        leftRecursionErrors = getLeftRecursionErrors noDuplicates --list of errors stored in leftRecursionErrors
        paramCount = buildParamCountList noDuplicates --build map from rules without duplicates
        paramCountErrors = findParamCountErrors noDuplicates paramCount --find errors using the map
    in duplicateErrors ++ undefinedErrors ++ leftRecursionErrors ++ paramCountErrors --join all errors into one list and return

-- Cleans and returns a valid ADT by removing rules that cause errors.
-- Keeps only non-duplicate, valid, and non-left-recursive rules.
getValidADT :: ADT -> ADT
getValidADT (ADT rules) = let 
    (_, noDuplicates) = findAndRemoveDuplicates rules
    in 
        ADT (iterate noDuplicates) --final return value of the getValidADT function
    where 
        iterate :: [Rule] -> [Rule]
        iterate currentRule = 
            let (rulesToDelete, _) = getWrongRules currentRule
            in 
                if null rulesToDelete --recursion base case, return currentRule
                    then currentRule
                    --if rulesToDelete is not empty, filter out the bad rules and recurse
                    else 
                        --r is each rule in currentRule, check if its name is in rulesToDelete
                        let newRules = filter(\r -> not(getRuleName r `elem` rulesToDelete)) currentRule
                        in
                            --recurse with newRules
                            iterate newRules

--Finds rules that are either undefined or left-recursive.
-- Input: list of rules.
-- Output: tuple of (list of wrong rule names, empty list placeholder).
getWrongRules :: [Rule ] -> ([String], [String])                      
getWrongRules rules = 
    let names = map getRuleName rules
        --filters main rules list to only those that have undefined names, maps to get their names, removes duplicates
        -- filter -> map -> removeDups
        undefinedRules = removeDuplicates $ map getRuleName $ filter (\r -> hasUndefined r names) rules
        -- build dependency graph, filter main rules list to only left-recursive rules, map to get their names, remove duplicates
        leftRecursiveRules = removeDuplicates $ map getRuleName $ filter (isLeftRecursive (constructRuleHeaderMap rules)) rules
        wrongRules = removeDuplicates (undefinedRules ++ leftRecursiveRules)
    in (wrongRules, [])

-- gets the name of a rule from its RuleHeader.
-- Input: one Rule.
-- Output: the rule's name as a string.
getRuleName :: Rule -> String
getRuleName (Rule (RuleHeader name _ )_) = name 


-- fidns and removes duplicate rules by name.
-- Input: list of rules.
-- Output: (list of error messages for duplicates, list of unique rules).
findAndRemoveDuplicates :: [Rule] -> ([String], [Rule])
findAndRemoveDuplicates rules = 
    let (marked, duplicates) = foldl
            (\(markedList, dupList) rule ->
                let name = getRuleName rule
                in if name `elem` markedList
                    then (markedList, name : dupList)
                    else (name : markedList, dupList)
            ) ([], []) rules
        errors = map (\name -> "Duplicate rule: " ++ name) (removeDuplicates duplicates)
        uniqueRules = removeDuplicatesBy (\r1 r2 -> getRuleName r1 == getRuleName r2) rules
    in (errors, uniqueRules)

-- finds undefined nonterminals in all rules.
-- Input: list of rules, and list of known names.
-- Output: list of error messages for undefined nonterminals.
findUndefinedErrors :: [Rule] -> [String] -> [String]
findUndefinedErrors rules names =
    concatMap (\r ->
       if hasUndefined r names
       then ["Undefined nonterminal: " ++ getRuleName r]
       else []
    ) rules

-- checks if a single rule uses any undefined nonterminal symbols.
-- Input: one rule and a list of known names.
-- Output: True if rule has undefined nonterminals, otherwise False.
hasUndefined :: Rule -> [String] -> Bool
hasUndefined rule names =
    let rhsNames = listRhsNonTerminals rule
    in not (all (\name -> name `elem` names) rhsNames)

-- Lists all nonterminals used on the right-hand side of a rule.
-- Input: one rule.
-- Output: list of all nonterminal names appearing in that rule.
listRhsNonTerminals :: Rule -> [String]
listRhsNonTerminals (Rule _ (Alt alts)) = 
    removeDuplicates (concat (map getNTSeq alts ))
    where
        getNTSeq :: Seq -> [String]
        getNTSeq (Seq nodes) = removeDuplicates (concat (map getNTNode nodes))
        getNTNode :: Node -> [String]
        getNTNode (NodeSymbol (NonTerminal name)) = [name]
        getNTNode (NodeApply name args) = removeDuplicates (name: (concat (map getNTNode args)))
        getNTNode (NodeTok a) = getNTNode a
        getNTNode (NodeStar a) = getNTNode a
        getNTNode (NodePlus a) = getNTNode a
        getNTNode (NodeQuestion a) = getNTNode a
        getNTNode _ = []

-- Detects and reports rules that are left-recursive.
-- Input: list of rules.
-- Output: list of error messages for rules with left recursion.
getLeftRecursionErrors :: [Rule] -> [String]
getLeftRecursionErrors rules =
    let graph = constructRuleHeaderMap rules in concatMap (
        \r -> let name = getRuleName r
             in if isLeftRecursive graph r
                then ["Left recursion in: " ++ name]
                else []) rules


type RuleHeaderMap = [(String, [String])]

--  Builds a dependency graph (RuleHeaderMap) from all rules.
-- Input: list of rules.
-- Output: list of (ruleName, firstNonTerminalsCalled).
constructRuleHeaderMap :: [Rule] -> RuleHeaderMap
constructRuleHeaderMap rules =
    map (\r -> (getRuleName r, getNTHeaders r)) rules
    where
        getNTHeaders :: Rule -> [String]
        getNTHeaders (Rule _ (Alt alts)) =
            concatMap getSeqHeaders alts
        
        getSeqHeaders :: Seq -> [String]
        getSeqHeaders (Seq []) = []
        getSeqHeaders (Seq (n:ns)) = case n of 
            NodeSymbol (NonTerminal name) -> [name]
            NodeApply name _ -> [name]
            _ -> []

-- Checks if a rule is left-recursive by searching for cycles in the dependency graph.
-- Input: the dependency graph and the rule to test.
-- Output: True if the rule is left-recursive, otherwise False.
isLeftRecursive :: RuleHeaderMap -> Rule -> Bool
isLeftRecursive ruleMap rule = 
    let startName = getRuleName rule
    in fst (checkForCycle startName [startName] [] startName)
    where
        checkForCycle :: String -> [String] -> [String] -> String -> (Bool, [String])
        checkForCycle startName recursionStack visited currentNode = 
                    case lookup currentNode ruleMap of
                        Nothing -> (False, visited)
                        Just nextRules -> 
                            let (foundACycle, nodesAfterCheck) = foldl
                                    (\(cycleAlreadyFound, visitedNodesAccumulator) nextRule ->
                                        if cycleAlreadyFound then 
                                            (True, visitedNodesAccumulator)
                                        else if nextRule `elem` recursionStack then
                                            -- A cycle was found! (e.g., stack=[C,B,A], nextRule=B)
                                            -- The cycle path is the part of the stack from the top
                                            -- down to 'nextRule'.
                                            let (path, _) = break (== nextRule) recursionStack
                                                cycleNodes = (nextRule : path)
                                            -- Check if our *original* 'startName' is part of this cycle.
                                            in 
                                                if startName `elem` cycleNodes
                                                    then (True, visitedNodesAccumulator) -- Yes, it's left-recursive
                                                    else (False, visitedNodesAccumulator) -- No, this is a different cycle

                                        else if nextRule `elem` visitedNodesAccumulator then 
                                            (False, visitedNodesAccumulator) 
                                        else
                                            let (cycleInBranch, visitedListAfterBranch) = checkForCycle startName (nextRule : recursionStack) visitedNodesAccumulator nextRule
                                            in (cycleInBranch, visitedListAfterBranch)
                                    ) 
                                    (False, visited)
                                    nextRules
                            in (foundACycle, currentNode : nodesAfterCheck)

-- Removes duplicate elements from a list (keeps first occurrence).
-- Input: any list with Eq elements.
-- Output: list with unique elements.
removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates list = reverse (foldl
    (\acc item ->
        if item `elem` acc then acc
        else item : acc
    ) [] list)

-- removes duplicates based on a custom comparison function.
-- Input: equality function and list.
-- Output: list with unique elements determined by the equality check.
removeDuplicatesBy :: (a -> a -> Bool) -> [a] -> [a]
removeDuplicatesBy eq list = reverse (foldl
    (\acc item ->
    if any (\x -> eq x item) acc then acc
        else item : acc
    ) [] list)


--Part F: extension for improving validations
-- This section checks that every rule call uses the correct number of parameters.
-- For example: if <pair(a,b)> is defined with 2 parameters, then calling <pair(a)> is an error.

type ParamCountList = [(String, Int)]


-- | Build a list that links rule names to how many parameters they have.
-- Input: list of rules.
-- Output: list of (ruleName, parameterCount).
buildParamCountList :: [Rule] -> ParamCountList
buildParamCountList ruleList =
    -- Map each rule into a (name, count) tuple.
    [(ruleName, length params) | Rule (RuleHeader ruleName params) _ <- ruleList]


-- | Look up how many parameters a given rule name expects.
-- Input: the rule name and the list of (ruleName, paramCount) pairs.
-- Output: Just paramCount if found, Nothing if not found.
lookupParamCount :: String -> ParamCountList -> Maybe Int
lookupParamCount _ [] = Nothing
lookupParamCount target ((name, count):rest)
    | target == name = Just count
    | otherwise      = lookupParamCount target rest


-- | Check all rules for incorrect parameter counts.
-- Input: list of rules and the parameter list built above.
-- Output: list of error messages.
findParamCountErrors :: [Rule] -> ParamCountList -> [String]
findParamCountErrors ruleList paramList =
    -- Check every rule and remove any duplicate errors.
    removeDuplicates $ concatMap (checkRuleParams paramList) ruleList


-- | Check one rule for parameter count errors.
checkRuleParams :: ParamCountList -> Rule -> [String]
checkRuleParams paramList (Rule (RuleHeader ruleName _) (Alt alternatives)) =
    -- Go through each alternative and check all its sequences.
    concatMap (checkSeqParams ruleName paramList) alternatives


-- | Check one sequence for wrong parameter usage.
checkSeqParams :: String -> ParamCountList -> Seq -> [String]
checkSeqParams currentRule paramList (Seq nodeList) =
    -- Go through every node in this sequence.
    concatMap (checkNodeParams currentRule paramList) nodeList


-- | Main recursive function to check each node.
-- Input: current rule name, parameter list, and a single node.
-- Output: list of error messages.
checkNodeParams :: String -> ParamCountList -> Node -> [String]
checkNodeParams currentRule paramList node =
    case node of
        -- If node is a function-like nonterminal call: <pair(a,b)>
        NodeApply calledRule args ->
            let 
                -- 1. Recursively check the arguments themselves.
                argErrors = concatMap (checkNodeParams currentRule paramList) args

                -- 2. Compare actual vs expected parameter count.
                currentError =
                    case lookupParamCount calledRule paramList of
                        Nothing -> []  -- undefined rule (checked elsewhere)
                        Just expected ->
                            let actual = length args
                            in if actual == expected
                                then []
                                else [ "In rule <" ++ currentRule ++ ">: Nonterminal <" ++ calledRule 
                                       ++ "> called with " ++ show actual
                                       ++ " arguments, but expected " ++ show expected
                                     ]
            in currentError ++ argErrors

        --nodes contain other nodes then recurse.
        NodeTok subNode      -> checkNodeParams currentRule paramList subNode
        NodeStar subNode     -> checkNodeParams currentRule paramList subNode
        NodePlus subNode     -> checkNodeParams currentRule paramList subNode
        NodeQuestion subNode -> checkNodeParams currentRule paramList subNode

        -- Base cases, no parameters, no errors.
        NodeSymbol _ -> []
        NodeParam _  -> []

