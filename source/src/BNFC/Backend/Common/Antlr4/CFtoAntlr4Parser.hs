{-
    BNF Converter: Antlr4 Java 1.8 Generator
    Copyright (C) 2004  Author:  Markus Forsberg, Michael Pellauer,
                                 Bjorn Bringert

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{-
   **************************************************************
    BNF Converter Module

    Description   : This module generates the ANTLR .g4 input file. It
                    follows the same basic structure of CFtoHappy.

    Author        : Gabriele Paganelli (gapag@distruzione.org),


    License       : GPL (GNU General Public License)

    Created       : 15 Oct, 2015

    Modified      :


   **************************************************************
-}
module BNFC.Backend.Common.Antlr4.CFtoAntlr4Parser ( cf2AntlrParse , defaultAntlrParameters, getRuleName) where
import BNFC.Backend.Common.MultipleParserGenerationTools (ToolParameters (..))
import BNFC.CF
import BNFC.Backend.Java.Utils
import BNFC.Backend.Common.NamedVariables
import BNFC.Utils ( (+++), (+.+), mkName, NameStyle(..))
import BNFC.Backend.Common.Antlr4.AntlrComponents

-- Type declarations
type Rules       = [(NonTerminal,[(Pattern, Fun, Action)])]
type Pattern     = String
type Action      = String
type MetaVar     = (String, Cat)

defaultAntlrParameters = ToolParams{
    targetReservedWords  = ["grammar"],
    commentString  = "",
    multilineComment  = \x -> x,
    preservePositions = True,
    packageBase       = "",
    packageAbsyn      = "",
    generateAction    = \_ _ _ _ _ -> "",
    lexerMembers     = "",
    parserMembers    = "",
    lexerHeader = "" ,
    parserHeader = ""
}

-- | Creates the ANTLR parser grammar for this CF.
--The environment comes from CFtoAntlr4Lexer
cf2AntlrParse :: ToolParameters -> CF -> SymEnv -> String
cf2AntlrParse tpar cf env = unlines
    [ header
    , parserHeaderContent $ (parserHeader tpar)
    , parserMembersContent $ (parserMembers tpar)
    , tokens
    , prRules tpar (packageAbsyn tpar) (rulesForAntlr4 tpar cf env)
    ]
  where
    header :: String
    header = unlines
        [ "// This ANTLRv4 file was machine-generated by BNFC"
        , "parser grammar" +++ identifier ++ "Parser;"
        ]
    tokens :: String
    tokens = unlines
        [ "options {"
        , "  tokenVocab = "++identifier++"Lexer;"
        , "}"
        ]
    identifier = getLastInPackage $ packageBase tpar

--The following functions are a (relatively) straightforward translation
--of the ones in CFtoHappy.hs
rulesForAntlr4 :: ToolParameters -> CF -> SymEnv -> Rules
rulesForAntlr4 tpar cf env = map mkOne getrules
  where
    getrules          = ruleGroups cf
    mkOne (cat,rules) = constructRule tpar cf env rules cat

-- | For every non-terminal, we construct a set of rules. A rule is a sequence of
-- terminals and non-terminals, and an action to be performed.
constructRule :: ToolParameters -> CF -> SymEnv -> [Rule] -> NonTerminal -> (NonTerminal,[(Pattern, Fun, Action)])
constructRule tpar cf env rules nt =
    (nt, [ (p , funRule r , generateJavaAction tpar nt (funRule r) (revM b m) b)
          | (index ,r0) <- zip [1..(length rules)] rules,
          let (b,r) = if isConsFun (funRule r0) && elem (valCat r0) revs
                          then (True, revSepListRule r0)
                          else (False, r0)
              (p,m) = generatePatterns tpar index env r])
 where
   revM False = id
   revM True  = reverse
   revs       = cfgReversibleCats cf

-- Generates a string containing the semantic action.
generateJavaAction :: ToolParameters -> NonTerminal -> Fun -> [MetaVar]
               -> Bool   -- ^ Whether the list should be reversed or not.
                         --   Only used if this is a list rule.
               -> Action
generateJavaAction gen nt f ms rev = (generateAction gen) gen nt f ms rev

-- | Generate patterns and a set of metavariables indicating
-- where in the pattern the non-terminal
-- >>> generatePatterns 2 [] (Rule "myfun" (Cat "A") [])
-- (" /* empty */ ",[])
-- >>> generatePatterns 3 [("def", "_SYMB_1")] (Rule "myfun" (Cat "A") [Right "def", Left (Cat "B")])
-- ("_SYMB_1 p_3_2=b ",[("p_3_2",B)])
generatePatterns :: ToolParameters -> Int -> SymEnv -> Rule -> (Pattern,[MetaVar])
generatePatterns tpar ind env r = case rhsRule r of
    []  -> (" /* empty */ ",[])
    its -> (mkIt 1 its, metas its)
 where
    mkIt _ [] = []
    mkIt n (i:is) = case i of
        Left c -> "p_" ++show ind++"_"++ show (n :: Int) ++ "="++ c'
            +++ mkIt (n+1) is
          where
              c' = case c of
                  TokenCat "Ident"   -> "IDENT"
                  TokenCat "Integer" -> "INTEGER"
                  TokenCat "Char"    -> "CHAR"
                  TokenCat "Double"  -> "DOUBLE"
                  TokenCat "String"  -> "STRING"
                  _                  -> if isTokenCat c
                                          then identCat c
                                          else firstLowerCase
                                                (getRuleName tpar (identCat c))
        Right s -> case lookup s env of
            (Just x) -> x +++ mkIt (n+1) is
            (Nothing) -> mkIt n is
    metas its = [("p_" ++ show ind ++"_"++ show i, category)
                    | (i,Left category) <- zip [1 :: Int ..] its]

-- | Puts together the pattern and actions and returns a string containing all
-- the rules.
prRules :: ToolParameters -> String -> Rules -> String
prRules _ _ [] = []
prRules tpar packabs ((_, []):rs) = prRules tpar packabs rs
prRules tpar packabs ((nt,(p, fun, a):ls):rs) =
    preamble ++ ";\n" ++ prRules tpar packabs rs
  where
    preamble          = unwords [ nt'
                        , "returns"
                        , "["
                        , packabs+.+normcat
                        , "result"
                        , "]"
                        , ":"
                        , p
                        , "{"++a++"}"
                        , "#"
                        , antlrRuleLabel fun
                        , '\n' : pr ls
                        ]
    alternative (p',fun',a')
                      = unwords ["  |", p', "{"++a'++"}", "#"
                        , antlrRuleLabel fun']
    catid             = identCat nt
    normcat           = identCat (normCat nt)
    nt'               = getRuleName tpar $ firstLowerCase catid
    pr []             = []
    pr (k:ls) = unlines [alternative k] ++ pr ls
    antlrRuleLabel fnc
      | isNilFun fnc   = catid ++ "_Empty"
      | isOneFun fnc   = catid ++ "_AppendLast"
      | isConsFun fnc  = catid ++ "_PrependFirst"
      | isCoercion fnc = "Coercion_" ++ catid
      | otherwise      = getLabelName fnc
  
getRuleName tpar z = if x `elem` (targetReservedWords tpar) then z ++ "_" else z
                where x = firstLowerCase z

getLabelName = mkName ["Rule"] CamelCase