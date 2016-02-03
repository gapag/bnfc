{-
    BNF Converter: ocamlyacc Generator
    Copyright (C) 2005  Author:  Kristofer Johannisson

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

-- based on BNFC Haskell backend


module BNFC.Backend.OCaml.CFtoOCamlYacc
       (
       cf2ocamlyacc, terminal
       )
        where

import BNFC.CF
import Data.List (intersperse,nub)
import Data.Char

import BNFC.Utils ((+++))
import BNFC.Backend.OCaml.OCamlUtil

-- Type declarations

type Pattern     = String
type Action      = String
type MetaVar     = String

-- The main function, that given a CF
-- generates a ocamlyacc module.
cf2ocamlyacc :: String -> String -> String -> CF -> String
cf2ocamlyacc name absName lexName cf
 = unlines
    [header name absName lexName cf,
    declarations absName cf,
    "%%",
    rules cf
    ]


header :: String -> String -> String -> CF -> String
header _ absName _ cf = unlines
         ["/* This ocamlyacc file was machine-generated by the BNF converter */",
          "%{",
          "open " ++ absName,
          "open Lexing",
          "",
          definedRules cf,
          "%}"
         ]

definedRules :: CF -> String
definedRules cf = unlines [mkDef f xs e | FunDef f xs e <- cfgPragmas cf]
    where
        mkDef f xs e = 
            "let " ++ f ++ " " ++ mkTuple xs ++ " = " ++ ocamlExp e
            where
                ocamlExp :: Exp -> String
                ocamlExp (App s es) = s ++ ' ' : mkTuple (map ocamlExp es)
                ocamlExp (LitInt i) = show i
                ocamlExp (LitDouble d) = show d
                ocamlExp (LitChar c) = "\'" ++ c : "\'"
                ocamlExp (LitString s) = "\"" ++ s ++ "\""

declarations :: String -> CF -> String
declarations absName cf = unlines
    [tokens (cfgSymbols cf) (reservedWords cf),
     specialTokens cf,
     entryPoints absName cf
    ]

tokens :: [String] -> [String] -> String
tokens symbols reswords = unlines
    [
        if (length reswords) > 0
            then "%token" +++ concat (intersperse " " (map ("TOK_" ++) reswords))
        else ""
        ,
        concatMap (\(s,n) -> "\n%token SYMB" ++ (show n) +++ "/*" +++ s +++ "*/")
            (zip symbols [1..])
    ]

-- | map a CF terminal into a ocamlyacc token
terminal :: CF -> String -> String
terminal cf s  |  s `elem` reservedWords cf = "TOK_" ++ s
terminal cf s  = case lookup s (zip (cfgSymbols cf) [1..]) of
    Just i -> "SYMB" ++ show i
    Nothing -> error $ "CFtoOCamlYacc: terminal " ++ show s ++ " not defined in CF."


-- | map a CF nonterminal into a ocamlyacc symbol
nonterminal :: Cat -> String
nonterminal c = map spaceToUnderscore (fixType c)
    where spaceToUnderscore ' ' = '_'
          spaceToUnderscore x = x

specialTokens :: CF -> String
specialTokens cf = unlines ("%token TOK_EOF" : map aux (nub $ ["Ident","String","Integer","Double","Char"] ++ map show (literals cf)))
    where aux cat = "%token" +++ (case cat of
            "Ident"   -> "<string>"
            "String"  -> "<string>"
            "Integer" -> "<int>"
            "Double"  -> "<float>"
            "Char"    -> "<char>"
            _         -> "<string>"      )
           +++ "TOK_" ++ cat


entryPoints :: String -> CF -> String
entryPoints absName cf = unlines $
    ("%start" +++
        concat (intersperse " " (map epName eps)))
    :
    (map typing eps)
    where eps = (nub $ map normCat (allEntryPoints cf))
          typing :: Cat -> String
          typing c = "%type" +++ "<" ++ qualify c ++ ">" +++ epName c
          qualify c = if c `elem` [ TokenCat "Integer", TokenCat "Double", TokenCat "Char",
                                    TokenCat "String", ListCat (TokenCat "Integer"),
                                    ListCat (TokenCat "Double"),
                                    ListCat (TokenCat "Char"),
                                    ListCat (TokenCat "String") ]
                      then fixType c
                      else absName ++ "." ++ fixType c

epName :: Cat -> String
epName c = "p" ++ capitalize (nonterminal c)
            where capitalize s = case s of
                    [] -> []
                    c:cs -> toUpper c : cs

entryPointRules :: CF -> String
entryPointRules cf = unlines $ map mkRule (nub $ map normCat (allEntryPoints cf))
    where
        mkRule :: Cat -> String
        mkRule s = unlines [
            epName s ++ " : " ++ nonterminal s ++ " TOK_EOF { $1 }",
            "  | error { raise (BNFC_Util.Parse_error (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) };"
            ]

rules :: CF -> String
rules cf = unlines [
    entryPointRules cf,
    (unlines $ map (prOne . mkOne) (ruleGroups cf)),
    specialRules cf
    ]
    where
        mkOne (cat,rules) = constructRule cf rules cat
        prOne (_,[]) = [] -- nt has only internal use
        prOne (nt,((p,a):ls)) =
          unwords [nt', ":" , p, "{", a, "}", "\n" ++ pr ls] ++ ";\n"
         where
           nt' = nonterminal nt
           pr [] = []
           pr ((p,a):ls) =
             unlines [(concat $ intersperse " " ["  |", p, "{", a , "}"])] ++ pr ls



-- For every non-terminal, we construct a set of rules. A rule is a sequence of
-- terminals and non-terminals, and an action to be performed
-- As an optimization, a pair of list rules [C] ::= "" | C k [C]
-- is left-recursivized into [C] ::= "" | [C] C k.
-- This could be generalized to cover other forms of list rules.
constructRule :: CF -> [Rule] -> NonTerminal -> (NonTerminal,[(Pattern,Action)])
constructRule cf rules nt = (nt,[(p,generateAction nt (funRule r) (mkFlip b m)) |
     r0 <- rules,
     let (b,r) = if isConsFun (funRule r0) && elem (valCat r0) revs
                   then (True,revSepListRule r0)
                 else (False,r0),
     let (p,m) = generatePatterns cf r])
 where
   revs = cfgReversibleCats cf
   mkFlip doit xs = case xs of
       a:b:rest | doit -> b:a:rest
       _ -> xs



-- Generates a string containing the semantic action.
-- An action can for example be: Sum $1 $2, that is, construct an AST
-- with the constructor Sum applied to the two metavariables $1 and $2.
generateAction :: NonTerminal -> Fun -> [MetaVar] -> Action
generateAction _ f ms = (if isCoercion f then "" else f') +++ mkTuple ms
    where f' = case f of -- ocaml cons is somehow not a standard infix oper, right?
                    "(:[])" -> "(fun x -> [x])"
                    "(:)" -> "(fun (x,xs) -> x::xs)"
                    _ -> f


generatePatterns :: CF -> Rule -> (Pattern,[MetaVar])
generatePatterns cf r = case rhsRule r of
  []  -> ("/* empty */",[])
  its -> (unwords (map mkIt its), metas its)
 where
   mkIt i = case i of
     Left c -> nonterminal c
     Right (Anonymous s) -> terminal cf s
     Right (Indentation s) -> terminal cf s
   metas its = [revIf c ('$': show i) | (i,Left c) <- zip [1 ::Int ..] its]
   revIf c m = if (not (isConsFun (funRule r)) && elem c revs)
                 then ("(List.rev " ++ m ++ ")")
               else m  -- no reversal in the left-recursive Cons rule itself
   revs = cfgReversibleCats cf





specialRules :: CF -> String
specialRules cf = unlines $
                  map aux (literals cf)
 where
   aux cat =
     case cat of
         TokenCat "Ident"   -> "ident : TOK_Ident  { Ident $1 };"
         TokenCat "String"  -> "string : TOK_String { $1 };"
         TokenCat "Integer" -> "int :  TOK_Integer  { $1 };"
         TokenCat "Double"  -> "float : TOK_Double  { $1 };"
         TokenCat "Char"    -> "char : TOK_Char { $1 };"
         own       -> (fixType own) ++ " : TOK_" ++ show own ++
                      " { " ++ show own ++ " ("++ posn ++ "$1)};"
                -- PCC: take "own" as type name? (manual says newtype)
      where -- ignore position categories for now
         posn = "" -- if isPositionCat cf cat then "mkPosToken " else ""



