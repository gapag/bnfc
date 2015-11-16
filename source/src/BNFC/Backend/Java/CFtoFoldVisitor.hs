{-
    BNF Converter: Java 1.5 Fold Vistor generator
    Copyright (C) 2006 Bjorn Bringert
    Based on CFtoVisitSkel.hs, Copyright (C) 2004-2006  Michael Pellauer

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

module BNFC.Backend.Java.CFtoFoldVisitor (cf2FoldVisitor) where

import BNFC.CF
import BNFC.Backend.Java.Utils(isBasicType, TypeMapping)
import BNFC.Utils ((+++))
import BNFC.Backend.Common.NamedVariables
import BNFC.PrettyPrint

cf2FoldVisitor :: TypeMapping -> String -> String -> CF -> String
cf2FoldVisitor tm packageBase packageAbsyn cf =
  unlines
    ["package" +++ packageBase ++ ";",
     "",
     "import" +++ packageAbsyn ++ ".*;",
     "import java.util.Collections;",
     "import java.util.List;",
     "import java.util.ArrayList;",
     "",
     "/** BNFC-Generated Fold Visitor */",
     "public abstract class FoldVisitor<R,A> implements AllVisitor<R,A> {",
     "    public abstract R leaf(A arg);",
     "    public abstract R combine(R x, R y, A arg);",
     "",
     concatMap (prData tm packageAbsyn user) groups,
     "}"]
  where
    user = fst (unzip (tokenPragmas cf))
    groups = [ g | g@(c,_) <- fixCoercions (ruleGroupsInternals cf), not (isList c || isIndentationEnter c) ]

--Traverses a category based on its type.
prData :: TypeMapping -> String -> [UserDef] -> (Cat, [Rule]) -> String
prData tm packageAbsyn user (cat, rules) = unlines
    [ "/* " ++ identCat cat ++ " */"
    , concatMap (prRule tm packageAbsyn user cat) rules
    ]

--traverses a standard rule.
prRule :: TypeMapping -> String -> [UserDef] -> Cat -> Rule -> String
prRule tm packageAbsyn user _ (Rule fun _ cats)
    | not (isCoercion fun || isDefinedRule fun) = unlines $
  ["    public R visit(" ++ cls ++ " p, A arg) {",
   "      R r = leaf(arg);"]
  ++  map ("      "++) visitVars
  ++ ["      return r;",
      "    }"]
   where
    cats' = filter ((/= InternalCat) . fst) (nonTerminals (numVars cats))
    cls = packageAbsyn ++ "." ++ fun
    visitVars = lines $ render $ vcat $ map (prCat tm user) cats'
prRule _ _ _ _ _ = ""

-- | Traverses a class's instance variables.
-- >>> prCat [Cat "A"] (Cat "A", "a_")
-- <BLANKLINE>
-- >>> prCat [] (ListCat (Cat "Integer"), "listinteger_")
-- <BLANKLINE>
-- >>> prCat [] (ListCat (Cat "N"), "listn_")
-- for (N x : p.listn_)
-- {
--   r = combine(x.accept(this, arg), r, arg);
-- }
-- >>> prCat [] (Cat "N", "n_")
-- r = combine(p.n_.accept(this, arg), r, arg);
prCat :: TypeMapping
      -> [UserDef]
      -> (Cat, Doc) -- ^ Variable category and name
      -> Doc        -- ^ Code for visiting the variable
prCat tm user (cat,nt)
    | isBasicType user varType || (isList cat && isBasicType user et) = empty
    | isList cat = vcat
        [ "for (" <> text et <> " x : " <> var <> ")"
        , codeblock 2 [ "r = combine(x.accept(this, arg), r, arg);" ] ]
    | otherwise = "r = combine(" <> var <> ".accept(this, arg), r, arg);"
      where
      var = "p." <> nt
      varType = tm (identCat (normCat cat)) user
      et      = tm (show$normCatOfList cat) user
