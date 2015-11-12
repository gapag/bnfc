{-
    BNF Converter: Java 1.5 Compositional Vistor generator
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

module BNFC.Backend.Java.CFtoComposVisitor (cf2ComposVisitor) where

import Data.List
import BNFC.CF
import BNFC.Backend.Java.Utils (TypeMapping, isBasicType)
import BNFC.Utils ((+++))
import BNFC.Backend.Common.NamedVariables
import BNFC.PrettyPrint

cf2ComposVisitor :: TypeMapping -> String -> String -> CF -> String
cf2ComposVisitor tm packageBase packageAbsyn cf =
  concat [
    header,
    concatMap (prData tm packageAbsyn user) groups,
    "}"]
  where
    user   = fst (unzip (tokenPragmas cf))
    groups = [ g
        | g@(c,_) <- fixCoercions (ruleGroupsInternals cf), not (isList c) ]
    is     = map (prInterface packageAbsyn) groups
    header = unlines [
      "package" +++ packageBase ++ ";"
      , "import" +++ packageAbsyn ++ ".*;"
      , "/** BNFC-Generated Composition Visitor"
      , "*/"
      , ""
      , "public class ComposVisitor<A> implements"
      , intercalate ",\n" $ map ("  "++) is
      , "{"
      ]


prInterface :: String -> (Cat, [Rule]) -> String
prInterface packageAbsyn (cat, _) =
    q ++ ".Visitor<" ++ q ++ ",A>"
  where q = packageAbsyn ++ "." ++ identCat cat

--Traverses a category based on its type.
prData :: TypeMapping -> String -> [UserDef] -> (Cat, [Rule]) -> String
prData tm packageAbsyn user (cat, rules) = unlines
    [ "/* " ++ identCat cat ++ " */"
    , concatMap (render . prRule tm packageAbsyn user cat) rules
    ]
-- | traverses a standard rule.
-- >>> prRule "lang.absyn" [Cat "A"] (Cat "B") (Rule "F" (Cat "B") [NonTerminal (Cat "A"), AnonymousTerminal "+", NonTerminal (ListCat (Cat "B"))])
--     public B visit(lang.absyn.F p, A arg)
--     {
--       String a_ = p.a_;
--       ListB listb_ = new ListB();
--       for (B x : p.listb_)
--       {
--         listb_.add(x.accept(this,arg));
--       }
--       return new lang.absyn.F(a_, listb_);
--     }
prRule :: TypeMapping -> String -> [UserDef] -> Cat -> Rule -> Doc
prRule tm packageAbsyn user cat (Rule fun _ cats)
  | not (isCoercion fun || isDefinedRule fun) = nest 4 $ vcat
    [ "public " <> text(identCat cat) <> " visit(" <> cls <> " p, A arg)"
    , codeblock 2
        [ vcat (map (prCat tm user) cats')
        , "return new" <+> cls <> parens (hsep (punctuate "," vnames)) <> ";" ] ]
  where
    cats' = filter ((/= InternalCat) . fst) (nonTerminals (numVars cats))
    cls = text (packageAbsyn ++ "." ++ fun)
    vnames = map snd cats'
prRule _ _ _ _ _ = ""

-- | Traverses a class's instance variables.
-- >>> prCat [Cat "A"] (Cat "A", "a_")
-- String a_ = p.a_;
-- >>> prCat [] (ListCat (Cat "Integer"), "listinteger_")
-- ListInteger listinteger_ = p.listinteger_;
-- >>> prCat [] (ListCat (Cat "N"), "listn_")
-- ListN listn_ = new ListN();
-- for (N x : p.listn_)
-- {
--   listn_.add(x.accept(this,arg));
-- }
-- >>> prCat [] (Cat "N", "n_")
-- N n_ = p.n_.accept(this, arg);
prCat :: TypeMapping -- ^ Mapping from CF types to Java types
      -> [UserDef]   -- ^ User defined token categories
      -> (Cat, Doc)  -- ^ Variable category and names
      -> Doc         -- ^ Code for visiting the variable
prCat tm user (cat, nt)
  | isBasicType user varType || (isList cat && isBasicType user et) = decl var
  | isList cat = decl ("new" <+> text varType <> "()")
              $$ "for (" <> text et <> " x : " <> var <> ")"
              $$ codeblock 2 [ nt <> ".add(x.accept(this,arg));" ]
  | otherwise = decl (var <> ".accept(this, arg)")
  where
    var     = "p." <> nt
    varType = tm (identCat (normCat cat)) user
    et      = tm (show$normCatOfList cat) user
    decl v  = text varType <+> nt <+> "=" <+> v <> ";"

