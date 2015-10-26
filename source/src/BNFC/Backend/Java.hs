{-
    BNF Converter: Java Top File
    Copyright (C) 2004  Author:  Markus Forsberg, Peter Gammie,
                                 Michael Pellauer, Bjorn Bringert

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

-------------------------------------------------------------------
-- |
-- Module      :  JavaTop
-- Copyright   :  (C)opyright 2003, {markus, aarne, pellauer, peteg, bringert} at cs dot chalmers dot se
-- License     :  GPL (see COPYING for details)
--
-- Maintainer  :  {markus, aarne} at cs dot chalmers dot se
-- Stability   :  alpha
-- Portability :  Haskell98
--
-- Top-level for the Java back end.
--
-- > $Id: JavaTop15.hs,v 1.12 2007/01/08 18:20:23 aarne Exp $
-------------------------------------------------------------------

module BNFC.Backend.Java ( makeJava ) where

-------------------------------------------------------------------
-- Dependencies.
-------------------------------------------------------------------
import System.FilePath (pathSeparator)
import BNFC.Utils
import Data.List ( intercalate)
import BNFC.CF
import BNFC.Options as Options
import BNFC.Backend.Base
import BNFC.Backend.Java.Utils
import BNFC.Backend.Java.CFtoCup15 ( cf2Cup )
import BNFC.Backend.Java.CFtoJLex15
import BNFC.Backend.Java.CFtoAntlr4Lexer
import BNFC.Backend.Java.CFtoAntlr4Parser
import BNFC.Backend.Java.CFtoJavaAbs15 ( cf2JavaAbs )
import BNFC.Backend.Java.CFtoJavaPrinter15
import BNFC.Backend.Java.CFtoVisitSkel15
import BNFC.Backend.Java.CFtoComposVisitor
import BNFC.Backend.Java.CFtoAbstractVisitor
import BNFC.Backend.Java.CFtoFoldVisitor
import BNFC.Backend.Java.CFtoAllVisitor
import BNFC.Backend.Common.NamedVariables (SymEnv, firstLowerCase)
import qualified BNFC.Backend.Common.Makefile as Makefile
import BNFC.PrettyPrint
-------------------------------------------------------------------
-- | Build the Java output.
-------------------------------------------------------------------

-- | This creates the Java files.
makeJava :: SharedOptions -> CF -> MkFiles ()
makeJava options@Options{..} cf =
    do -- Create the package directories if necessary.
       let packageBase = case inPackage of
                             Nothing -> lang
                             Just p -> p ++ "." ++ lang
           packageAbsyn = packageBase ++ "." ++ "Absyn"
           dirBase = pkgToDir packageBase
           dirAbsyn = pkgToDir packageAbsyn
           javaex str = dirBase ++ str +.+ "java"
           bnfcfiles = bnfcVisitorsAndTests packageBase packageAbsyn cf
                            cf2JavaPrinter
                            cf2VisitSkel
                            cf2ComposVisitor
                            cf2AbstractVisitor
                            cf2FoldVisitor
                            cf2AllVisitor
                            ((testclass parselexspec)
                                ((results lexmake) !! 0) -- lexer class
                                ((results parmake) !! 0) -- parser class
                                )
           makebnfcfile x = mkfile (javaex (fst $ x bnfcfiles)) $ (snd $ x bnfcfiles)

       let absynFiles = remDups $ cf2JavaAbs packageBase packageAbsyn cf
           absynBaseNames = map fst absynFiles
           absynFileNames = map (dirAbsyn ++) absynBaseNames
       let writeAbsyn (filename, contents) =
               mkfile (dirAbsyn ++ filename ++ ".java") contents
       mapM_ writeAbsyn absynFiles
       makebnfcfile bprettyprinter
       makebnfcfile bskel
       makebnfcfile bcompos
       makebnfcfile babstract
       makebnfcfile bfold
       makebnfcfile ball
       makebnfcfile btest
---       mkfile ("Test" ++ name) $ "java " ++ dirBase ++ "Test $(1)"
       let (lex, env) = lexfun packageBase cf
       -- where the lexer file is created. lex is the content!
       mkfile (dirBase ++ (inputfile lexmake) ) lex
       liftIO $ putStrLn $ "   (Tested with "+++ (toolname lexmake ) +++ (toolversion lexmake ) +++")"
       -- where the parser file is created.
       mkfile (dirBase ++ (inputfile parmake))
            $ parsefun packageBase packageAbsyn cf env
       liftIO $ putStrLn $ case (supportsEntryPoints parmake) of
                            False -> "   (Parser created only for category " ++ show (firstEntry cf) ++ ")" -- JLex- JFlex specific (in antlr you have a parser for each category)
                            _ -> "(Parser created for all categories)"
       liftIO $ putStrLn $ "   (Tested with " +++ (toolname parmake) +++ (toolversion parmake ) +++ ")"
       Makefile.mkMakefile options $ makefile dirBase dirAbsyn absynFileNames parselexspec
    where
      remDups [] = []
      remDups ((a,b):as) = case lookup a as of
                             Just {} -> remDups as
                             Nothing -> (a, b) : remDups as

      pkgToDir :: String -> FilePath
      pkgToDir s = replace '.' pathSeparator s ++ [pathSeparator]
      parselexspec = parserLexerSelector lang javaLexerParser
      lexfun = cf2lex $ lexer parselexspec
      parsefun = cf2parse $ parser parselexspec
      parmake = makeparserdetails (parser parselexspec)
      lexmake = makelexerdetails (lexer parselexspec)



-- FIXME It's almost certainly better to just feed all the Java source
-- files to javac in one go.
-- Replace with an ANT script?
-- This writes the rules/declares variables for running the lexer/parser.
-- FIXME Must automate the creation of makefile rules based on input-outputs to the generation tools.
-- It needs not to separate brutally the makefile creation.
makefile ::  FilePath -> FilePath -> [String] -> ParserLexerSpecification -> Doc
makefile  dirBase dirAbsyn absynFileNames jlexpar = vcat $
    makeVars [  ("JAVAC", "javac"),
                ("JAVAC_FLAGS", "-sourcepath ."),
                ( "JAVA", "java"),
                ( "JAVA_FLAGS", ""),
            -- parser executable
                ( "PARSER", (executable parmake)),
            -- parser flags
                ( "PARSER_FLAGS", (flags parmake) dirBase),
             -- lexer executable (and flags?)
                ( "LEXER", (executable lexmake)),
                ( "LEXER_FLAGS", (flags lexmake) dirBase)
    ]
    ++
    makeRules [ ("all", [ "test" ], []),
                ( "test", ("absyn" : classes), []),
                ( ".PHONY", ["absyn"],     []),
                ("%.class", [ "%.java" ],  [ runJavac "$^" ]),
                ("absyn",   [absynJavaSrc],[ runJavac "$^" ])
                ]++
    [
  -- running the lexergen: output of lexer -> input of lexer : calls lexer
    let ff = filename lexmake -- name of input file without extension
        dirBaseff = dirBase ++ ff -- prepend directory
        inp = dirBase ++ (inputfile lexmake) in
        Makefile.mkRule (dirBaseff +.+ "java") [ inp ]
        [ "${LEXER} ${LEXER_FLAGS} "++ inp ]

    -- running the parsergen, these there are its outputs
    -- output of parser -> input of parser : calls parser
  , let inp = dirBase ++ (inputfile parmake) in
        Makefile.mkRule (intercalate " " (map (dirBase++) $ (dotJava $ results parmake)))
          [ inp ] $
          [ "${PARSER} ${PARSER_FLAGS} " ++ inp
          ]++ if moveresults parmake then ["mv " ++ (unwords $ dotJava $ results parmake)+++ dirBase ] else []

  -- Class of the output of lexer generator wants java of : output of lexer and parser generator
  ,
    let lexerOutClass = dirBase ++ (filename lexmake) +.+ "class"
        outname = \x -> dirBase ++ x +.+ "java"
        deps = map outname ((results lexmake) ++ (results parmake)) in
    Makefile.mkRule (lexerOutClass) deps []
  ]++
  -- Class of output of parser generator wants java of output of parser generator.
  -- Changing this to the
  --, Makefile.mkRule (dirBase ++ "sym.class") [ dirBase ++ "sym.java" ] []
  --, Makefile.mkRule (dirBase ++ "parser.class") [ dirBase ++ "parser.java", dirBase ++ "sym.java" ] []
  (reverse [Makefile.mkRule tar dep [] | (tar,dep) <- partialParserGoals dirBase (results parmake)])
  -- This remains the same, it's the prettyprinting of BNFC
  ++[ Makefile.mkRule (dirBase ++ "PrettyPrinter.class") [ dirBase ++ "PrettyPrinter.java" ] []

  -- Removes all the class files created anywhere
  , Makefile.mkRule "clean" [] [ "rm -f " ++ dirAbsyn ++ "*.class" ++ " " ++ dirBase ++ "*.class" ]

  -- Remains the same
  , Makefile.mkRule "distclean" [ "vclean" ] []

  -- removes everything
  , Makefile.mkRule "vclean" [] [ " rm -f " ++ absynJavaSrc ++ " " ++ absynJavaClass
                                  , " rm -f " ++ dirAbsyn ++ "*.class"
                                  --     , "rm -f " ++ "Test" ++ name
                                  , " rmdir " ++ dirAbsyn
                                  , " rm -f " ++ unwords (map (dirBase ++) $
                                              [inputfile lexmake,
                                              inputfile parmake]++
                                              dotJava (results lexmake)++
                                              ["VisitSkel.java",
                                              "ComposVisitor.java",
                                              "AbstractVisitor.java",
                                              "FoldVisitor.java",
                                              "AllVisitor.java",
                                              "PrettyPrinter.java",
                                              "Skeleton.java",
                                              "Test.java"]++
                                              (dotJava (results parmake))++
                                              ["*.class"])
                                  , " rm -f Makefile"
                                  , " rmdir -p " ++ dirBase ]
    ]
    where
      makeVars x = [Makefile.mkVar n v | (n,v) <- x]
      makeRules x = [Makefile.mkRule tar dep recipe  | (tar, dep, recipe) <- x]
      parmake           = makeparserdetails (parser jlexpar)
      lexmake           = makelexerdetails (lexer jlexpar)
      absynJavaSrc      = unwords (dotJava absynFileNames)
      absynJavaClass    = unwords (dotClass absynFileNames)
      classes = prependPath dirBase lst
      lst = (dotClass (results lexmake)) ++ [ "PrettyPrinter.class", "Test.class"
          , "ComposVisitor.class", "AbstractVisitor.class"
          , "FoldVisitor.class", "AllVisitor.class"]++
           (dotClass (results parmake)) ++ ["Test.class"]

type TestClass = String -> String -> String -> String -> CF -> String

-- | Test class details for J(F)Lex + CUP
cuptest :: TestClass
cuptest = (javaTest ["java_cup.runtime"]
                    "Throwable"
                    (\_ -> [])
                    (\x -> \i -> x <> i <> ";")
                    (\x -> \i -> x <> i <> ";")
                    (\_ -> \pabs -> \enti -> pabs <> "." <> enti <+> "parse_tree = p."<> "p" <> enti <> "();")
                    "At line \" + String.valueOf(t.l.line_num()) + \", near \\\"\" + t.l.buff() + \"\\\" :"
                    )

-- | Test class details for ANTLR4
-- TODO could make imports better by not importing the whole package (e.g. if they end with ., use star, otherwise don't)
antlrtest :: TestClass
antlrtest = (javaTest ["org.antlr.v4.runtime","org.antlr.v4.runtime.atn","org.antlr.v4.runtime.dfa","java.util"]
                    "TestError"
                    (\x ->antlrErrorHandling x)
                    (\x -> \i ->  vcat [x <> "(new ANTLRInputStream" <> i <>");", "l.addErrorListener(new BNFCErrorListener());"])
                    (\x -> \i -> vcat [x <> "(new CommonTokenStream" <> i <>");", "p.addErrorListener(new BNFCErrorListener());"])
                    (\pbase -> \pabs -> \enti -> vcat [
                        let rulename = getRuleName (show enti)
                            typename = text rulename
                            methodname = text $ firstLowerCase rulename
                        in
                            pbase <> "." <> typename <> "Context pc = p."<>methodname<>"();",
                            "org.antlr.v4.runtime.Token _tkn = p.getInputStream().getTokenSource().nextToken();",
                            "if(_tkn.getType() != -1) throw new TestError(\"Stream does not end with EOF\",_tkn.getLine(),_tkn.getCharPositionInLine());",
                            pabs <> "." <> enti <+> "parse_tree = pc.result;"
                        ])
                    "At line \" + e.line + \", column \" + e.column + \" :"
            )


parserLexerSelector :: String -> JavaLexerParser -> ParserLexerSpecification
parserLexerSelector _ JLexCup = mkParserLexerSpecification cf2JLex cf2cup cuptest
parserLexerSelector _ JFlexCup = mkParserLexerSpecification cf2JFlex cf2cup cuptest
parserLexerSelector l Antlr4 = mkParserLexerSpecification (cf2AntlrLex' l) (cf2AntlrParse' l) antlrtest

data ParserLexerSpecification = ParseLexSpec{
    parser :: CFToParser  ,
    lexer  :: CFToLexer   ,
    testclass :: TestClass
}

-- positional constructor
mkParserLexerSpecification :: CFToLexer -> CFToParser -> TestClass -> ParserLexerSpecification
mkParserLexerSpecification tl tp tc = ParseLexSpec {
    parser = tp,
    lexer = tl,
    testclass = tc
}

-- |CF -> LEXER GENERATION TOOL BRIDGE
-- | type of function translating the CF to an appropriate lexer generation tool.
type CF2LexerFunction = String -> CF -> (Doc, SymEnv)
-- Chooses the translation from CF to the lexer
data CFToLexer = CF2Lex {
    cf2lex :: String -> CF -> (Doc, SymEnv),
    makelexerdetails :: MakeFileDetails
}
-- | Shorthand for positional constructor
mkCFtoLexer :: CF2LexerFunction -> MakeFileDetails -> CFToLexer
mkCFtoLexer fu mf = CF2Lex {
    cf2lex = fu,
    makelexerdetails = mf
}

-- | Instances of cf-lexergen bridges
cf2JLex, cf2JFlex:: CFToLexer
cf2JLex     = mkCFtoLexer BNFC.Backend.Java.CFtoJLex15.cf2jlex' jlexmakedetails
cf2JFlex    = mkCFtoLexer BNFC.Backend.Java.CFtoJLex15.cf2jflex' jflexmakedetails
cf2AntlrLex' :: String -> CFToLexer
cf2AntlrLex' l = mkCFtoLexer BNFC.Backend.Java.CFtoAntlr4Lexer.cf2AntlrLex $ antlrmakedetails $ l++"Lexer" -- TODO


-- | CF -> PARSER GENERATION TOOL BRIDGE
-- | type of function translating the CF to an appropriate parser generation tool.
type CF2ParserFunction = String -> String -> CF -> SymEnv -> String
-- | Chooses the translation from CF to the parser
data CFToParser = CF2Parse {
    cf2parse :: CF2ParserFunction,
    makeparserdetails :: MakeFileDetails
}
-- |  Shorthand for positional constructor
mkCFtoParser :: CF2ParserFunction -> MakeFileDetails -> CFToParser
mkCFtoParser fu mf = CF2Parse {
    cf2parse = fu,
    makeparserdetails = mf
}
-- | Instances of cf-parsergen bridges
cf2cup :: CFToParser
cf2cup = mkCFtoParser BNFC.Backend.Java.CFtoCup15.cf2Cup cupmakedetails
cf2AntlrParse' :: String -> CFToParser
cf2AntlrParse' l = mkCFtoParser BNFC.Backend.Java.CFtoAntlr4Parser.cf2AntlrParse $ antlrmakedetails $ l++"Parser"


-- | shorthand for Makefile command running javac or java
runJavac , runJava:: String -> String
runJava s = mkRunProgram "JAVA" s
runJavac s = mkRunProgram "JAVAC" s

-- | function returning a string executing a program contained in a variable j on input s
mkRunProgram :: String -> String -> String
mkRunProgram j s = (Makefile.refVar j) +++ (Makefile.refVar (j +-+ "FLAGS")) +++ s


type OutputDirectory = String

-- | MAKEFILE DETAILS FROM RUNNING THE PARSER-LEXER GENERATION TOOLS
-- | executable : the string that executes the generation tool
-- | flags : a string containing flags to pass to the tool
-- | filename : the input file to the tool
-- | toolname : name of the tool
-- | toolversion : tool version
-- | supportsEntryPoints : true if the tool is a parser and supports entry points, false otherwise
-- | results : list of names (without extension!) of files resulting
--             from the application of the tool which are relevant to
--             a make rule
-- | moveresults : if true, the files are moved to the base directory, otherwise they are left where they are
data MakeFileDetails = MakeDetails {
    executable::String,
    flags::(OutputDirectory -> String),
    filename::String,
    fileextension::String,
    toolname::String,
    toolversion::String,
    supportsEntryPoints::Bool,
    results :: [String],
    moveresults :: Bool
}

-- Positional constructor.
mkMakeFileDetails :: String -> (OutputDirectory->String) -> String -> String -> String -> String -> Bool -> [String] -> Bool -> MakeFileDetails
mkMakeFileDetails x f fil ex tn tv sup res mr = MakeDetails{
    executable = x,
    flags = f,
    filename = fil,
    fileextension = ex,
    toolname = tn,
    toolversion = tv,
    supportsEntryPoints = sup,
    results = res, -- Note: the first class is the tool's main class,
                   -- that is, the one whose objects represent the
                   -- tool in a Java program
    moveresults = mr
}

mapEmpty :: a->String
mapEmpty = (\_ -> "")

-- Instances of makefile details.
cupmakedetails, jflexmakedetails, jlexmakedetails :: MakeFileDetails

jlexmakedetails  = mkMakeFileDetails
                        (runJava "JLex.Main")
                        mapEmpty
                        "Yylex"
                        ""
                        "JLex"
                        "1.2.6."
                        False
                        ["Yylex"]
                        False

jflexmakedetails = mkMakeFileDetails
                        "jflex"
                        mapEmpty
                        "Yylex"
                        ""
                        "JFlex"
                        "1.4.3"
                        False
                        ["Yylex"]
                        False


cupmakedetails = mkMakeFileDetails
                    (runJava "java_cup.Main")
                    (\_ -> "-nopositions -expect 100")
                    "_cup"
                    "cup"
                    "CUP"
                    "0.10k"
                    False
                    ["parser", "sym"]
                    True


antlrmakedetails :: String -> MakeFileDetails
antlrmakedetails l = mkMakeFileDetails
    (runJava "org.antlr.v4.Tool")
    (\x -> intercalate " " $ map (+++(take ((length x) - 1 ) x )) [ "-lib", "-package"])
    l
    "g4"
    "ANTLRv4"
    "4.5.1"
    True
    [l]
    False


prependPath , appendExtension :: String -> [String] -> [String]
prependPath s fi = [s ++ x | x<- fi]
appendExtension s fi = [x+.+s | x<- fi]

dotJava,dotClass :: [String] -> [String]
dotJava a = appendExtension "java" a
dotClass a = appendExtension "class" a


type CFToJava = String -> String -> CF -> String
-- | Contains the pairs filename/content for all the non-abstract syntax files generated by BNFC
data BNFCGeneratedEntities = BNFCGenerated {
    bprettyprinter :: (String, String),
    btest :: (String, String),
    bcompos :: (String, String),
    babstract :: (String, String),
    bfold :: (String,  String),
    ball :: (String,  String),
    bskel :: (String,  String)
}

-- | Positional constructor.
bnfcVisitorsAndTests :: String -> String -> CF ->
                            CFToJava ->  CFToJava -> CFToJava -> CFToJava -> CFToJava -> CFToJava -> CFToJava -> BNFCGeneratedEntities
bnfcVisitorsAndTests pbase pabsyn cf cf0 cf1 cf2 cf3 cf4 cf5 cf6 =
 BNFCGenerated{
    bprettyprinter = ( "PrettyPrinter" , app cf0),
    bskel = ( "VisitSkel", app cf1),
    bcompos = ( "ComposVisitor" , app cf2),
    babstract = ( "AbstractVisitor" , app cf3),
    bfold = ( "FoldVisitor", app cf4),
    ball = ( "AllVisitor", app cf5),
    btest = ( "Test" , app cf6)

 }
    where app x = x pbase pabsyn cf


-- bnfcGenerated = [ "PrettyPrinter", "Test", "ComposVisitor", "AbstractVisitor", "FoldVisitor", "AllVisitor"]

inputfile x = (filename x) ++ case fileextension x of
                                        "" -> ""
                                        a -> "."++a

-- |  constructs the rules regarding the parser in the makefile
partialParserGoals :: String -> [String] -> [(String, [String])]
partialParserGoals _ [] = []
partialParserGoals dbas (x:rest) = ((dbas++x+.+"class"),(map (\y ->dbas++y+.+"java") (x:rest))):(partialParserGoals dbas rest)


-- | Creates the Test.java class.
-- imports      : list of imported packages
-- err          : name of the exception thrown in case of parsing failure
-- lexerconstruction : function formulating the construction of the lexer object
-- parserconstruction : as above, for parser object
-- invocation   : function formulating the invocation of the parser tool within Java
-- errmsg       : error string output in consequence of a parsing failure
-- lexer        : class of the lexer
-- parser       : class of the parser
-- packageBase  : package where the non-abstract syntax classes are created
-- packageAbsyn : package where the abstract syntax classes are created
-- cf           : the CF bundle
javaTest :: [Doc] -> String -> (String -> [Doc]) ->
            (Doc -> Doc -> Doc) ->
            (Doc -> Doc -> Doc)->
            (Doc -> Doc -> Doc -> Doc) ->
            String ->
            TestClass
javaTest imports err errhand
    lexerconstruction
    parserconstruction
    invocation
    errmsg
    lexer parser packageBase packageAbsyn cf = render $ vcat $
    [
      "package" <+> text packageBase <> ";"
      , "import" <+> text packageBase <> ".*;"
      , "import" <+> text packageAbsyn <> ".*;"
      , "import java.io.*;"
    ]++
--      , "import java_cup.runtime.*;"
        (map importfun imports)
        ++ (errhand err)
    ++[ ""
      , "public class Test"
      , codeblock 2 [
        lx <+> "l;"
        ,px <+> "p;"
        ,""
        ,"public Test(String[] args)"
        , codeblock 2 [
            "try"
            , codeblock 2 ([
                "Reader input;"
                ,"if (args.length == 0) input = new InputStreamReader(System.in);"
                ,"else input = new FileReader(args[0]);"
                ,"l = new "<>(lexerconstruction lx "(input)")
            ])
            ,"catch(IOException e)"
            ,codeblock 2 [
                "System.err.println(\"Error: File not found: \" + args[0]);"
                ,"System.exit(1);"
            ]
            ,"p = new "<> (parserconstruction px "(l)")
        ]
        ,""
        ,"public" <+> (text packageAbsyn) <> "." <> absentity <+>"parse() throws Exception"
        , codeblock 2 [
            "/* The default parser is the first-defined entry point. */"
            , "/* You may want to change this. Other options are: */"
            , "/* " <> fsep (punctuate "," (showOpts (tail eps))) <> " */"
            , invocation px (text packageAbsyn) absentity
            , "System.out.println();"
            , "System.out.println(\"Parse Succesful!\");"
            , "System.out.println();"
            , "System.out.println(\"[Abstract Syntax]\");"
            , "System.out.println();"
            , "System.out.println(PrettyPrinter.show(parse_tree));"
            , "System.out.println();"
            , "System.out.println(\"[Linearized Tree]\");"
            , "System.out.println();"
            , "System.out.println(PrettyPrinter.print(parse_tree));"
            , "return parse_tree;"
        ]
        , ""
        , "public static void main(String args[]) throws Exception"
        , codeblock 2 [
            "Test t = new Test(args);"
            ,"try"
            ,codeblock 2 [
                "t.parse();"
            ]
            ,"catch("<>(text err)<+>"e)"
            ,codeblock 2 [
                 "System.err.println(\""<>(text errmsg)<>"\");"
                , "System.err.println(\"     \" + e.getMessage());"
                , "System.exit(1);"
            ]
        ]
      ]
    ]
    where
        importfun x = "import" <+> x <> ".*;"
        lx = text lexer
        px = text parser

        absentity = text $ (show def)
        eps = allEntryPoints cf
        def = head eps
        showOpts [] = []
        showOpts (x:xs) | normCat x /= x = showOpts xs
                        | otherwise      = text ('p' : identCat x) : showOpts xs


-- | Error handling in ANTLR.
-- By default, ANTLR does not stop after any parsing error and attempts to go on,
-- delivering what it has been able to parse. It does not throw any exception, unlike
-- J(F)lex+CUP.
-- To make the test class behave as with J(F)lex+CUP the additional code is needed.
antlrErrorHandling :: String -> [Doc]
antlrErrorHandling te = ["class"<+>tedoc<+>"extends RuntimeException"
    ,codeblock 2 ["int line;"
        ,"int column;"
        ,"public"<+>tedoc<>"(String msg, int l, int c)"
        ,codeblock 2 ["super(msg);" ,"line = l;" ,"column = c;"]]
    ,"class BNFCErrorListener implements ANTLRErrorListener"
    ,codeblock 2 [ "@Override"
        ,"public void syntaxError(Recognizer<?, ?> recognizer, Object o, int i, int i1, String s, RecognitionException e)"
        ,codeblock 2[ "throw new"<+>tedoc<>"(s,i,i1);"]
    , "@Override"
        ,"public void reportAmbiguity(Parser parser, DFA dfa, int i, int i1, boolean b, BitSet bitSet, ATNConfigSet atnConfigSet)"
        ,codeblock 2[ "throw new"<+>tedoc<>"(\"Ambiguity at\",i,i1);" ]
    ,"@Override"
        ,"public void reportAttemptingFullContext(Parser parser, DFA dfa, int i, int i1, BitSet bitSet, ATNConfigSet atnConfigSet)"
        ,codeblock 2 [{-"throw new"<+>tedoc<>"(\"Attempting full context\",i,i1);"-}]
    ,"@Override"
        ,"public void reportContextSensitivity(Parser parser, DFA dfa, int i, int i1, int i2, ATNConfigSet atnConfigSet)"
        ,codeblock 2 [ {-"throw new"<+>tedoc<>"(\"Context sensitivity\",i,i1);"-}]
        ]
    ]
    where tedoc = text te


