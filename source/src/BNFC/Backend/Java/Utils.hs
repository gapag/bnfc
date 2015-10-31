module BNFC.Backend.Java.Utils
 ( isBasicType
 , getRuleName
 , isReserved
 , getLabelName
 , TypeMapping
 , strictTypename
 , flexibleTypename

     )
 where
import BNFC.Backend.Common.NamedVariables
import BNFC.Utils ( mkName, NameStyle(..))

javaReserved = [
    "abstract" 	,"continue" 	,"for" 	,"new" 	,"switch"
    ,"assert"  ,"default" 	,"goto" 	,"package" 	,"synchronized"
    ,"boolean" 	,"do" 	,"if" 	,"private" 	,"this"
    ,"break" 	,"double" 	,"implements" 	,"protected" 	,"throw"
    ,"byte" 	,"else" 	,"import" 	,"public" 	,"throws"
    ,"case" 	,"enum" 	,"instanceof" 	,"return" 	,"transient"
    ,"catch" 	,"extends" 	,"int" 	,"short" 	,"try"
    ,"char" 	,"final" 	,"interface" 	,"static" 	,"void"
    ,"class" 	,"finally" 	,"long" 	,"strictfp" 	,"volatile"
    ,"const" 	,"float" 	,"native" 	,"super" 	,"while"
    ]

getRuleName z = if x `elem` ("grammar" : javaReserved) then z ++ "_" else z
                where x = firstLowerCase z

getLabelName = mkName ["Rule"] CamelCase

isReserved :: String -> Bool
isReserved x = x `elem` javaReserved

-- Checks if something is a basic or user-defined type.
isBasicType :: [UserDef] -> String -> Bool
isBasicType user v =
    v `elem` (map show user ++ ["Integer"
                               , "Character"
                               , "String"
                               , "Double"
                               , "java.math.BigInteger"
                               , "java.math.BigDecimal"
                               ])

type TypeMapping = String -> [UserDef] -> String

--This makes up for the fact that there's no typedef in Java
strictTypename :: String -> [UserDef] -> String
strictTypename t user | t == "Ident"            = "String"
                | t == "Char"             = "Character"
                | t `elem` map show user  = "String"
                | t == "Integer"          = "Integer"
                | t == "Double"           = "Double"
                | otherwise               = t

flexibleTypename :: String -> [UserDef] -> String
flexibleTypename t user | t == "Integer"  = "java.math.BigInteger"
                | t == "Double"           = "java.math.BigDecimal"
                | otherwise               = strictTypename t user