module PrintGrammar(showGrammar)
    where

import Text.PrettyPrint.HughesPJ
import Parser

showGrammar = render.grammar2Doc

indent :: Doc -> Doc
indent = nest 2


nameIt :: Doc -> [Doc] -> Doc
nameIt s lst = parens $ vcat (s:(map indent lst))

grammar2Doc :: NormalGrammar -> Doc
grammar2Doc (Grammar name rules) =
    nameIt (text name) (map rule2Doc rules)

nameEq :: String -> String -> Doc
nameEq name val = (text name) <> (text "=") <> (quotes$text val)

rule2Doc :: NormalRule -> Doc
rule2Doc (Rule dtn dfn rn cl) =
    (nameIt ((text rn) <+> (nameEq "type" dtn) <+> (nameEq "func" dfn))
            [(clause2Doc cl)]) <> (text "\n")

nameSmall :: String -> Doc -> Doc
nameSmall id val = parens $ (text id) <+> val

clause2Doc (Id s)               = nameSmall "Id" (text s)
clause2Doc (StrLit s)           = nameSmall "StrLit" (quotes$text s)
clause2Doc (Dot)                = text "(.)"
clause2Doc (RegExpLit s)        = nameSmall "RegExpLit" (quotes$text s)
clause2Doc (Star cl Nothing)    = nameIt (text "*") [(clause2Doc cl)]
clause2Doc (Star cl (Just cl1)) = nameIt (text "*") (map clause2Doc [cl, cl1])
clause2Doc (Plus cl Nothing)    = nameIt (text "+") [(clause2Doc cl)]
clause2Doc (Plus cl (Just cl1)) = nameIt (text "+") (map clause2Doc [cl, cl1])
clause2Doc (Alt cls)            = nameIt (text "Alt") (map clause2Doc cls)
clause2Doc (Seq n cls)          = nameIt ((text "Seq") <+> (quotes$text n)) (map clause2Doc cls)
clause2Doc (Opt cl)             = nameSmall "?"    (clause2Doc cl)
clause2Doc (Lifted cl)          = nameSmall "^" (clause2Doc cl)
clause2Doc (Ignore cl)          = nameSmall "!" (clause2Doc cl)
