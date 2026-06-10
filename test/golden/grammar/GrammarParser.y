{
{-# LANGUAGE DeriveDataTypeable #-}
module GrammarParser where
import qualified Data.Generics as Gen
import qualified GrammarLexer as L (Token(..), PosToken(..), AlexPosn(..), alexScanTokens)
}

%name parseGrammar
%tokentype { L.PosToken }
%monad { Either String }
%error { parseError }

%token

rtk__eof { L.PosToken _ L.EndOfFile }
tok_Clause_dummy_14 { L.PosToken _ L.Tk__tok_Clause_dummy_14 }
tok_Grammar_dummy_15 { L.PosToken _ L.Tk__tok_Grammar_dummy_15 }
tok_IdList_dummy_13 { L.PosToken _ L.Tk__tok_IdList_dummy_13 }
tok_ImportsOpt_dummy_12 { L.PosToken _ L.Tk__tok_ImportsOpt_dummy_12 }
tok_Name_dummy_11 { L.PosToken _ L.Tk__tok_Name_dummy_11 }
tok_OptDelim_dummy_10 { L.PosToken _ L.Tk__tok_OptDelim_dummy_10 }
tok_Option_dummy_9 { L.PosToken _ L.Tk__tok_Option_dummy_9 }
tok_OptionList_dummy_8 { L.PosToken _ L.Tk__tok_OptionList_dummy_8 }
tok_Rule_dummy_7 { L.PosToken _ L.Tk__tok_Rule_dummy_7 }
tok_RuleList_dummy_6 { L.PosToken _ L.Tk__tok_RuleList_dummy_6 }
tok_StrLit_dummy_5 { L.PosToken _ L.Tk__tok_StrLit_dummy_5 }
tok__tilde__16 { L.PosToken _ L.Tk__tok__tilde__16 }
tok__pipe__11 { L.PosToken _ L.Tk__tok__pipe__11 }
tok_imports_2 { L.PosToken _ L.Tk__tok_imports_2 }
tok_grammar_0 { L.PosToken _ L.Tk__tok_grammar_0 }
tok__symbol_symmacro_9 { L.PosToken _ L.Tk__tok__symbol_symmacro_9 }
tok__symbol_shortcuts_6 { L.PosToken _ L.Tk__tok__symbol_shortcuts_6 }
tok__symbol__15 { L.PosToken _ L.Tk__tok__symbol__15 }
tok__eql__3 { L.PosToken _ L.Tk__tok__eql__3 }
tok__semi__1 { L.PosToken _ L.Tk__tok__semi__1 }
tok__colon__4 { L.PosToken _ L.Tk__tok__colon__4 }
tok__dot__5 { L.PosToken _ L.Tk__tok__dot__5 }
tok__coma__10 { L.PosToken _ L.Tk__tok__coma__10 }
tok__plus__14 { L.PosToken _ L.Tk__tok__plus__14 }
tok__star__13 { L.PosToken _ L.Tk__tok__star__13 }
tok__rparen__8 { L.PosToken _ L.Tk__tok__rparen__8 }
tok__lparen__7 { L.PosToken _ L.Tk__tok__lparen__7 }
tok__exclamation__12 { L.PosToken _ L.Tk__tok__exclamation__12 }
regexplit { L.PosToken _ (L.Tk__regexplit $$) }
bigstr { L.PosToken _ (L.Tk__bigstr $$) }
str { L.PosToken _ (L.Tk__str $$) }
id { L.PosToken _ (L.Tk__id $$) }
qq_Name { L.PosToken _ (L.Tk__qq_Name $$) }
qq_StrLit { L.PosToken _ (L.Tk__qq_StrLit $$) }
qq_OptDelim { L.PosToken _ (L.Tk__qq_OptDelim $$) }
qq_Clause { L.PosToken _ (L.Tk__qq_Clause $$) }
qq_IdList { L.PosToken _ (L.Tk__qq_IdList $$) }
qq_Option { L.PosToken _ (L.Tk__qq_Option $$) }
qq_OptionList { L.PosToken _ (L.Tk__qq_OptionList $$) }
qq_Rule { L.PosToken _ (L.Tk__qq_Rule $$) }
qq_RuleList { L.PosToken _ (L.Tk__qq_RuleList $$) }
qq_ImportsOpt { L.PosToken _ (L.Tk__qq_ImportsOpt $$) }
qq_Grammar { L.PosToken _ (L.Tk__qq_Grammar $$) }

%%

Grammar__top : Grammar rtk__eof { $1 }

Grammar : tok_Grammar_dummy_15 Grammar tok_Grammar_dummy_15 { Ctr__Grammar__0 $2 } |
          tok_Clause_dummy_14 Clause tok_Clause_dummy_14 { Ctr__Grammar__1 $2 } |
          tok_IdList_dummy_13 IdList tok_IdList_dummy_13 { Ctr__Grammar__2 (reverse $2) } |
          tok_ImportsOpt_dummy_12 ImportsOpt tok_ImportsOpt_dummy_12 { Ctr__Grammar__3 $2 } |
          tok_Name_dummy_11 Name tok_Name_dummy_11 { Ctr__Grammar__4 $2 } |
          tok_OptDelim_dummy_10 OptDelim tok_OptDelim_dummy_10 { Ctr__Grammar__5 $2 } |
          tok_Option_dummy_9 Option tok_Option_dummy_9 { Ctr__Grammar__6 $2 } |
          tok_OptionList_dummy_8 OptionList tok_OptionList_dummy_8 { Ctr__Grammar__7 (reverse $2) } |
          tok_Rule_dummy_7 Rule tok_Rule_dummy_7 { Ctr__Grammar__8 $2 } |
          tok_RuleList_dummy_6 RuleList tok_RuleList_dummy_6 { Ctr__Grammar__9 (reverse $2) } |
          tok_StrLit_dummy_5 StrLit tok_StrLit_dummy_5 { Ctr__Grammar__10 $2 }

Grammar : qq_Grammar { Anti_Grammar $1 } |
          tok_grammar_0 StrLit tok__semi__1 ImportsOpt RuleList { Ctr__Grammar__11 $2 $4 (reverse $5) }

Clause5 : qq_Clause { Anti_Clause $1 } |
          tok__lparen__7 Clause tok__rparen__8 { $2 } |
          Name { Ctr__Clause__1 $1 } |
          StrLit { Ctr__Clause__2 $1 } |
          tok__dot__5 { Ctr__Clause__3 } |
          regexplit { Ctr__Clause__4 $1 }

Clause4 : Clause5 tok__star__13 OptDelim { Ctr__Clause__5 $1 $3 } |
          Clause5 tok__plus__14 OptDelim { Ctr__Clause__6 $1 $3 } |
          Clause5 tok__symbol__15 { Ctr__Clause__7 $1 } |
          Clause5 { $1 }

Clause3 : tok__coma__10 Clause4 { Ctr__Clause__9 $2 } |
          tok__exclamation__12 Clause4 { Ctr__Clause__10 $2 } |
          Clause4 { $1 }

Clause2 : Clause2 Clause3 { Ctr__Clause__12 $1 $2 } |
          Clause3 { $1 }

Clause : Clause tok__pipe__11 Clause2 { Ctr__Clause__14 $1 $3 } |
         Clause2 { $1 }

IdList__plus_list_ : ListElem_IdList3 { [$1] } |
                     IdList__plus_list_ tok__coma__10 ListElem_IdList3 { $3 : $1 }

IdList : IdList__plus_list_ { $1 } |
         {- empty -} { [] }

ImportsOpt : qq_ImportsOpt { Anti_ImportsOpt $1 } |
             { Ctr__ImportsOpt__0 } |
             Rule_0 { Ctr__ImportsOpt__1 $1 }

Name : qq_Name { Anti_Name $1 } |
       id { Ctr__Name__0 $1 }

ListElem_IdList3 : qq_IdList { Anti_Name $1 } |
                   Name { $1 }

OptDelim : qq_OptDelim { Anti_OptDelim $1 } |
           { Ctr__OptDelim__0 } |
           Rule_4 { Ctr__OptDelim__1 $1 }

Option : qq_Option { Anti_Option $1 } |
         tok__symbol_shortcuts_6 tok__lparen__7 IdList tok__rparen__8 { Ctr__Option__0 (reverse $3) } |
         tok__symbol_symmacro_9 { Ctr__Option__1 }

ListElem_OptionList2 : qq_OptionList { Anti_Option $1 } |
                       Option { $1 }

OptionList : ListElem_OptionList2 { [$1] } |
             OptionList ListElem_OptionList2 { $2 : $1 }

Rule1 : qq_Rule { Anti_Rule $1 } |
        Name tok__eql__3 Clause tok__semi__1 { Ctr__Rule__0 $1 $3 } |
        Name tok__colon__4 Name tok__eql__3 Clause tok__semi__1 { Ctr__Rule__1 $1 $3 $5 } |
        Name tok__dot__5 Name tok__colon__4 Name tok__eql__3 Clause tok__semi__1 { Ctr__Rule__2 $1 $3 $5 $7 } |
        tok__dot__5 Name tok__colon__4 Name tok__eql__3 Clause tok__semi__1 { Ctr__Rule__3 $2 $4 $6 }

Rule : OptionList Rule1 { Ctr__Rule__4 (reverse $1) $2 } |
       Rule1 { $1 }

ListElem_RuleList1 : qq_RuleList { Anti_Rule $1 } |
                     Rule { $1 }

RuleList : {- empty -} { [] } |
           RuleList ListElem_RuleList1 { $2 : $1 }

Rule_0 : tok_imports_2 bigstr { Ctr__Rule_0__0 $2 }

Rule_4 : tok__tilde__16 Clause5 { Ctr__Rule_4__0 $2 }

StrLit : qq_StrLit { Anti_StrLit $1 } |
         str { Ctr__StrLit__0 $1 }


{
parseError :: [L.PosToken] -> Either String a
parseError [] = Left "Parse error: unexpected end of input"
parseError (L.PosToken (L.AlexPn _ line col) tok : _) =
    Left $ "Parse error at line " ++ show line ++ ", column " ++ show col ++ ": unexpected " ++ showRtkToken tok

-- Render a token the way it appears in the source, for error messages
showRtkToken :: L.Token -> String
showRtkToken L.EndOfFile = "end of input"
showRtkToken L.Tk__tok_Clause_dummy_14 = "'tok_Clause_dummy_14'"
showRtkToken L.Tk__tok_Grammar_dummy_15 = "'tok_Grammar_dummy_15'"
showRtkToken L.Tk__tok_IdList_dummy_13 = "'tok_IdList_dummy_13'"
showRtkToken L.Tk__tok_ImportsOpt_dummy_12 = "'tok_ImportsOpt_dummy_12'"
showRtkToken L.Tk__tok_Name_dummy_11 = "'tok_Name_dummy_11'"
showRtkToken L.Tk__tok_OptDelim_dummy_10 = "'tok_OptDelim_dummy_10'"
showRtkToken L.Tk__tok_Option_dummy_9 = "'tok_Option_dummy_9'"
showRtkToken L.Tk__tok_OptionList_dummy_8 = "'tok_OptionList_dummy_8'"
showRtkToken L.Tk__tok_Rule_dummy_7 = "'tok_Rule_dummy_7'"
showRtkToken L.Tk__tok_RuleList_dummy_6 = "'tok_RuleList_dummy_6'"
showRtkToken L.Tk__tok_StrLit_dummy_5 = "'tok_StrLit_dummy_5'"
showRtkToken L.Tk__tok__tilde__16 = "'~'"
showRtkToken L.Tk__tok__pipe__11 = "'|'"
showRtkToken L.Tk__tok_imports_2 = "'imports'"
showRtkToken L.Tk__tok_grammar_0 = "'grammar'"
showRtkToken L.Tk__tok__symbol_symmacro_9 = "'@symmacro'"
showRtkToken L.Tk__tok__symbol_shortcuts_6 = "'@shortcuts'"
showRtkToken L.Tk__tok__symbol__15 = "'?'"
showRtkToken L.Tk__tok__eql__3 = "'='"
showRtkToken L.Tk__tok__semi__1 = "';'"
showRtkToken L.Tk__tok__colon__4 = "':'"
showRtkToken L.Tk__tok__dot__5 = "'.'"
showRtkToken L.Tk__tok__coma__10 = "','"
showRtkToken L.Tk__tok__plus__14 = "'+'"
showRtkToken L.Tk__tok__star__13 = "'*'"
showRtkToken L.Tk__tok__rparen__8 = "')'"
showRtkToken L.Tk__tok__lparen__7 = "'('"
showRtkToken L.Tk__tok__exclamation__12 = "'!'"
showRtkToken (L.Tk__regexplit v) = "regexplit " ++ show v
showRtkToken (L.Tk__bigstr v) = "bigstr " ++ show v
showRtkToken (L.Tk__str v) = "str " ++ show v
showRtkToken (L.Tk__id v) = "id " ++ show v
showRtkToken (L.Tk__qq_Name v) = "qq_Name " ++ show v
showRtkToken (L.Tk__qq_StrLit v) = "qq_StrLit " ++ show v
showRtkToken (L.Tk__qq_OptDelim v) = "qq_OptDelim " ++ show v
showRtkToken (L.Tk__qq_Clause v) = "qq_Clause " ++ show v
showRtkToken (L.Tk__qq_IdList v) = "qq_IdList " ++ show v
showRtkToken (L.Tk__qq_Option v) = "qq_Option " ++ show v
showRtkToken (L.Tk__qq_OptionList v) = "qq_OptionList " ++ show v
showRtkToken (L.Tk__qq_Rule v) = "qq_Rule " ++ show v
showRtkToken (L.Tk__qq_RuleList v) = "qq_RuleList " ++ show v
showRtkToken (L.Tk__qq_ImportsOpt v) = "qq_ImportsOpt " ++ show v
showRtkToken (L.Tk__qq_Grammar v) = "qq_Grammar " ++ show v

data Grammar = Ctr__Grammar__0 Grammar |
               Ctr__Grammar__1 Clause |
               Ctr__Grammar__2 IdList |
               Ctr__Grammar__3 ImportsOpt |
               Ctr__Grammar__4 Name |
               Ctr__Grammar__5 OptDelim |
               Ctr__Grammar__6 Option |
               Ctr__Grammar__7 OptionList |
               Ctr__Grammar__8 Rule |
               Ctr__Grammar__9 RuleList |
               Ctr__Grammar__10 StrLit |
               Anti_Grammar String |
               Ctr__Grammar__11 StrLit ImportsOpt RuleList
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Clause = Anti_Clause String |
              Ctr__Clause__1 Name |
              Ctr__Clause__2 StrLit |
              Ctr__Clause__3 |
              Ctr__Clause__4 String |
              Ctr__Clause__5 Clause OptDelim |
              Ctr__Clause__6 Clause OptDelim |
              Ctr__Clause__7 Clause |
              Ctr__Clause__9 Clause |
              Ctr__Clause__10 Clause |
              Ctr__Clause__12 Clause Clause |
              Ctr__Clause__14 Clause Clause
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type IdList = [Name]
data ImportsOpt = Anti_ImportsOpt String |
                  Ctr__ImportsOpt__0 |
                  Ctr__ImportsOpt__1 Rule_0
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Name = Anti_Name String |
            Ctr__Name__0 String
            deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data OptDelim = Anti_OptDelim String |
                Ctr__OptDelim__0 |
                Ctr__OptDelim__1 Rule_4
                deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Option = Anti_Option String |
              Ctr__Option__0 IdList |
              Ctr__Option__1
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type OptionList = [Option]
data Rule = Anti_Rule String |
            Ctr__Rule__0 Name Clause |
            Ctr__Rule__1 Name Name Clause |
            Ctr__Rule__2 Name Name Name Clause |
            Ctr__Rule__3 Name Name Clause |
            Ctr__Rule__4 OptionList Rule
            deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type RuleList = [Rule]
data Rule_0 = Ctr__Rule_0__0 String
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_4 = Ctr__Rule_4__0 Clause
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data StrLit = Anti_StrLit String |
              Ctr__StrLit__0 String
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
}