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
regexplit { L.PosToken _ (L.Tk__regexplit _) }
bigstr { L.PosToken _ (L.Tk__bigstr _) }
str { L.PosToken _ (L.Tk__str _) }
id { L.PosToken _ (L.Tk__id _) }
qq_Name { L.PosToken _ (L.Tk__qq_Name _) }
qq_StrLit { L.PosToken _ (L.Tk__qq_StrLit _) }
qq_OptDelim { L.PosToken _ (L.Tk__qq_OptDelim _) }
qq_Clause { L.PosToken _ (L.Tk__qq_Clause _) }
qq_IdList { L.PosToken _ (L.Tk__qq_IdList _) }
qq_Option { L.PosToken _ (L.Tk__qq_Option _) }
qq_OptionList { L.PosToken _ (L.Tk__qq_OptionList _) }
qq_Rule { L.PosToken _ (L.Tk__qq_Rule _) }
qq_RuleList { L.PosToken _ (L.Tk__qq_RuleList _) }
qq_ImportsOpt { L.PosToken _ (L.Tk__qq_ImportsOpt _) }
qq_Grammar { L.PosToken _ (L.Tk__qq_Grammar _) }

%%

Grammar__top : Grammar rtk__eof { $1 }

Grammar : tok_Grammar_dummy_15 Grammar tok_Grammar_dummy_15 { Ctr__Grammar__0 (rtkPosOf $1) $2 } |
          tok_Clause_dummy_14 Clause tok_Clause_dummy_14 { Ctr__Grammar__1 (rtkPosOf $1) $2 } |
          tok_IdList_dummy_13 IdList tok_IdList_dummy_13 { Ctr__Grammar__2 (rtkPosOf $1) (reverse $2) } |
          tok_ImportsOpt_dummy_12 ImportsOpt tok_ImportsOpt_dummy_12 { Ctr__Grammar__3 (rtkPosOf $1) $2 } |
          tok_Name_dummy_11 Name tok_Name_dummy_11 { Ctr__Grammar__4 (rtkPosOf $1) $2 } |
          tok_OptDelim_dummy_10 OptDelim tok_OptDelim_dummy_10 { Ctr__Grammar__5 (rtkPosOf $1) $2 } |
          tok_Option_dummy_9 Option tok_Option_dummy_9 { Ctr__Grammar__6 (rtkPosOf $1) $2 } |
          tok_OptionList_dummy_8 OptionList tok_OptionList_dummy_8 { Ctr__Grammar__7 (rtkPosOf $1) (reverse $2) } |
          tok_Rule_dummy_7 Rule tok_Rule_dummy_7 { Ctr__Grammar__8 (rtkPosOf $1) $2 } |
          tok_RuleList_dummy_6 RuleList tok_RuleList_dummy_6 { Ctr__Grammar__9 (rtkPosOf $1) (reverse $2) } |
          tok_StrLit_dummy_5 StrLit tok_StrLit_dummy_5 { Ctr__Grammar__10 (rtkPosOf $1) $2 }

Grammar : qq_Grammar { Anti_Grammar (tkVal_qq_Grammar $1) } |
          tok_grammar_0 StrLit tok__semi__1 ImportsOpt RuleList { Ctr__Grammar__11 (rtkPosOf $1) $2 $4 (reverse $5) }

Clause5 : qq_Clause { Anti_Clause (tkVal_qq_Clause $1) } |
          tok__lparen__7 Clause tok__rparen__8 { $2 } |
          Name { Ctr__Clause__1 (rtkPosOf $1) $1 } |
          StrLit { Ctr__Clause__2 (rtkPosOf $1) $1 } |
          tok__dot__5 { Ctr__Clause__3 (rtkPosOf $1) } |
          regexplit { Ctr__Clause__4 (rtkPosOf $1) (tkVal_regexplit $1) }

Clause4 : Clause5 tok__star__13 OptDelim { Ctr__Clause__5 (rtkPosOf $1) $1 $3 } |
          Clause5 tok__plus__14 OptDelim { Ctr__Clause__6 (rtkPosOf $1) $1 $3 } |
          Clause5 tok__symbol__15 { Ctr__Clause__7 (rtkPosOf $1) $1 } |
          Clause5 { $1 }

Clause3 : tok__coma__10 Clause4 { Ctr__Clause__9 (rtkPosOf $1) $2 } |
          tok__exclamation__12 Clause4 { Ctr__Clause__10 (rtkPosOf $1) $2 } |
          Clause4 { $1 }

Clause2 : Clause2 Clause3 { Ctr__Clause__12 (rtkPosOf $1) $1 $2 } |
          Clause3 { $1 }

Clause : Clause tok__pipe__11 Clause2 { Ctr__Clause__14 (rtkPosOf $1) $1 $3 } |
         Clause2 { $1 }

IdList__plus_list_ : ListElem_IdList3 { [$1] } |
                     IdList__plus_list_ tok__coma__10 ListElem_IdList3 { $3 : $1 }

IdList : IdList__plus_list_ { $1 } |
         {- empty -} { [] }

ImportsOpt : qq_ImportsOpt { Anti_ImportsOpt (tkVal_qq_ImportsOpt $1) } |
             { Ctr__ImportsOpt__0 rtkNoPos } |
             Rule_0 { Ctr__ImportsOpt__1 (rtkPosOf $1) $1 }

Name : qq_Name { Anti_Name (tkVal_qq_Name $1) } |
       id { Ctr__Name__0 (rtkPosOf $1) (tkVal_id $1) }

ListElem_IdList3 : qq_IdList { Anti_Name (tkVal_qq_IdList $1) } |
                   Name { $1 }

OptDelim : qq_OptDelim { Anti_OptDelim (tkVal_qq_OptDelim $1) } |
           { Ctr__OptDelim__0 rtkNoPos } |
           Rule_4 { Ctr__OptDelim__1 (rtkPosOf $1) $1 }

Option : qq_Option { Anti_Option (tkVal_qq_Option $1) } |
         tok__symbol_shortcuts_6 tok__lparen__7 IdList tok__rparen__8 { Ctr__Option__0 (rtkPosOf $1) (reverse $3) } |
         tok__symbol_symmacro_9 { Ctr__Option__1 (rtkPosOf $1) }

ListElem_OptionList2 : qq_OptionList { Anti_Option (tkVal_qq_OptionList $1) } |
                       Option { $1 }

OptionList : ListElem_OptionList2 { [$1] } |
             OptionList ListElem_OptionList2 { $2 : $1 }

Rule1 : qq_Rule { Anti_Rule (tkVal_qq_Rule $1) } |
        Name tok__eql__3 Clause tok__semi__1 { Ctr__Rule__0 (rtkPosOf $1) $1 $3 } |
        Name tok__colon__4 Name tok__eql__3 Clause tok__semi__1 { Ctr__Rule__1 (rtkPosOf $1) $1 $3 $5 } |
        Name tok__dot__5 Name tok__colon__4 Name tok__eql__3 Clause tok__semi__1 { Ctr__Rule__2 (rtkPosOf $1) $1 $3 $5 $7 } |
        tok__dot__5 Name tok__colon__4 Name tok__eql__3 Clause tok__semi__1 { Ctr__Rule__3 (rtkPosOf $1) $2 $4 $6 }

Rule : OptionList Rule1 { Ctr__Rule__4 (rtkPosOf (reverse $1)) (reverse $1) $2 } |
       Rule1 { $1 }

ListElem_RuleList1 : qq_RuleList { Anti_Rule (tkVal_qq_RuleList $1) } |
                     Rule { $1 }

RuleList : {- empty -} { [] } |
           RuleList ListElem_RuleList1 { $2 : $1 }

Rule_0 : tok_imports_2 bigstr { Ctr__Rule_0__0 (rtkPosOf $1) (tkVal_bigstr $2) }

Rule_4 : tok__tilde__16 Clause5 { Ctr__Rule_4__0 (rtkPosOf $1) $2 }

StrLit : qq_StrLit { Anti_StrLit (tkVal_qq_StrLit $1) } |
         str { Ctr__StrLit__0 (rtkPosOf $1) (tkVal_str $1) }


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

-- Source position of a node: every constructor except the Anti_* splice
-- artifacts stores the position of its alternative's first symbol in its
-- first field. Positions are transparent for equality and ordering, so two
-- ASTs that differ only in source positions (e.g. a quasi-quote parsed at
-- compile time vs the same construct parsed at run time) compare equal.
newtype RtkPos = RtkPos L.AlexPosn deriving (Show, Gen.Data, Gen.Typeable)
instance Eq RtkPos where _ == _ = True
instance Ord RtkPos where compare _ _ = EQ

-- The position used where no source token exists: empty productions, empty
-- lists, absent optionals and Anti_* quasi-quote splices
rtkNoPos :: RtkPos
rtkNoPos = RtkPos (L.AlexPn 0 0 0)

class RtkPosOf a where
    rtkPosOf :: a -> RtkPos
instance RtkPosOf L.PosToken where
    rtkPosOf (L.PosToken p _) = RtkPos p
instance RtkPosOf a => RtkPosOf [a] where
    rtkPosOf (x : _) = rtkPosOf x
    rtkPosOf []      = rtkNoPos
instance RtkPosOf a => RtkPosOf (Maybe a) where
    rtkPosOf (Just x) = rtkPosOf x
    rtkPosOf Nothing  = rtkNoPos
-- A Char carries no position; this also covers String token payloads
instance RtkPosOf Char where
    rtkPosOf _ = rtkNoPos

-- Recover a token's payload from the whole positioned token: %token
-- bindings keep the L.PosToken so semantic actions can read its position
tkVal_regexplit :: L.PosToken -> String
tkVal_regexplit (L.PosToken _ (L.Tk__regexplit v)) = v
tkVal_regexplit t = error ("rtk internal error: token regexplit expected, got " ++ showRtkToken (L.ptToken t))
tkVal_bigstr :: L.PosToken -> String
tkVal_bigstr (L.PosToken _ (L.Tk__bigstr v)) = v
tkVal_bigstr t = error ("rtk internal error: token bigstr expected, got " ++ showRtkToken (L.ptToken t))
tkVal_str :: L.PosToken -> String
tkVal_str (L.PosToken _ (L.Tk__str v)) = v
tkVal_str t = error ("rtk internal error: token str expected, got " ++ showRtkToken (L.ptToken t))
tkVal_id :: L.PosToken -> String
tkVal_id (L.PosToken _ (L.Tk__id v)) = v
tkVal_id t = error ("rtk internal error: token id expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Name :: L.PosToken -> String
tkVal_qq_Name (L.PosToken _ (L.Tk__qq_Name v)) = v
tkVal_qq_Name t = error ("rtk internal error: token qq_Name expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_StrLit :: L.PosToken -> String
tkVal_qq_StrLit (L.PosToken _ (L.Tk__qq_StrLit v)) = v
tkVal_qq_StrLit t = error ("rtk internal error: token qq_StrLit expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_OptDelim :: L.PosToken -> String
tkVal_qq_OptDelim (L.PosToken _ (L.Tk__qq_OptDelim v)) = v
tkVal_qq_OptDelim t = error ("rtk internal error: token qq_OptDelim expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Clause :: L.PosToken -> String
tkVal_qq_Clause (L.PosToken _ (L.Tk__qq_Clause v)) = v
tkVal_qq_Clause t = error ("rtk internal error: token qq_Clause expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_IdList :: L.PosToken -> String
tkVal_qq_IdList (L.PosToken _ (L.Tk__qq_IdList v)) = v
tkVal_qq_IdList t = error ("rtk internal error: token qq_IdList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Option :: L.PosToken -> String
tkVal_qq_Option (L.PosToken _ (L.Tk__qq_Option v)) = v
tkVal_qq_Option t = error ("rtk internal error: token qq_Option expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_OptionList :: L.PosToken -> String
tkVal_qq_OptionList (L.PosToken _ (L.Tk__qq_OptionList v)) = v
tkVal_qq_OptionList t = error ("rtk internal error: token qq_OptionList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Rule :: L.PosToken -> String
tkVal_qq_Rule (L.PosToken _ (L.Tk__qq_Rule v)) = v
tkVal_qq_Rule t = error ("rtk internal error: token qq_Rule expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_RuleList :: L.PosToken -> String
tkVal_qq_RuleList (L.PosToken _ (L.Tk__qq_RuleList v)) = v
tkVal_qq_RuleList t = error ("rtk internal error: token qq_RuleList expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_ImportsOpt :: L.PosToken -> String
tkVal_qq_ImportsOpt (L.PosToken _ (L.Tk__qq_ImportsOpt v)) = v
tkVal_qq_ImportsOpt t = error ("rtk internal error: token qq_ImportsOpt expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Grammar :: L.PosToken -> String
tkVal_qq_Grammar (L.PosToken _ (L.Tk__qq_Grammar v)) = v
tkVal_qq_Grammar t = error ("rtk internal error: token qq_Grammar expected, got " ++ showRtkToken (L.ptToken t))

data Grammar = Ctr__Grammar__0 RtkPos Grammar |
               Ctr__Grammar__1 RtkPos Clause |
               Ctr__Grammar__2 RtkPos IdList |
               Ctr__Grammar__3 RtkPos ImportsOpt |
               Ctr__Grammar__4 RtkPos Name |
               Ctr__Grammar__5 RtkPos OptDelim |
               Ctr__Grammar__6 RtkPos Option |
               Ctr__Grammar__7 RtkPos OptionList |
               Ctr__Grammar__8 RtkPos Rule |
               Ctr__Grammar__9 RtkPos RuleList |
               Ctr__Grammar__10 RtkPos StrLit |
               Anti_Grammar String |
               Ctr__Grammar__11 RtkPos StrLit ImportsOpt RuleList
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Grammar where
    rtkPosOf (Ctr__Grammar__0 p _) = p
    rtkPosOf (Ctr__Grammar__1 p _) = p
    rtkPosOf (Ctr__Grammar__2 p _) = p
    rtkPosOf (Ctr__Grammar__3 p _) = p
    rtkPosOf (Ctr__Grammar__4 p _) = p
    rtkPosOf (Ctr__Grammar__5 p _) = p
    rtkPosOf (Ctr__Grammar__6 p _) = p
    rtkPosOf (Ctr__Grammar__7 p _) = p
    rtkPosOf (Ctr__Grammar__8 p _) = p
    rtkPosOf (Ctr__Grammar__9 p _) = p
    rtkPosOf (Ctr__Grammar__10 p _) = p
    rtkPosOf (Anti_Grammar _) = rtkNoPos
    rtkPosOf (Ctr__Grammar__11 p _ _ _) = p
data Clause = Anti_Clause String |
              Ctr__Clause__1 RtkPos Name |
              Ctr__Clause__2 RtkPos StrLit |
              Ctr__Clause__3 RtkPos |
              Ctr__Clause__4 RtkPos String |
              Ctr__Clause__5 RtkPos Clause OptDelim |
              Ctr__Clause__6 RtkPos Clause OptDelim |
              Ctr__Clause__7 RtkPos Clause |
              Ctr__Clause__9 RtkPos Clause |
              Ctr__Clause__10 RtkPos Clause |
              Ctr__Clause__12 RtkPos Clause Clause |
              Ctr__Clause__14 RtkPos Clause Clause
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Clause where
    rtkPosOf (Anti_Clause _) = rtkNoPos
    rtkPosOf (Ctr__Clause__1 p _) = p
    rtkPosOf (Ctr__Clause__2 p _) = p
    rtkPosOf (Ctr__Clause__3 p) = p
    rtkPosOf (Ctr__Clause__4 p _) = p
    rtkPosOf (Ctr__Clause__5 p _ _) = p
    rtkPosOf (Ctr__Clause__6 p _ _) = p
    rtkPosOf (Ctr__Clause__7 p _) = p
    rtkPosOf (Ctr__Clause__9 p _) = p
    rtkPosOf (Ctr__Clause__10 p _) = p
    rtkPosOf (Ctr__Clause__12 p _ _) = p
    rtkPosOf (Ctr__Clause__14 p _ _) = p
type IdList = [Name]
data ImportsOpt = Anti_ImportsOpt String |
                  Ctr__ImportsOpt__0 RtkPos |
                  Ctr__ImportsOpt__1 RtkPos Rule_0
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf ImportsOpt where
    rtkPosOf (Anti_ImportsOpt _) = rtkNoPos
    rtkPosOf (Ctr__ImportsOpt__0 p) = p
    rtkPosOf (Ctr__ImportsOpt__1 p _) = p
data Name = Anti_Name String |
            Ctr__Name__0 RtkPos String
            deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Name where
    rtkPosOf (Anti_Name _) = rtkNoPos
    rtkPosOf (Ctr__Name__0 p _) = p
data OptDelim = Anti_OptDelim String |
                Ctr__OptDelim__0 RtkPos |
                Ctr__OptDelim__1 RtkPos Rule_4
                deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf OptDelim where
    rtkPosOf (Anti_OptDelim _) = rtkNoPos
    rtkPosOf (Ctr__OptDelim__0 p) = p
    rtkPosOf (Ctr__OptDelim__1 p _) = p
data Option = Anti_Option String |
              Ctr__Option__0 RtkPos IdList |
              Ctr__Option__1 RtkPos
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Option where
    rtkPosOf (Anti_Option _) = rtkNoPos
    rtkPosOf (Ctr__Option__0 p _) = p
    rtkPosOf (Ctr__Option__1 p) = p
type OptionList = [Option]
data Rule = Anti_Rule String |
            Ctr__Rule__0 RtkPos Name Clause |
            Ctr__Rule__1 RtkPos Name Name Clause |
            Ctr__Rule__2 RtkPos Name Name Name Clause |
            Ctr__Rule__3 RtkPos Name Name Clause |
            Ctr__Rule__4 RtkPos OptionList Rule
            deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule where
    rtkPosOf (Anti_Rule _) = rtkNoPos
    rtkPosOf (Ctr__Rule__0 p _ _) = p
    rtkPosOf (Ctr__Rule__1 p _ _ _) = p
    rtkPosOf (Ctr__Rule__2 p _ _ _ _) = p
    rtkPosOf (Ctr__Rule__3 p _ _ _) = p
    rtkPosOf (Ctr__Rule__4 p _ _) = p
type RuleList = [Rule]
data Rule_0 = Ctr__Rule_0__0 RtkPos String
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_0 where
    rtkPosOf (Ctr__Rule_0__0 p _) = p
data Rule_4 = Ctr__Rule_4__0 RtkPos Clause
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_4 where
    rtkPosOf (Ctr__Rule_4__0 p _) = p
data StrLit = Anti_StrLit String |
              Ctr__StrLit__0 RtkPos String
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf StrLit where
    rtkPosOf (Anti_StrLit _) = rtkNoPos
    rtkPosOf (Ctr__StrLit__0 p _) = p
}