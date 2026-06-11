{
{-# LANGUAGE DeriveDataTypeable #-}
module T1Parser where
import qualified Data.Generics as Gen
import qualified T1Lexer as L (Token(..), PosToken(..), AlexPosn(..), alexScanTokens)
}

%name parseT1
%tokentype { L.PosToken }
%monad { Either String }
%error { parseError }

%token

rtk__eof { L.PosToken _ L.EndOfFile }
tok_A_dummy_19 { L.PosToken _ L.Tk__tok_A_dummy_19 }
tok_B_dummy_18 { L.PosToken _ L.Tk__tok_B_dummy_18 }
tok_C_dummy_17 { L.PosToken _ L.Tk__tok_C_dummy_17 }
tok_D_dummy_16 { L.PosToken _ L.Tk__tok_D_dummy_16 }
tok_E_dummy_15 { L.PosToken _ L.Tk__tok_E_dummy_15 }
tok_F1_dummy_14 { L.PosToken _ L.Tk__tok_F1_dummy_14 }
tok_F2_dummy_13 { L.PosToken _ L.Tk__tok_F2_dummy_13 }
tok_F3_dummy_12 { L.PosToken _ L.Tk__tok_F3_dummy_12 }
tok_F4_dummy_11 { L.PosToken _ L.Tk__tok_F4_dummy_11 }
tok_F5_dummy_10 { L.PosToken _ L.Tk__tok_F5_dummy_10 }
tok_G_dummy_9 { L.PosToken _ L.Tk__tok_G_dummy_9 }
tok_b_1 { L.PosToken _ L.Tk__tok_b_1 }
tok_a_0 { L.PosToken _ L.Tk__tok_a_0 }
tok__coma__2 { L.PosToken _ L.Tk__tok__coma__2 }
qq_G { L.PosToken _ (L.Tk__qq_G _) }
qq_F5 { L.PosToken _ (L.Tk__qq_F5 _) }
qq_F4 { L.PosToken _ (L.Tk__qq_F4 _) }
qq_F3 { L.PosToken _ (L.Tk__qq_F3 _) }
qq_F2 { L.PosToken _ (L.Tk__qq_F2 _) }
qq_F1 { L.PosToken _ (L.Tk__qq_F1 _) }
qq_E { L.PosToken _ (L.Tk__qq_E _) }
qq_D { L.PosToken _ (L.Tk__qq_D _) }
qq_C { L.PosToken _ (L.Tk__qq_C _) }
qq_B { L.PosToken _ (L.Tk__qq_B _) }
qq_A { L.PosToken _ (L.Tk__qq_A _) }

%%

T1__top : A rtk__eof { $1 }

A : tok_A_dummy_19 A tok_A_dummy_19 { Ctr__A__0 (rtkPosOf $1) $2 } |
    tok_B_dummy_18 B tok_B_dummy_18 { Ctr__A__1 (rtkPosOf $1) $2 } |
    tok_C_dummy_17 C tok_C_dummy_17 { Ctr__A__2 (rtkPosOf $1) (reverse $2) } |
    tok_D_dummy_16 D tok_D_dummy_16 { Ctr__A__3 (rtkPosOf $1) $2 } |
    tok_E_dummy_15 E tok_E_dummy_15 { Ctr__A__4 (rtkPosOf $1) $2 } |
    tok_F1_dummy_14 F1 tok_F1_dummy_14 { Ctr__A__5 (rtkPosOf $1) (reverse $2) } |
    tok_F2_dummy_13 F2 tok_F2_dummy_13 { Ctr__A__6 (rtkPosOf $1) (reverse $2) } |
    tok_F3_dummy_12 F3 tok_F3_dummy_12 { Ctr__A__7 (rtkPosOf $1) (reverse $2) } |
    tok_F4_dummy_11 F4 tok_F4_dummy_11 { Ctr__A__8 (rtkPosOf $1) $2 } |
    tok_F5_dummy_10 F5 tok_F5_dummy_10 { Ctr__A__9 (rtkPosOf $1) (reverse $2) } |
    tok_G_dummy_9 G tok_G_dummy_9 { Ctr__A__10 (rtkPosOf $1) $2 }

ListElem_F56 : qq_F5 { Anti_A (tkVal_qq_F5 $1) } |
               A { $1 }

ListElem_F35 : qq_F3 { Anti_A (tkVal_qq_F3 $1) } |
               A { $1 }

ListElem_F24 : qq_F2 { Anti_A (tkVal_qq_F2 $1) } |
               A { $1 }

ListElem_F13 : qq_F1 { Anti_A (tkVal_qq_F1 $1) } |
               A { $1 }

ListElem_C0 : qq_C { Anti_A (tkVal_qq_C $1) } |
              A { $1 }

A : qq_A { Anti_A (tkVal_qq_A $1) } |
    tok_a_0 { Ctr__A__16 (rtkPosOf $1) }

B : qq_B { Anti_B (tkVal_qq_B $1) } |
    tok_a_0 { Ctr__B__0 (rtkPosOf $1) } |
    tok_b_1 { Ctr__B__1 (rtkPosOf $1) }

C : {- empty -} { [] } |
    C ListElem_C0 { $2 : $1 }

D : qq_D { Anti_D (tkVal_qq_D $1) } |
    Rule_1 C { Ctr__D__0 (rtkPosOf $1) $1 (reverse $2) }

E : qq_E { Anti_E (tkVal_qq_E $1) } |
    Rule_2 { Ctr__E__0 (rtkPosOf $1) $1 } |
    C { Ctr__E__1 (rtkPosOf (reverse $1)) (reverse $1) }

F1 : ListElem_F13 { [$1] } |
     F1 ListElem_F13 { $2 : $1 }

F2 : {- empty -} { [] } |
     F2 ListElem_F24 { $2 : $1 }

F3__plus_list_ : ListElem_F35 { [$1] } |
                 F3__plus_list_ tok__coma__2 ListElem_F35 { $3 : $1 }

F3 : F3__plus_list_ { $1 } |
     {- empty -} { [] }

F4 : qq_F4 { Anti_F4 (tkVal_qq_F4 $1) } |
     { Ctr__F4__0 rtkNoPos } |
     A { Ctr__F4__1 (rtkPosOf $1) $1 }

F5 : ListElem_F56 { [$1] } |
     F5 tok__coma__2 ListElem_F56 { $3 : $1 }

G : qq_G { Anti_G (tkVal_qq_G $1) } |
    A Rule_7 { Ctr__G__0 (rtkPosOf $1) $1 $2 }

Rule_1 : A B { Ctr__Rule_1__0 (rtkPosOf $1) $1 $2 }

Rule_2 : A B { Ctr__Rule_2__0 (rtkPosOf $1) $1 $2 }

Rule_7 : B C Rule_8 A { Ctr__Rule_7__0 (rtkPosOf $1) $1 (reverse $2) $3 $4 }

Rule_8 : D E { Ctr__Rule_8__0 (rtkPosOf $1) $1 $2 }


{
parseError :: [L.PosToken] -> Either String a
parseError [] = Left "Parse error: unexpected end of input"
parseError (L.PosToken (L.AlexPn _ line col) tok : _) =
    Left $ "Parse error at line " ++ show line ++ ", column " ++ show col ++ ": unexpected " ++ showRtkToken tok

-- Render a token the way it appears in the source, for error messages
showRtkToken :: L.Token -> String
showRtkToken L.EndOfFile = "end of input"
showRtkToken L.Tk__tok_A_dummy_19 = "'tok_A_dummy_19'"
showRtkToken L.Tk__tok_B_dummy_18 = "'tok_B_dummy_18'"
showRtkToken L.Tk__tok_C_dummy_17 = "'tok_C_dummy_17'"
showRtkToken L.Tk__tok_D_dummy_16 = "'tok_D_dummy_16'"
showRtkToken L.Tk__tok_E_dummy_15 = "'tok_E_dummy_15'"
showRtkToken L.Tk__tok_F1_dummy_14 = "'tok_F1_dummy_14'"
showRtkToken L.Tk__tok_F2_dummy_13 = "'tok_F2_dummy_13'"
showRtkToken L.Tk__tok_F3_dummy_12 = "'tok_F3_dummy_12'"
showRtkToken L.Tk__tok_F4_dummy_11 = "'tok_F4_dummy_11'"
showRtkToken L.Tk__tok_F5_dummy_10 = "'tok_F5_dummy_10'"
showRtkToken L.Tk__tok_G_dummy_9 = "'tok_G_dummy_9'"
showRtkToken L.Tk__tok_b_1 = "'b'"
showRtkToken L.Tk__tok_a_0 = "'a'"
showRtkToken L.Tk__tok__coma__2 = "','"
showRtkToken (L.Tk__qq_G v) = "qq_G " ++ show v
showRtkToken (L.Tk__qq_F5 v) = "qq_F5 " ++ show v
showRtkToken (L.Tk__qq_F4 v) = "qq_F4 " ++ show v
showRtkToken (L.Tk__qq_F3 v) = "qq_F3 " ++ show v
showRtkToken (L.Tk__qq_F2 v) = "qq_F2 " ++ show v
showRtkToken (L.Tk__qq_F1 v) = "qq_F1 " ++ show v
showRtkToken (L.Tk__qq_E v) = "qq_E " ++ show v
showRtkToken (L.Tk__qq_D v) = "qq_D " ++ show v
showRtkToken (L.Tk__qq_C v) = "qq_C " ++ show v
showRtkToken (L.Tk__qq_B v) = "qq_B " ++ show v
showRtkToken (L.Tk__qq_A v) = "qq_A " ++ show v

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
tkVal_qq_G :: L.PosToken -> String
tkVal_qq_G (L.PosToken _ (L.Tk__qq_G v)) = v
tkVal_qq_G t = error ("rtk internal error: token qq_G expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_F5 :: L.PosToken -> String
tkVal_qq_F5 (L.PosToken _ (L.Tk__qq_F5 v)) = v
tkVal_qq_F5 t = error ("rtk internal error: token qq_F5 expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_F4 :: L.PosToken -> String
tkVal_qq_F4 (L.PosToken _ (L.Tk__qq_F4 v)) = v
tkVal_qq_F4 t = error ("rtk internal error: token qq_F4 expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_F3 :: L.PosToken -> String
tkVal_qq_F3 (L.PosToken _ (L.Tk__qq_F3 v)) = v
tkVal_qq_F3 t = error ("rtk internal error: token qq_F3 expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_F2 :: L.PosToken -> String
tkVal_qq_F2 (L.PosToken _ (L.Tk__qq_F2 v)) = v
tkVal_qq_F2 t = error ("rtk internal error: token qq_F2 expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_F1 :: L.PosToken -> String
tkVal_qq_F1 (L.PosToken _ (L.Tk__qq_F1 v)) = v
tkVal_qq_F1 t = error ("rtk internal error: token qq_F1 expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_E :: L.PosToken -> String
tkVal_qq_E (L.PosToken _ (L.Tk__qq_E v)) = v
tkVal_qq_E t = error ("rtk internal error: token qq_E expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_D :: L.PosToken -> String
tkVal_qq_D (L.PosToken _ (L.Tk__qq_D v)) = v
tkVal_qq_D t = error ("rtk internal error: token qq_D expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_C :: L.PosToken -> String
tkVal_qq_C (L.PosToken _ (L.Tk__qq_C v)) = v
tkVal_qq_C t = error ("rtk internal error: token qq_C expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_B :: L.PosToken -> String
tkVal_qq_B (L.PosToken _ (L.Tk__qq_B v)) = v
tkVal_qq_B t = error ("rtk internal error: token qq_B expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_A :: L.PosToken -> String
tkVal_qq_A (L.PosToken _ (L.Tk__qq_A v)) = v
tkVal_qq_A t = error ("rtk internal error: token qq_A expected, got " ++ showRtkToken (L.ptToken t))

data A = Ctr__A__0 RtkPos A |
         Ctr__A__1 RtkPos B |
         Ctr__A__2 RtkPos C |
         Ctr__A__3 RtkPos D |
         Ctr__A__4 RtkPos E |
         Ctr__A__5 RtkPos F1 |
         Ctr__A__6 RtkPos F2 |
         Ctr__A__7 RtkPos F3 |
         Ctr__A__8 RtkPos F4 |
         Ctr__A__9 RtkPos F5 |
         Ctr__A__10 RtkPos G |
         Anti_A String |
         Ctr__A__16 RtkPos
         deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf A where
    rtkPosOf (Ctr__A__0 p _) = p
    rtkPosOf (Ctr__A__1 p _) = p
    rtkPosOf (Ctr__A__2 p _) = p
    rtkPosOf (Ctr__A__3 p _) = p
    rtkPosOf (Ctr__A__4 p _) = p
    rtkPosOf (Ctr__A__5 p _) = p
    rtkPosOf (Ctr__A__6 p _) = p
    rtkPosOf (Ctr__A__7 p _) = p
    rtkPosOf (Ctr__A__8 p _) = p
    rtkPosOf (Ctr__A__9 p _) = p
    rtkPosOf (Ctr__A__10 p _) = p
    rtkPosOf (Anti_A _) = rtkNoPos
    rtkPosOf (Ctr__A__16 p) = p
data B = Anti_B String |
         Ctr__B__0 RtkPos |
         Ctr__B__1 RtkPos
         deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf B where
    rtkPosOf (Anti_B _) = rtkNoPos
    rtkPosOf (Ctr__B__0 p) = p
    rtkPosOf (Ctr__B__1 p) = p
type C = [A]
data D = Anti_D String |
         Ctr__D__0 RtkPos Rule_1 C
         deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf D where
    rtkPosOf (Anti_D _) = rtkNoPos
    rtkPosOf (Ctr__D__0 p _ _) = p
data E = Anti_E String |
         Ctr__E__0 RtkPos Rule_2 |
         Ctr__E__1 RtkPos C
         deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf E where
    rtkPosOf (Anti_E _) = rtkNoPos
    rtkPosOf (Ctr__E__0 p _) = p
    rtkPosOf (Ctr__E__1 p _) = p
type F1 = [A]
type F2 = [A]
type F3 = [A]
data F4 = Anti_F4 String |
          Ctr__F4__0 RtkPos |
          Ctr__F4__1 RtkPos A
          deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf F4 where
    rtkPosOf (Anti_F4 _) = rtkNoPos
    rtkPosOf (Ctr__F4__0 p) = p
    rtkPosOf (Ctr__F4__1 p _) = p
type F5 = [A]
data G = Anti_G String |
         Ctr__G__0 RtkPos A Rule_7
         deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf G where
    rtkPosOf (Anti_G _) = rtkNoPos
    rtkPosOf (Ctr__G__0 p _ _) = p
data Rule_1 = Ctr__Rule_1__0 RtkPos A B
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_1 where
    rtkPosOf (Ctr__Rule_1__0 p _ _) = p
data Rule_2 = Ctr__Rule_2__0 RtkPos A B
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_2 where
    rtkPosOf (Ctr__Rule_2__0 p _ _) = p
data Rule_7 = Ctr__Rule_7__0 RtkPos B C Rule_8 A
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_7 where
    rtkPosOf (Ctr__Rule_7__0 p _ _ _ _) = p
data Rule_8 = Ctr__Rule_8__0 RtkPos D E
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Rule_8 where
    rtkPosOf (Ctr__Rule_8__0 p _ _) = p
}