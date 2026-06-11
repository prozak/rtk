{
{-# LANGUAGE DeriveDataTypeable #-}
module PParser where
import qualified Data.Generics as Gen
import qualified PLexer as L (Token(..), PosToken(..), AlexPosn(..), alexScanTokens)
}

%name parseP
%tokentype { L.PosToken }
%monad { Either String }
%error { parseError }

%token

rtk__eof { L.PosToken _ L.EndOfFile }
tok_E_dummy_3 { L.PosToken _ L.Tk__tok_E_dummy_3 }
tok_Id_dummy_2 { L.PosToken _ L.Tk__tok_Id_dummy_2 }
tok_Op1_dummy_1 { L.PosToken _ L.Tk__tok_Op1_dummy_1 }
tok_Op2_dummy_0 { L.PosToken _ L.Tk__tok_Op2_dummy_0 }
tok_P_dummy_4 { L.PosToken _ L.Tk__tok_P_dummy_4 }
tok_xor_14 { L.PosToken _ L.Tk__tok_xor_14 }
tok_shr4_10 { L.PosToken _ L.Tk__tok_shr4_10 }
tok_shr16_11 { L.PosToken _ L.Tk__tok_shr16_11 }
tok_shr1_9 { L.PosToken _ L.Tk__tok_shr1_9 }
tok_shl1_8 { L.PosToken _ L.Tk__tok_shl1_8 }
tok_plus_15 { L.PosToken _ L.Tk__tok_plus_15 }
tok_or_13 { L.PosToken _ L.Tk__tok_or_13 }
tok_not_7 { L.PosToken _ L.Tk__tok_not_7 }
tok_lambda_1 { L.PosToken _ L.Tk__tok_lambda_1 }
tok_if0_5 { L.PosToken _ L.Tk__tok_if0_5 }
tok_fold_6 { L.PosToken _ L.Tk__tok_fold_6 }
tok_and_12 { L.PosToken _ L.Tk__tok_and_12 }
tok_1_4 { L.PosToken _ L.Tk__tok_1_4 }
tok_0_3 { L.PosToken _ L.Tk__tok_0_3 }
tok__rparen__2 { L.PosToken _ L.Tk__tok__rparen__2 }
tok__lparen__0 { L.PosToken _ L.Tk__tok__lparen__0 }
id { L.PosToken _ (L.Tk__id _) }
qq_Id { L.PosToken _ (L.Tk__qq_Id _) }
qq_Op2 { L.PosToken _ (L.Tk__qq_Op2 _) }
qq_Op1 { L.PosToken _ (L.Tk__qq_Op1 _) }
qq_E { L.PosToken _ (L.Tk__qq_E _) }
qq_P { L.PosToken _ (L.Tk__qq_P _) }

%%

P__top : P rtk__eof { $1 }

P : tok_P_dummy_4 P tok_P_dummy_4 { Ctr__P__0 (rtkPosOf $1) $2 } |
    tok_E_dummy_3 E tok_E_dummy_3 { Ctr__P__1 (rtkPosOf $1) $2 } |
    tok_Id_dummy_2 Id tok_Id_dummy_2 { Ctr__P__2 (rtkPosOf $1) $2 } |
    tok_Op1_dummy_1 Op1 tok_Op1_dummy_1 { Ctr__P__3 (rtkPosOf $1) $2 } |
    tok_Op2_dummy_0 Op2 tok_Op2_dummy_0 { Ctr__P__4 (rtkPosOf $1) $2 }

P : qq_P { Anti_P (tkVal_qq_P $1) } |
    tok__lparen__0 tok_lambda_1 tok__lparen__0 Id tok__rparen__2 E tok__rparen__2 { Ctr__P__5 (rtkPosOf $1) $4 $6 }

E : qq_E { Anti_E (tkVal_qq_E $1) } |
    tok_0_3 { Ctr__E__0 (rtkPosOf $1) } |
    tok_1_4 { Ctr__E__1 (rtkPosOf $1) } |
    Id { Ctr__E__2 (rtkPosOf $1) $1 } |
    tok__lparen__0 tok_if0_5 E E E tok__rparen__2 { Ctr__E__3 (rtkPosOf $1) $3 $4 $5 } |
    tok__lparen__0 tok_fold_6 E E tok__lparen__0 tok_lambda_1 tok__lparen__0 Id Id tok__rparen__2 E tok__rparen__2 tok__rparen__2 { Ctr__E__4 (rtkPosOf $1) $3 $4 $8 $9 $11 } |
    tok__lparen__0 Op1 E tok__rparen__2 { Ctr__E__5 (rtkPosOf $1) $2 $3 } |
    tok__lparen__0 Op2 E E tok__rparen__2 { Ctr__E__6 (rtkPosOf $1) $2 $3 $4 }

Id : qq_Id { Anti_Id (tkVal_qq_Id $1) } |
     id { Ctr__Id__0 (rtkPosOf $1) (tkVal_id $1) }

Op1 : qq_Op1 { Anti_Op1 (tkVal_qq_Op1 $1) } |
      tok_not_7 { Ctr__Op1__0 (rtkPosOf $1) } |
      tok_shl1_8 { Ctr__Op1__1 (rtkPosOf $1) } |
      tok_shr1_9 { Ctr__Op1__2 (rtkPosOf $1) } |
      tok_shr4_10 { Ctr__Op1__3 (rtkPosOf $1) } |
      tok_shr16_11 { Ctr__Op1__4 (rtkPosOf $1) }

Op2 : qq_Op2 { Anti_Op2 (tkVal_qq_Op2 $1) } |
      tok_and_12 { Ctr__Op2__0 (rtkPosOf $1) } |
      tok_or_13 { Ctr__Op2__1 (rtkPosOf $1) } |
      tok_xor_14 { Ctr__Op2__2 (rtkPosOf $1) } |
      tok_plus_15 { Ctr__Op2__3 (rtkPosOf $1) }


{
parseError :: [L.PosToken] -> Either String a
parseError [] = Left "unexpected end of input"
parseError (L.PosToken (L.AlexPn _ line col) tok : _) =
    Left $ show line ++ ":" ++ show col ++ ":unexpected " ++ showRtkToken tok

-- Render a token the way it appears in the source, for error messages
showRtkToken :: L.Token -> String
showRtkToken L.EndOfFile = "end of input"
showRtkToken L.Tk__tok_E_dummy_3 = "'tok_E_dummy_3'"
showRtkToken L.Tk__tok_Id_dummy_2 = "'tok_Id_dummy_2'"
showRtkToken L.Tk__tok_Op1_dummy_1 = "'tok_Op1_dummy_1'"
showRtkToken L.Tk__tok_Op2_dummy_0 = "'tok_Op2_dummy_0'"
showRtkToken L.Tk__tok_P_dummy_4 = "'tok_P_dummy_4'"
showRtkToken L.Tk__tok_xor_14 = "'xor'"
showRtkToken L.Tk__tok_shr4_10 = "'shr4'"
showRtkToken L.Tk__tok_shr16_11 = "'shr16'"
showRtkToken L.Tk__tok_shr1_9 = "'shr1'"
showRtkToken L.Tk__tok_shl1_8 = "'shl1'"
showRtkToken L.Tk__tok_plus_15 = "'plus'"
showRtkToken L.Tk__tok_or_13 = "'or'"
showRtkToken L.Tk__tok_not_7 = "'not'"
showRtkToken L.Tk__tok_lambda_1 = "'lambda'"
showRtkToken L.Tk__tok_if0_5 = "'if0'"
showRtkToken L.Tk__tok_fold_6 = "'fold'"
showRtkToken L.Tk__tok_and_12 = "'and'"
showRtkToken L.Tk__tok_1_4 = "'1'"
showRtkToken L.Tk__tok_0_3 = "'0'"
showRtkToken L.Tk__tok__rparen__2 = "')'"
showRtkToken L.Tk__tok__lparen__0 = "'('"
showRtkToken (L.Tk__id v) = "id " ++ show v
showRtkToken (L.Tk__qq_Id v) = "qq_Id " ++ show v
showRtkToken (L.Tk__qq_Op2 v) = "qq_Op2 " ++ show v
showRtkToken (L.Tk__qq_Op1 v) = "qq_Op1 " ++ show v
showRtkToken (L.Tk__qq_E v) = "qq_E " ++ show v
showRtkToken (L.Tk__qq_P v) = "qq_P " ++ show v

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
tkVal_id :: L.PosToken -> String
tkVal_id (L.PosToken _ (L.Tk__id v)) = v
tkVal_id t = error ("rtk internal error: token id expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Id :: L.PosToken -> String
tkVal_qq_Id (L.PosToken _ (L.Tk__qq_Id v)) = v
tkVal_qq_Id t = error ("rtk internal error: token qq_Id expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Op2 :: L.PosToken -> String
tkVal_qq_Op2 (L.PosToken _ (L.Tk__qq_Op2 v)) = v
tkVal_qq_Op2 t = error ("rtk internal error: token qq_Op2 expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_Op1 :: L.PosToken -> String
tkVal_qq_Op1 (L.PosToken _ (L.Tk__qq_Op1 v)) = v
tkVal_qq_Op1 t = error ("rtk internal error: token qq_Op1 expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_E :: L.PosToken -> String
tkVal_qq_E (L.PosToken _ (L.Tk__qq_E v)) = v
tkVal_qq_E t = error ("rtk internal error: token qq_E expected, got " ++ showRtkToken (L.ptToken t))
tkVal_qq_P :: L.PosToken -> String
tkVal_qq_P (L.PosToken _ (L.Tk__qq_P v)) = v
tkVal_qq_P t = error ("rtk internal error: token qq_P expected, got " ++ showRtkToken (L.ptToken t))

data P = Ctr__P__0 RtkPos P |
         Ctr__P__1 RtkPos E |
         Ctr__P__2 RtkPos Id |
         Ctr__P__3 RtkPos Op1 |
         Ctr__P__4 RtkPos Op2 |
         Anti_P String |
         Ctr__P__5 RtkPos Id E
         deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf P where
    rtkPosOf (Ctr__P__0 p _) = p
    rtkPosOf (Ctr__P__1 p _) = p
    rtkPosOf (Ctr__P__2 p _) = p
    rtkPosOf (Ctr__P__3 p _) = p
    rtkPosOf (Ctr__P__4 p _) = p
    rtkPosOf (Anti_P _) = rtkNoPos
    rtkPosOf (Ctr__P__5 p _ _) = p
data E = Anti_E String |
         Ctr__E__0 RtkPos |
         Ctr__E__1 RtkPos |
         Ctr__E__2 RtkPos Id |
         Ctr__E__3 RtkPos E E E |
         Ctr__E__4 RtkPos E E Id Id E |
         Ctr__E__5 RtkPos Op1 E |
         Ctr__E__6 RtkPos Op2 E E
         deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf E where
    rtkPosOf (Anti_E _) = rtkNoPos
    rtkPosOf (Ctr__E__0 p) = p
    rtkPosOf (Ctr__E__1 p) = p
    rtkPosOf (Ctr__E__2 p _) = p
    rtkPosOf (Ctr__E__3 p _ _ _) = p
    rtkPosOf (Ctr__E__4 p _ _ _ _ _) = p
    rtkPosOf (Ctr__E__5 p _ _) = p
    rtkPosOf (Ctr__E__6 p _ _ _) = p
data Id = Anti_Id String |
          Ctr__Id__0 RtkPos String
          deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Id where
    rtkPosOf (Anti_Id _) = rtkNoPos
    rtkPosOf (Ctr__Id__0 p _) = p
data Op1 = Anti_Op1 String |
           Ctr__Op1__0 RtkPos |
           Ctr__Op1__1 RtkPos |
           Ctr__Op1__2 RtkPos |
           Ctr__Op1__3 RtkPos |
           Ctr__Op1__4 RtkPos
           deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Op1 where
    rtkPosOf (Anti_Op1 _) = rtkNoPos
    rtkPosOf (Ctr__Op1__0 p) = p
    rtkPosOf (Ctr__Op1__1 p) = p
    rtkPosOf (Ctr__Op1__2 p) = p
    rtkPosOf (Ctr__Op1__3 p) = p
    rtkPosOf (Ctr__Op1__4 p) = p
data Op2 = Anti_Op2 String |
           Ctr__Op2__0 RtkPos |
           Ctr__Op2__1 RtkPos |
           Ctr__Op2__2 RtkPos |
           Ctr__Op2__3 RtkPos
           deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
instance RtkPosOf Op2 where
    rtkPosOf (Anti_Op2 _) = rtkNoPos
    rtkPosOf (Ctr__Op2__0 p) = p
    rtkPosOf (Ctr__Op2__1 p) = p
    rtkPosOf (Ctr__Op2__2 p) = p
    rtkPosOf (Ctr__Op2__3 p) = p
}