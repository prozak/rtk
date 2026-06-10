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
id { L.PosToken _ (L.Tk__id $$) }
qq_Id { L.PosToken _ (L.Tk__qq_Id $$) }
qq_Op2 { L.PosToken _ (L.Tk__qq_Op2 $$) }
qq_Op1 { L.PosToken _ (L.Tk__qq_Op1 $$) }
qq_E { L.PosToken _ (L.Tk__qq_E $$) }
qq_P { L.PosToken _ (L.Tk__qq_P $$) }

%%

P__top : P rtk__eof { $1 }

P : tok_P_dummy_4 P tok_P_dummy_4 { Ctr__P__0 $2 } |
    tok_E_dummy_3 E tok_E_dummy_3 { Ctr__P__1 $2 } |
    tok_Id_dummy_2 Id tok_Id_dummy_2 { Ctr__P__2 $2 } |
    tok_Op1_dummy_1 Op1 tok_Op1_dummy_1 { Ctr__P__3 $2 } |
    tok_Op2_dummy_0 Op2 tok_Op2_dummy_0 { Ctr__P__4 $2 }

P : qq_P { Anti_P $1 } |
    tok__lparen__0 tok_lambda_1 tok__lparen__0 Id tok__rparen__2 E tok__rparen__2 { Ctr__P__5 $4 $6 }

E : qq_E { Anti_E $1 } |
    tok_0_3 { Ctr__E__0 } |
    tok_1_4 { Ctr__E__1 } |
    Id { Ctr__E__2 $1 } |
    tok__lparen__0 tok_if0_5 E E E tok__rparen__2 { Ctr__E__3 $3 $4 $5 } |
    tok__lparen__0 tok_fold_6 E E tok__lparen__0 tok_lambda_1 tok__lparen__0 Id Id tok__rparen__2 E tok__rparen__2 tok__rparen__2 { Ctr__E__4 $3 $4 $8 $9 $11 } |
    tok__lparen__0 Op1 E tok__rparen__2 { Ctr__E__5 $2 $3 } |
    tok__lparen__0 Op2 E E tok__rparen__2 { Ctr__E__6 $2 $3 $4 }

Id : qq_Id { Anti_Id $1 } |
     id { Ctr__Id__0 $1 }

Op1 : qq_Op1 { Anti_Op1 $1 } |
      tok_not_7 { Ctr__Op1__0 } |
      tok_shl1_8 { Ctr__Op1__1 } |
      tok_shr1_9 { Ctr__Op1__2 } |
      tok_shr4_10 { Ctr__Op1__3 } |
      tok_shr16_11 { Ctr__Op1__4 }

Op2 : qq_Op2 { Anti_Op2 $1 } |
      tok_and_12 { Ctr__Op2__0 } |
      tok_or_13 { Ctr__Op2__1 } |
      tok_xor_14 { Ctr__Op2__2 } |
      tok_plus_15 { Ctr__Op2__3 }


{
parseError :: [L.PosToken] -> Either String a
parseError [] = Left "Parse error: unexpected end of input"
parseError (L.PosToken (L.AlexPn _ line col) tok : _) =
    Left $ "Parse error at line " ++ show line ++ ", column " ++ show col ++ ": unexpected " ++ showRtkToken tok

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

data P = Ctr__P__0 P |
         Ctr__P__1 E |
         Ctr__P__2 Id |
         Ctr__P__3 Op1 |
         Ctr__P__4 Op2 |
         Anti_P String |
         Ctr__P__5 Id E
         deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data E = Anti_E String |
         Ctr__E__0 |
         Ctr__E__1 |
         Ctr__E__2 Id |
         Ctr__E__3 E E E |
         Ctr__E__4 E E Id Id E |
         Ctr__E__5 Op1 E |
         Ctr__E__6 Op2 E E
         deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Id = Anti_Id String |
          Ctr__Id__0 String
          deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Op1 = Anti_Op1 String |
           Ctr__Op1__0 |
           Ctr__Op1__1 |
           Ctr__Op1__2 |
           Ctr__Op1__3 |
           Ctr__Op1__4
           deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Op2 = Anti_Op2 String |
           Ctr__Op2__0 |
           Ctr__Op2__1 |
           Ctr__Op2__2 |
           Ctr__Op2__3
           deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
}