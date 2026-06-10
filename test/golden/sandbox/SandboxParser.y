{
{-# LANGUAGE DeriveDataTypeable #-}
module SandboxParser where
import qualified Data.Generics as Gen
import qualified SandboxLexer as L (Token(..), PosToken(..), AlexPosn(..), alexScanTokens)
}

%name parseSandbox
%tokentype { L.PosToken }
%monad { Either String }
%error { parseError }

%token

rtk__eof { L.PosToken _ L.EndOfFile }
tok_Sandbox_dummy_0 { L.PosToken _ L.Tk__tok_Sandbox_dummy_0 }
doccomment { L.PosToken _ (L.Tk__doccomment $$) }
qq_Sandbox { L.PosToken _ (L.Tk__qq_Sandbox $$) }

%%

Sandbox__top : Sandbox rtk__eof { $1 }

Sandbox : tok_Sandbox_dummy_0 Sandbox tok_Sandbox_dummy_0 { Ctr__Sandbox__0 $2 }

Sandbox : qq_Sandbox { Anti_Sandbox $1 } |
          doccomment { Ctr__Sandbox__1 $1 }


{
parseError :: [L.PosToken] -> Either String a
parseError [] = Left "Parse error: unexpected end of input"
parseError (L.PosToken (L.AlexPn _ line col) tok : _) =
    Left $ "Parse error at line " ++ show line ++ ", column " ++ show col ++ ": unexpected " ++ showRtkToken tok

-- Render a token the way it appears in the source, for error messages
showRtkToken :: L.Token -> String
showRtkToken L.EndOfFile = "end of input"
showRtkToken L.Tk__tok_Sandbox_dummy_0 = "'tok_Sandbox_dummy_0'"
showRtkToken (L.Tk__doccomment v) = "doccomment " ++ show v
showRtkToken (L.Tk__qq_Sandbox v) = "qq_Sandbox " ++ show v

data Sandbox = Ctr__Sandbox__0 Sandbox |
               Anti_Sandbox String |
               Ctr__Sandbox__1 String
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
}