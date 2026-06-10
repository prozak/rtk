{
{-# LANGUAGE DeriveDataTypeable #-}
module SandboxParser where
import qualified Data.Generics as Gen
import qualified SandboxLexer as L (Token(..), alexScanTokens)
}

%name parseSandbox
%tokentype { L.Token }
%error { \ rest -> error $ "Parse error " ++ (show rest) }

%token

tok_Sandbox_dummy_0 { L.Tk__tok_Sandbox_dummy_0 }
doccomment { L.Tk__doccomment $$ }
qq_Sandbox { L.Tk__qq_Sandbox $$ }

%%

Sandbox : tok_Sandbox_dummy_0 Sandbox tok_Sandbox_dummy_0 { Ctr__Sandbox__0 $2 }

Sandbox : qq_Sandbox { Anti_Sandbox $1 } |
          doccomment { Ctr__Sandbox__1 $1 }


{
data Sandbox = Ctr__Sandbox__0 Sandbox |
               Anti_Sandbox String |
               Ctr__Sandbox__1 String
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
}