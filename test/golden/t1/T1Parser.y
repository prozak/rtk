{
{-# LANGUAGE DeriveDataTypeable #-}
module T1Parser where
import qualified Data.Generics as Gen
import qualified T1Lexer as L (Token(..), alexScanTokens)
}

%name parseT1
%tokentype { L.Token }
%error { \ rest -> error $ "Parse error " ++ (show rest) }

%token

tok_A_dummy_19 { L.Tk__tok_A_dummy_19 }
tok_B_dummy_18 { L.Tk__tok_B_dummy_18 }
tok_C_dummy_17 { L.Tk__tok_C_dummy_17 }
tok_D_dummy_16 { L.Tk__tok_D_dummy_16 }
tok_E_dummy_15 { L.Tk__tok_E_dummy_15 }
tok_F1_dummy_14 { L.Tk__tok_F1_dummy_14 }
tok_F2_dummy_13 { L.Tk__tok_F2_dummy_13 }
tok_F3_dummy_12 { L.Tk__tok_F3_dummy_12 }
tok_F4_dummy_11 { L.Tk__tok_F4_dummy_11 }
tok_F5_dummy_10 { L.Tk__tok_F5_dummy_10 }
tok_G_dummy_9 { L.Tk__tok_G_dummy_9 }
tok_b_1 { L.Tk__tok_b_1 }
tok_a_0 { L.Tk__tok_a_0 }
tok__coma__2 { L.Tk__tok__coma__2 }
qq_G { L.Tk__qq_G $$ }
qq_F5 { L.Tk__qq_F5 $$ }
qq_F4 { L.Tk__qq_F4 $$ }
qq_F3 { L.Tk__qq_F3 $$ }
qq_F2 { L.Tk__qq_F2 $$ }
qq_F1 { L.Tk__qq_F1 $$ }
qq_E { L.Tk__qq_E $$ }
qq_D { L.Tk__qq_D $$ }
qq_C { L.Tk__qq_C $$ }
qq_B { L.Tk__qq_B $$ }
qq_A { L.Tk__qq_A $$ }

%%

A : tok_A_dummy_19 A tok_A_dummy_19 { Ctr__A__0 $2 } |
    tok_B_dummy_18 B tok_B_dummy_18 { Ctr__A__1 $2 } |
    tok_C_dummy_17 C tok_C_dummy_17 { Ctr__A__2 (reverse $2) } |
    tok_D_dummy_16 D tok_D_dummy_16 { Ctr__A__3 $2 } |
    tok_E_dummy_15 E tok_E_dummy_15 { Ctr__A__4 $2 } |
    tok_F1_dummy_14 F1 tok_F1_dummy_14 { Ctr__A__5 (reverse $2) } |
    tok_F2_dummy_13 F2 tok_F2_dummy_13 { Ctr__A__6 (reverse $2) } |
    tok_F3_dummy_12 F3 tok_F3_dummy_12 { Ctr__A__7 (reverse $2) } |
    tok_F4_dummy_11 F4 tok_F4_dummy_11 { Ctr__A__8 $2 } |
    tok_F5_dummy_10 F5 tok_F5_dummy_10 { Ctr__A__9 (reverse $2) } |
    tok_G_dummy_9 G tok_G_dummy_9 { Ctr__A__10 $2 }

ListElem_F56 : qq_F5 { Anti_A $1 } |
               A { $1 }

ListElem_F35 : qq_F3 { Anti_A $1 } |
               A { $1 }

ListElem_F24 : qq_F2 { Anti_A $1 } |
               A { $1 }

ListElem_F13 : qq_F1 { Anti_A $1 } |
               A { $1 }

ListElem_C0 : qq_C { Anti_A $1 } |
              A { $1 }

A : qq_A { Anti_A $1 } |
    tok_a_0 { Ctr__A__16 }

B : qq_B { Anti_B $1 } |
    tok_a_0 { Ctr__B__0 } |
    tok_b_1 { Ctr__B__1 }

C : {- empty -} { [] } |
    C ListElem_C0 { $2 : $1 }

D : qq_D { Anti_D $1 } |
    Rule_1 C { Ctr__D__0 $1 (reverse $2) }

E : qq_E { Anti_E $1 } |
    Rule_2 { Ctr__E__0 $1 } |
    C { Ctr__E__1 (reverse $1) }

F1 : ListElem_F13 { [$1] } |
     F1 ListElem_F13 { $2 : $1 }

F2 : {- empty -} { [] } |
     F2 ListElem_F24 { $2 : $1 }

F3__plus_list_ : ListElem_F35 { [$1] } |
                 F3__plus_list_ tok__coma__2 ListElem_F35 { $3 : $1 }

F3 : F3__plus_list_ { $1 } |
     {- empty -} { [] }

F4 : qq_F4 { Anti_F4 $1 } |
     { Ctr__F4__0 } |
     A { Ctr__F4__1 $1 }

F5 : ListElem_F56 { [$1] } |
     F5 tok__coma__2 ListElem_F56 { $3 : $1 }

G : qq_G { Anti_G $1 } |
    A Rule_7 { Ctr__G__0 $1 $2 }

Rule_1 : A B { Ctr__Rule_1__0 $1 $2 }

Rule_2 : A B { Ctr__Rule_2__0 $1 $2 }

Rule_7 : B C Rule_8 A { Ctr__Rule_7__0 $1 (reverse $2) $3 $4 }

Rule_8 : D E { Ctr__Rule_8__0 $1 $2 }


{
data A = Ctr__A__0 A |
         Ctr__A__1 B |
         Ctr__A__2 C |
         Ctr__A__3 D |
         Ctr__A__4 E |
         Ctr__A__5 F1 |
         Ctr__A__6 F2 |
         Ctr__A__7 F3 |
         Ctr__A__8 F4 |
         Ctr__A__9 F5 |
         Ctr__A__10 G |
         Anti_A String |
         Ctr__A__16
         deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data B = Anti_B String |
         Ctr__B__0 |
         Ctr__B__1
         deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type C = [A]
data D = Anti_D String |
         Ctr__D__0 Rule_1 C
         deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data E = Anti_E String |
         Ctr__E__0 Rule_2 |
         Ctr__E__1 C
         deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type F1 = [A]
type F2 = [A]
type F3 = [A]
data F4 = Anti_F4 String |
          Ctr__F4__0 |
          Ctr__F4__1 A
          deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type F5 = [A]
data G = Anti_G String |
         Ctr__G__0 A Rule_7
         deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_1 = Ctr__Rule_1__0 A B
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_2 = Ctr__Rule_2__0 A B
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_7 = Ctr__Rule_7__0 B C Rule_8 A
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_8 = Ctr__Rule_8__0 D E
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
}