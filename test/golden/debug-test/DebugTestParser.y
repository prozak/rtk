{
{-# LANGUAGE DeriveDataTypeable #-}
module DebugTestParser where
import qualified Data.Generics as Gen
import qualified DebugTestLexer as L (Token(..), alexScanTokens)
}

%name parseDebugTest
%tokentype { L.Token }
%error { \ rest -> error $ "Parse error " ++ (show rest) }

%token

tok_Assignment_dummy_18 { L.Tk__tok_Assignment_dummy_18 }
tok_Block_dummy_17 { L.Tk__tok_Block_dummy_17 }
tok_Expression_dummy_16 { L.Tk__tok_Expression_dummy_16 }
tok_Factor_dummy_15 { L.Tk__tok_Factor_dummy_15 }
tok_IfStatement_dummy_14 { L.Tk__tok_IfStatement_dummy_14 }
tok_Program_dummy_19 { L.Tk__tok_Program_dummy_19 }
tok_Statement_dummy_13 { L.Tk__tok_Statement_dummy_13 }
tok_Term_dummy_12 { L.Tk__tok_Term_dummy_12 }
tok_UnusedRule1_dummy_11 { L.Tk__tok_UnusedRule1_dummy_11 }
tok_UnusedRule2_dummy_10 { L.Tk__tok_UnusedRule2_dummy_10 }
tok_WhileLoop_dummy_9 { L.Tk__tok_WhileLoop_dummy_9 }
tok__symbol__12 { L.Tk__tok__symbol__12 }
tok__symbol__11 { L.Tk__tok__symbol__11 }
tok_while_10 { L.Tk__tok_while_10 }
tok_unused_13 { L.Tk__tok_unused_13 }
tok_if_8 { L.Tk__tok_if_8 }
tok_else_9 { L.Tk__tok_else_9 }
tok__eql__0 { L.Tk__tok__eql__0 }
tok__semi__1 { L.Tk__tok__semi__1 }
tok__symbol__5 { L.Tk__tok__symbol__5 }
tok__minus__3 { L.Tk__tok__minus__3 }
tok__plus__2 { L.Tk__tok__plus__2 }
tok__star__4 { L.Tk__tok__star__4 }
tok__rparen__7 { L.Tk__tok__rparen__7 }
tok__lparen__6 { L.Tk__tok__lparen__6 }
number { L.Tk__number $$ }
identifier { L.Tk__identifier $$ }
qq_UnusedRule2 { L.Tk__qq_UnusedRule2 $$ }
qq_UnusedRule1 { L.Tk__qq_UnusedRule1 $$ }
qq_Block { L.Tk__qq_Block $$ }
qq_WhileLoop { L.Tk__qq_WhileLoop $$ }
qq_IfStatement { L.Tk__qq_IfStatement $$ }
qq_Factor { L.Tk__qq_Factor $$ }
qq_Term { L.Tk__qq_Term $$ }
qq_Expression { L.Tk__qq_Expression $$ }
qq_Assignment { L.Tk__qq_Assignment $$ }
qq_Statement { L.Tk__qq_Statement $$ }
qq_Program { L.Tk__qq_Program $$ }

%%

Program : tok_Program_dummy_19 Program tok_Program_dummy_19 { Ctr__Program__0 (reverse $2) } |
          tok_Assignment_dummy_18 Assignment tok_Assignment_dummy_18 { Ctr__Program__1 $2 } |
          tok_Block_dummy_17 Block tok_Block_dummy_17 { Ctr__Program__2 $2 } |
          tok_Expression_dummy_16 Expression tok_Expression_dummy_16 { Ctr__Program__3 $2 } |
          tok_Factor_dummy_15 Factor tok_Factor_dummy_15 { Ctr__Program__4 $2 } |
          tok_IfStatement_dummy_14 IfStatement tok_IfStatement_dummy_14 { Ctr__Program__5 $2 } |
          tok_Statement_dummy_13 Statement tok_Statement_dummy_13 { Ctr__Program__6 $2 } |
          tok_Term_dummy_12 Term tok_Term_dummy_12 { Ctr__Program__7 $2 } |
          tok_UnusedRule1_dummy_11 UnusedRule1 tok_UnusedRule1_dummy_11 { Ctr__Program__8 $2 } |
          tok_UnusedRule2_dummy_10 UnusedRule2 tok_UnusedRule2_dummy_10 { Ctr__Program__9 (reverse $2) } |
          tok_WhileLoop_dummy_9 WhileLoop tok_WhileLoop_dummy_9 { Ctr__Program__10 $2 }

Program : {- empty -} { [] } |
          Program ListElem_Program0 { $2 : $1 }

Assignment : qq_Assignment { Anti_Assignment $1 } |
             identifier tok__eql__0 Expression tok__semi__1 { Ctr__Assignment__0 $1 $3 }

Block : qq_Block { Anti_Block $1 } |
        tok__symbol__11 Rule_7 tok__symbol__12 { Ctr__Block__0 (reverse $2) }

Expression : qq_Expression { Anti_Expression $1 } |
             Term Rule_1 { Ctr__Expression__0 $1 (reverse $2) }

Factor : qq_Factor { Anti_Factor $1 } |
         identifier { Ctr__Factor__0 $1 } |
         number { Ctr__Factor__1 $1 } |
         tok__lparen__6 Expression tok__rparen__7 { Ctr__Factor__2 $2 }

IfStatement : qq_IfStatement { Anti_IfStatement $1 } |
              tok_if_8 tok__lparen__6 Expression tok__rparen__7 Statement tok_else_9 Statement { Ctr__IfStatement__0 $3 $5 $7 }

IfStatement : qq_IfStatement { Anti_IfStatement $1 } |
              tok_if_8 tok__lparen__6 Expression tok__rparen__7 Statement { Ctr__IfStatement__1 $3 $5 }

Rule_1 : {- empty -} { [] } |
         Rule_1 Rule_2 { $2 : $1 }

Rule_2 : Rule_3 Term { Ctr__Rule_2__0 $1 $2 }

Rule_3 : tok__plus__2 { Ctr__Rule_3__0 } |
         tok__minus__3 { Ctr__Rule_3__1 }

Rule_4 : {- empty -} { [] } |
         Rule_4 Rule_5 { $2 : $1 }

Rule_5 : Rule_6 Factor { Ctr__Rule_5__0 $1 $2 }

Rule_6 : tok__star__4 { Ctr__Rule_6__0 } |
         tok__symbol__5 { Ctr__Rule_6__1 }

Rule_7 : {- empty -} { [] } |
         Rule_7 Statement { $2 : $1 }

Statement : qq_Statement { Anti_Statement $1 } |
            Assignment { Ctr__Statement__0 $1 } |
            IfStatement { Ctr__Statement__1 $1 } |
            WhileLoop { Ctr__Statement__2 $1 } |
            Block { Ctr__Statement__3 $1 }

ListElem_Program0 : qq_Program { Anti_Statement $1 } |
                    Statement { $1 }

Term : qq_Term { Anti_Term $1 } |
       Factor Rule_4 { Ctr__Term__0 $1 (reverse $2) }

ListElem_UnusedRule28 : qq_UnusedRule2 { Anti_UnusedRule1 $1 } |
                        UnusedRule1 { $1 }

UnusedRule1 : qq_UnusedRule1 { Anti_UnusedRule1 $1 } |
              tok_unused_13 identifier { Ctr__UnusedRule1__1 $2 }

UnusedRule2 : {- empty -} { [] } |
              UnusedRule2 ListElem_UnusedRule28 { $2 : $1 }

WhileLoop : qq_WhileLoop { Anti_WhileLoop $1 } |
            tok_while_10 tok__lparen__6 Expression tok__rparen__7 Statement { Ctr__WhileLoop__0 $3 $5 }


{
data Program = Ctr__Program__0 Program |
               Ctr__Program__1 Assignment |
               Ctr__Program__2 Block |
               Ctr__Program__3 Expression |
               Ctr__Program__4 Factor |
               Ctr__Program__5 IfStatement |
               Ctr__Program__6 Statement |
               Ctr__Program__7 Term |
               Ctr__Program__8 UnusedRule1 |
               Ctr__Program__9 UnusedRule2 |
               Ctr__Program__10 WhileLoop
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Assignment = Anti_Assignment String |
                  Ctr__Assignment__0 String Expression
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Block = Anti_Block String |
             Ctr__Block__0 Rule_7
             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Expression = Anti_Expression String |
                  Ctr__Expression__0 Term Rule_1
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Factor = Anti_Factor String |
              Ctr__Factor__0 String |
              Ctr__Factor__1 String |
              Ctr__Factor__2 Expression
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data IfStatement = Anti_IfStatement String |
                   Ctr__IfStatement__0 Expression Statement Statement |
                   Ctr__IfStatement__1 Expression Statement
                   deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type Rule_1 = [Rule_2]
data Rule_2 = Ctr__Rule_2__0 Rule_3 Term
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_3 = Ctr__Rule_3__0 |
              Ctr__Rule_3__1
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type Rule_4 = [Rule_5]
data Rule_5 = Ctr__Rule_5__0 Rule_6 Factor
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_6 = Ctr__Rule_6__0 |
              Ctr__Rule_6__1
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type Rule_7 = [Statement]
data Statement = Anti_Statement String |
                 Ctr__Statement__0 Assignment |
                 Ctr__Statement__1 IfStatement |
                 Ctr__Statement__2 WhileLoop |
                 Ctr__Statement__3 Block
                 deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Term = Anti_Term String |
            Ctr__Term__0 Factor Rule_4
            deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data UnusedRule1 = Anti_UnusedRule1 String |
                   Ctr__UnusedRule1__1 String
                   deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type UnusedRule2 = [UnusedRule1]
data WhileLoop = Anti_WhileLoop String |
                 Ctr__WhileLoop__0 Expression Statement
                 deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
}