{
{-# LANGUAGE DeriveDataTypeable #-}
module JavaSimpleParser where
import qualified Data.Generics as Gen
import qualified JavaSimpleLexer as L (Token(..), alexScanTokens)
}

%name parseJavaSimple
%tokentype { L.Token }
%error { \ rest -> error $ "Parse error " ++ (show rest) }

%token

tok_ClassDeclaration_dummy_10 { L.Tk__tok_ClassDeclaration_dummy_10 }
tok_CompilationUnit_dummy_9 { L.Tk__tok_CompilationUnit_dummy_9 }
tok_CompoundName_dummy_8 { L.Tk__tok_CompoundName_dummy_8 }
tok_Field_dummy_7 { L.Tk__tok_Field_dummy_7 }
tok_FieldList_dummy_6 { L.Tk__tok_FieldList_dummy_6 }
tok_JavaSimple_dummy_11 { L.Tk__tok_JavaSimple_dummy_11 }
tok_Package_dummy_5 { L.Tk__tok_Package_dummy_5 }
tok_Type_dummy_4 { L.Tk__tok_Type_dummy_4 }
tok__symbol__5 { L.Tk__tok__symbol__5 }
tok__symbol__4 { L.Tk__tok__symbol__4 }
tok_public_2 { L.Tk__tok_public_2 }
tok_package_0 { L.Tk__tok_package_0 }
tok_int_6 { L.Tk__tok_int_6 }
tok_class_3 { L.Tk__tok_class_3 }
tok_String_7 { L.Tk__tok_String_7 }
tok__semi__1 { L.Tk__tok__semi__1 }
tok__dot__8 { L.Tk__tok__dot__8 }
id { L.Tk__id $$ }
qq_CompoundName { L.Tk__qq_CompoundName $$ }
qq_Type { L.Tk__qq_Type $$ }
qq_Field { L.Tk__qq_Field $$ }
qq_FieldList { L.Tk__qq_FieldList $$ }
qq_ClassDeclaration { L.Tk__qq_ClassDeclaration $$ }
qq_Package { L.Tk__qq_Package $$ }
qq_CompilationUnit { L.Tk__qq_CompilationUnit $$ }
qq_JavaSimple { L.Tk__qq_JavaSimple $$ }

%%

JavaSimple : tok_JavaSimple_dummy_11 JavaSimple tok_JavaSimple_dummy_11 { Ctr__JavaSimple__0 $2 } |
             tok_ClassDeclaration_dummy_10 ClassDeclaration tok_ClassDeclaration_dummy_10 { Ctr__JavaSimple__1 $2 } |
             tok_CompilationUnit_dummy_9 CompilationUnit tok_CompilationUnit_dummy_9 { Ctr__JavaSimple__2 $2 } |
             tok_CompoundName_dummy_8 CompoundName tok_CompoundName_dummy_8 { Ctr__JavaSimple__3 $2 } |
             tok_Field_dummy_7 Field tok_Field_dummy_7 { Ctr__JavaSimple__4 $2 } |
             tok_FieldList_dummy_6 FieldList tok_FieldList_dummy_6 { Ctr__JavaSimple__5 (reverse $2) } |
             tok_Package_dummy_5 Package tok_Package_dummy_5 { Ctr__JavaSimple__6 $2 } |
             tok_Type_dummy_4 Type tok_Type_dummy_4 { Ctr__JavaSimple__7 $2 }

JavaSimple : qq_JavaSimple { Anti_JavaSimple $1 } |
             CompilationUnit { Ctr__JavaSimple__8 $1 }

ClassDeclaration : qq_ClassDeclaration { Anti_ClassDeclaration $1 } |
                   Rule_2 tok_class_3 id tok__symbol__4 FieldList tok__symbol__5 { Ctr__ClassDeclaration__0 $1 $3 (reverse $5) }

CompilationUnit : qq_CompilationUnit { Anti_CompilationUnit $1 } |
                  Rule_0 Rule_1 { Ctr__CompilationUnit__0 $1 $2 }

CompoundName : qq_CompoundName { Anti_CompoundName $1 } |
               id { Ctr__CompoundName__0 $1 } |
               CompoundName tok__dot__8 id { Ctr__CompoundName__1 $1 $3 }

Field : qq_Field { Anti_Field $1 } |
        Type id tok__semi__1 { Ctr__Field__0 $1 $2 }

ListElem_FieldList3 : qq_FieldList { Anti_Field $1 } |
                      Field { $1 }

FieldList : {- empty -} { [] } |
            FieldList ListElem_FieldList3 { $2 : $1 }

Package : qq_Package { Anti_Package $1 } |
          tok_package_0 CompoundName tok__semi__1 { Ctr__Package__0 $2 }

Rule_0 : { Ctr__Rule_0__0 } |
         Package { Ctr__Rule_0__1 $1 }

Rule_1 : { Ctr__Rule_1__0 } |
         ClassDeclaration { Ctr__Rule_1__1 $1 }

Rule_2 : { Ctr__Rule_2__0 } |
         tok_public_2 { Ctr__Rule_2__1 }

Type : qq_Type { Anti_Type $1 } |
       tok_int_6 { Ctr__Type__0 } |
       tok_String_7 { Ctr__Type__1 } |
       id { Ctr__Type__2 $1 }


{
data JavaSimple = Ctr__JavaSimple__0 JavaSimple |
                  Ctr__JavaSimple__1 ClassDeclaration |
                  Ctr__JavaSimple__2 CompilationUnit |
                  Ctr__JavaSimple__3 CompoundName |
                  Ctr__JavaSimple__4 Field |
                  Ctr__JavaSimple__5 FieldList |
                  Ctr__JavaSimple__6 Package |
                  Ctr__JavaSimple__7 Type |
                  Anti_JavaSimple String |
                  Ctr__JavaSimple__8 CompilationUnit
                  deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data ClassDeclaration = Anti_ClassDeclaration String |
                        Ctr__ClassDeclaration__0 Rule_2 String FieldList
                        deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data CompilationUnit = Anti_CompilationUnit String |
                       Ctr__CompilationUnit__0 Rule_0 Rule_1
                       deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data CompoundName = Anti_CompoundName String |
                    Ctr__CompoundName__0 String |
                    Ctr__CompoundName__1 CompoundName String
                    deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Field = Anti_Field String |
             Ctr__Field__0 Type String
             deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
type FieldList = [Field]
data Package = Anti_Package String |
               Ctr__Package__0 CompoundName
               deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_0 = Ctr__Rule_0__0 |
              Ctr__Rule_0__1 Package
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_1 = Ctr__Rule_1__0 |
              Ctr__Rule_1__1 ClassDeclaration
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Rule_2 = Ctr__Rule_2__0 |
              Ctr__Rule_2__1
              deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
data Type = Anti_Type String |
            Ctr__Type__0 |
            Ctr__Type__1 |
            Ctr__Type__2 String
            deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)
}