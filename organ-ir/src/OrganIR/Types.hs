-- | OrganIR data types, faithfully representing the JSON schema.
module OrganIR.Types where

import Data.Text (Text)

-- | Schema version (semver).
schemaVersion :: Text
schemaVersion = "0.1.0"

-- | A name with optional unique suffix.
data Name = Name {nameText :: Text, nameUnique :: Int}
    deriving (Eq, Show)

-- | A qualified name: module prefix + local name.
data QName = QName {qnModule :: Text, qnName :: Name}
    deriving (Eq, Show)

-- | Source language tag.
data SourceLang
    = LHaskell
    | LRust
    | LMercury
    | LIdris2
    | LLean4
    | LKoka
    | LOCaml
    | LSwift
    | LErlang
    | LPurescript
    | LAgda
    | LFSharp
    | LScala3
    | LJulia
    | LZig
    | LC
    | LCpp
    | LFortran
    | LAda
    | LSml
    | LCommonLisp
    | LScheme
    | LProlog
    | LLua
    | LForth
    deriving (Eq, Show)

-- | Top-level OrganIR document.
data OrganIR = OrganIR
    { irMetadata :: Metadata
    , irModule :: Module
    }
    deriving (Show)

-- | Document metadata.
data Metadata = Metadata
    { metaSourceLang :: SourceLang
    , metaCompilerVersion :: Maybe Text
    , metaSourceFile :: Maybe Text
    , metaShimVersion :: Text
    , metaTimestamp :: Maybe Text
    }
    deriving (Show)

-- | A module with definitions, data types, and effect declarations.
data Module = Module
    { modName :: Text
    , modExports :: [Text]
    , modImports :: [QName]
    , modDefs :: [Definition]
    , modDataTypes :: [DataType]
    , modEffectDecls :: [EffectDecl]
    }
    deriving (Show)

-- | Definition sort.
data Sort = SFun | SVal | SExternal | SCon
    deriving (Eq, Show)

-- | Definition visibility.
data Visibility = Public | Private
    deriving (Eq, Show)

-- | A top-level definition.
data Definition = Definition
    { defName :: QName
    , defType :: Ty
    , defExpr :: Expr
    , defSort :: Sort
    , defVisibility :: Visibility
    , defArity :: Int
    }
    deriving (Show)

-- | Type representation.
data Ty
    = TForall [TyVar] Ty
    | TFn [FnArg] EffectRow Ty
    | TApp QName [Ty]
    | TCon QName
    | TVar Name
    | TSyn QName Ty
    | TAny
    deriving (Eq, Show)

-- | Function argument with optional multiplicity.
data FnArg = FnArg
    { fnArgMultiplicity :: Maybe Multiplicity
    , fnArgType :: Ty
    }
    deriving (Eq, Show)

-- | Multiplicity annotation.
data Multiplicity = Many | Affine | Linear
    deriving (Eq, Show)

-- | Type variable with optional kind.
data TyVar = TyVar {tvName :: Name, tvKind :: Maybe Text}
    deriving (Eq, Show)

-- | Effect row.
data EffectRow = EffectRow
    { erEffects :: [QName]
    , erTail :: Maybe Name
    }
    deriving (Eq, Show)

-- | Expression representation.
data Expr
    = EVar Name
    | ELit Lit
    | ECon QName [Expr]
    | EApp Expr [Expr]
    | ELam [LamParam] Expr
    | ELet [LetBind] Expr
    | ECase Expr [Branch]
    | ETypeApp Expr [Ty]
    | ETypeLam [TyVar] Expr
    | EPerform QName Text [Expr]
    | EHandle QName Expr Expr
    | ERetain Name
    | ERelease Name
    | EDrop Name
    | EReuse Name
    | EDelay Expr
    | EForce Expr
    | ETuple [Expr]
    | EList [Expr]
    | ERaise Expr
    | EUnreachable
    deriving (Show)

-- | Literal values.
data Lit = LitInt Integer | LitFloat Double | LitString Text | LitBool Bool
    deriving (Show)

-- | Lambda parameter.
data LamParam = LamParam {lpName :: Name, lpType :: Maybe Ty}
    deriving (Show)

-- | Let binding.
data LetBind = LetBind {lbName :: Name, lbType :: Maybe Ty, lbExpr :: Expr}
    deriving (Show)

-- | Case branch.
data Branch = Branch {brPattern :: Pat, brBody :: Expr}
    deriving (Show)

-- | Pattern.
data Pat
    = PatCon QName [PatBinder]
    | PatLit Lit
    | PatVar Name (Maybe Ty)
    | PatWild
    deriving (Show)

-- | Binder in a constructor pattern.
data PatBinder = PatBinder {pbName :: Name, pbType :: Maybe Ty}
    deriving (Show)

-- | Data type declaration.
data DataType = DataType
    { dtName :: QName
    , dtTypeParams :: [TyVar]
    , dtConstructors :: [Constructor]
    }
    deriving (Show)

-- | Data type constructor.
data Constructor = Constructor
    { conName :: QName
    , conFields :: [Ty]
    }
    deriving (Show)

-- | Effect declaration.
data EffectDecl = EffectDecl
    { edName :: QName
    , edTypeParams :: [TyVar]
    , edOperations :: [Operation]
    }
    deriving (Show)

-- | Effect operation.
data Operation = Operation
    { opName :: Text
    , opType :: Ty
    }
    deriving (Show)
