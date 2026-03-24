-- | Smart constructors for building OrganIR values.
module OrganIR.Build where

import Data.Text (Text)
import OrganIR.Types

-- * Names

-- | Name with no unique suffix.
name :: Text -> Name
name t = Name t 0

-- | Qualified name in the empty module.
localName :: Text -> QName
localName t = QName "" (name t)

-- | Qualified name with module.
qualName :: Text -> Text -> QName
qualName m t = QName m (name t)

-- * Documents

-- | Build a minimal OrganIR document.
organIR :: SourceLang -> Text -> Text -> [Definition] -> OrganIR
organIR lang shimVer modName defs =
    OrganIR
        { irMetadata = Metadata lang Nothing Nothing shimVer Nothing
        , irModule = Module modName [] defs [] []
        }

-- | Build a full OrganIR document.
organIRFull :: Metadata -> Module -> OrganIR
organIRFull = OrganIR

-- * Definitions

-- | A function definition.
funDef :: Text -> Ty -> Expr -> Int -> Definition
funDef n ty body = Definition (localName n) ty body SFun Public

-- | A value definition.
valDef :: Text -> Ty -> Expr -> Definition
valDef n ty body =
    Definition (localName n) ty body SVal Public 0

-- | A constructor definition.
conDef :: Text -> Ty -> Int -> Definition
conDef n ty = Definition (localName n) ty (ECon (localName n) []) SCon Public

-- * Types

-- | The untyped "any" type.
tAny :: Ty
tAny = TAny

-- | A named type constructor with no arguments.
tCon :: Text -> Ty
tCon = TCon . localName

-- | A type constructor application.
tApp :: Text -> [Ty] -> Ty
tApp c = TApp (localName c)

-- | A function type (pure, no effects).
tFn :: [Ty] -> Ty -> Ty
tFn args = TFn (map (FnArg Nothing) args) pureEffect

-- | A simple type variable.
tVar :: Text -> Ty
tVar = TVar . name

-- * Effects

-- | The pure (empty) effect row.
pureEffect :: EffectRow
pureEffect = EffectRow [] Nothing

-- * Expressions

-- | Variable reference.
eVar :: Text -> Expr
eVar = EVar . name

-- | Integer literal.
eInt :: Integer -> Expr
eInt = ELit . LitInt

-- | Float literal.
eFloat :: Double -> Expr
eFloat = ELit . LitFloat

-- | String literal.
eString :: Text -> Expr
eString = ELit . LitString

-- | Boolean literal.
eBool :: Bool -> Expr
eBool = ELit . LitBool

-- | Function application (single argument).
eApp1 :: Expr -> Expr -> Expr
eApp1 f a = EApp f [a]

-- | Lambda with simple named parameters (no type annotations).
eLam :: [Text] -> Expr -> Expr
eLam params = ELam (map (\n -> LamParam (name n) Nothing) params)

-- | Let expression with a single untyped binding.
eLet1 :: Text -> Expr -> Expr -> Expr
eLet1 n e = ELet [LetBind (name n) Nothing e]

-- * Patterns

-- | Wildcard pattern.
pWild :: Pat
pWild = PatWild

-- | Variable pattern.
pVar :: Text -> Pat
pVar = flip PatVar Nothing . name

-- | Constructor pattern with named binders.
pCon :: Text -> [Text] -> Pat
pCon c bs = PatCon (localName c) (map (\b -> PatBinder (name b) Nothing) bs)
