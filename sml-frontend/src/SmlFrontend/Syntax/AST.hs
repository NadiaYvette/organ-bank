{- | SML abstract syntax tree (Definition Ch. 2-3, core language).
Covers expressions, declarations, patterns, and types.
Module language (structures, signatures, functors) is deferred.
-}
module SmlFrontend.Syntax.AST where

import Data.List.NonEmpty (NonEmpty)
import SmlFrontend.Syntax.Const
import SmlFrontend.Syntax.Ident

-- | Source position for error messages.
data Pos = Pos {posLine :: Int, posCol :: Int}
    deriving (Eq, Show)

noPos :: Pos
noPos = Pos 0 0

-- | Expressions (Definition §2.6).
data Exp
    = -- | Literal
      ESCon SCon
    | -- | Variable / constructor reference
      EVar LongVId
    | -- | Record {lab=exp, ...}
      ERecord [(Lab, Exp)]
    | -- | let dec in exp end
      ELet [Dec] Exp
    | -- | Application (f x)
      EApp Exp Exp
    | -- | Infix application (x + y), resolved later
      EInfix Exp VId Exp
    | -- | Type constraint (exp : ty)
      ETyped Exp Ty
    | -- | Exception handler (exp handle match)
      EHandle Exp (NonEmpty MRule)
    | -- | raise exp
      ERaise Exp
    | -- | fn match (lambda)
      EFn (NonEmpty MRule)
    | -- | case exp of match
      ECase Exp (NonEmpty MRule)
    | -- | if e1 then e2 else e3 (derived)
      EIf Exp Exp Exp
    | -- | e1 andalso e2 (derived)
      EAndalso Exp Exp
    | -- | e1 orelse e2 (derived)
      EOrelse Exp Exp
    | -- | (e1; e2; ...; en) (derived)
      ESeq [Exp]
    | -- | (e1, ..., en) (derived)
      ETuple [Exp]
    | -- | [e1, ..., en] (derived)
      EList [Exp]
    | -- | #lab (record selector)
      ESelector Lab
    | -- | while e1 do e2 (derived)
      EWhile Exp Exp
    | -- | Source position wrapper
      EPos Pos Exp
    deriving (Show)

-- | Match rule: pat => exp.
data MRule = MRule Pat Exp
    deriving (Show)

-- | Patterns (Definition §2.6).
data Pat
    = -- | Wildcard _
      PWild
    | -- | Variable pattern
      PVar VId
    | -- | Literal pattern
      PSCon SCon
    | -- | Constructor pattern (C or C pat)
      PCon LongVId (Maybe Pat)
    | -- | Infix pattern (pat :: pat), resolved later
      PInfix Pat VId Pat
    | -- | Typed pattern (pat : ty)
      PTyped Pat Ty
    | -- | Layered pattern (vid as pat)
      PAs VId Pat
    | -- | Record pattern; Bool = has wildcard (...)
      PRecord [(Lab, Pat)] Bool
    | -- | Tuple pattern (derived)
      PTuple [Pat]
    | -- | List pattern (derived)
      PList [Pat]
    | -- | Source position wrapper
      PPos Pos Pat
    deriving (Show)

{- | Type expressions (Definition §2.6).
Constructors prefixed with Ty to avoid clashing with Ident's TyCon/TyVar.
-}
data Ty
    = -- | Type variable 'a
      TyTyVar TyVar
    | -- | Type constructor application (ty list)
      TyApp [Ty] LongTyCon
    | -- | Function type (ty -> ty)
      TyFun Ty Ty
    | -- | Tuple type (ty * ty)
      TyTuple [Ty]
    | -- | Record type {lab: ty, ...}
      TyRecord [(Lab, Ty)]
    | -- | Source position wrapper
      TyPos Pos Ty
    deriving (Show)

-- | Declarations (Definition §2.6).
data Dec
    = -- | val tyvarseq valbind
      DVal [TyVar] [ValBind]
    | -- | fun tyvarseq funbind
      DFun [TyVar] [FunBind]
    | -- | type typbind
      DType [TypBind]
    | -- | datatype datbind
      DDatatype [DatBind]
    | -- | datatype replication
      DDatatypeRepl TyCon LongTyCon
    | -- | abstype datbind with dec end
      DAbstype [DatBind] [Dec]
    | -- | exception exbind
      DException [ExBind]
    | -- | local dec in dec end
      DLocal [Dec] [Dec]
    | -- | open longstrid+
      DOpen [LongStrId]
    | -- | infix d vid+
      DInfix Int [VId]
    | -- | infixr d vid+
      DInfixr Int [VId]
    | -- | nonfix vid+
      DNonfix [VId]
    | -- | Sequential declarations
      DSeq [Dec]
    | -- | Source position wrapper
      DPos Pos Dec
    deriving (Show)

-- | Value binding.
data ValBind = ValBind
    { vbRec :: Bool
    -- ^ Is this rec?
    , vbPat :: Pat
    , vbExp :: Exp
    }
    deriving (Show)

-- | Function binding: one or more clauses (Definition §2.7).
newtype FunBind = FunBind (NonEmpty FunClause)
    deriving (Show)

-- | A single clause of a function binding: f pat1 ... patn = exp.
data FunClause = FunClause VId [Pat] (Maybe Ty) Exp
    deriving (Show)

-- | Type binding.
data TypBind = TypBind [TyVar] TyCon Ty
    deriving (Show)

-- | Datatype binding (Definition §2.7, ≥1 constructor).
data DatBind = DatBind [TyVar] TyCon (NonEmpty ConBind)
    deriving (Show)

-- | Constructor binding.
data ConBind = ConBind VId (Maybe Ty)
    deriving (Show)

-- | Exception binding.
data ExBind
    = -- | exception C of ty
      ExNew VId (Maybe Ty)
    | -- | exception C = longvid
      ExRepl VId LongVId
    deriving (Show)

-- | A top-level program is a sequence of declarations.
newtype Program = Program [Dec]
    deriving (Show)
