{- | Compact binary serialization for OrganIR.

Wire format:
  - 4-byte magic: "ORIR"
  - 2-byte version: 0, 1
  - Payload: length-prefixed fields, tag bytes for unions
  - Integers: variable-length (7 bits/byte, high bit = continuation)
  - Text: varint length + UTF-8 bytes
  - Lists: varint length + elements
  - Optionals: 0x00 Nothing | 0x01 + value
  - Tagged unions: tag byte + fields
-}
module OrganIR.Binary
    ( encodeOrganIR
    , decodeOrganIR
    ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)
import OrganIR.Types

-- ============================================================================
-- Encoding
-- ============================================================================

type Builder = [ByteString] -> [ByteString]

runBuilder :: Builder -> ByteString
runBuilder b = BS.concat (b [])

emit :: ByteString -> Builder
emit bs = (bs :)

emitByte :: Word8 -> Builder
emitByte w = emit (BS.singleton w)

-- | Variable-length encoding for non-negative integers.
-- 7 bits per byte, high bit = more bytes follow.
emitVarUInt :: Integer -> Builder
emitVarUInt n
    | n < 0 = error "emitVarUInt: negative"
    | n < 128 = emitByte (fromIntegral n)
    | otherwise = emitByte (fromIntegral (n .&. 0x7f) .|. 0x80) . emitVarUInt (shiftR n 7)

-- | Signed integer: zigzag encoding then varuint.
emitVarInt :: Integer -> Builder
emitVarInt n
    | n >= 0 = emitVarUInt (2 * n)
    | otherwise = emitVarUInt (2 * negate n - 1)

emitText :: Text -> Builder
emitText t =
    let bs = TE.encodeUtf8 t
     in emitVarUInt (fromIntegral (BS.length bs)) . emit bs

emitBool :: Bool -> Builder
emitBool False = emitByte 0
emitBool True = emitByte 1

emitList :: (a -> Builder) -> [a] -> Builder
emitList f xs = emitVarUInt (fromIntegral (length xs)) . foldr (\x acc -> f x . acc) id xs

emitMaybe :: (a -> Builder) -> Maybe a -> Builder
emitMaybe _ Nothing = emitByte 0
emitMaybe f (Just x) = emitByte 1 . f x

emitDouble :: Double -> Builder
emitDouble d = emitText (T.pack (show d))

-- | Encode OrganIR to compact binary.
encodeOrganIR :: OrganIR -> ByteString
encodeOrganIR ir = runBuilder $
    -- Magic: "ORIR"
    emit (BS.pack [0x4F, 0x52, 0x49, 0x52])
    -- Version: 0, 1
    . emit (BS.pack [0, 1])
    . putOrganIR ir

putOrganIR :: OrganIR -> Builder
putOrganIR (OrganIR meta modul) = putMetadata meta . putModule modul

putMetadata :: Metadata -> Builder
putMetadata (Metadata lang compVer srcFile shimVer ts) =
    putSourceLang lang
    . emitMaybe emitText compVer
    . emitMaybe emitText srcFile
    . emitText shimVer
    . emitMaybe emitText ts

putSourceLang :: SourceLang -> Builder
putSourceLang = emitByte . sourceLangTag

sourceLangTag :: SourceLang -> Word8
sourceLangTag = \case
    LHaskell -> 0; LRust -> 1; LMercury -> 2; LIdris2 -> 3; LLean4 -> 4
    LKoka -> 5; LOCaml -> 6; LSwift -> 7; LErlang -> 8; LPurescript -> 9
    LAgda -> 10; LFSharp -> 11; LScala3 -> 12; LJulia -> 13; LZig -> 14
    LC -> 15; LCpp -> 16; LFortran -> 17; LAda -> 18; LSml -> 19
    LCommonLisp -> 20; LScheme -> 21; LProlog -> 22; LLua -> 23; LForth -> 24

putModule :: Module -> Builder
putModule (Module nm exports imports defs dts eds) =
    emitText nm
    . emitList emitText exports
    . emitList putQName imports
    . emitList putDefinition defs
    . emitList putDataType dts
    . emitList putEffectDecl eds

putName :: Name -> Builder
putName (Name t u) = emitText t . emitVarInt (fromIntegral u)

putQName :: QName -> Builder
putQName (QName m n) = emitText m . putName n

putDefinition :: Definition -> Builder
putDefinition (Definition qn ty expr sort vis arity) =
    putQName qn
    . putTy ty
    . putExpr expr
    . putSort sort
    . putVisibility vis
    . emitVarUInt (fromIntegral arity)

putSort :: Sort -> Builder
putSort = emitByte . \case SFun -> 0; SVal -> 1; SExternal -> 2; SCon -> 3

putVisibility :: Visibility -> Builder
putVisibility = emitByte . \case Public -> 0; Private -> 1

putTy :: Ty -> Builder
putTy = \case
    TAny -> emitByte 0
    TCon qn -> emitByte 1 . putQName qn
    TApp qn args -> emitByte 2 . putQName qn . emitList putTy args
    TFn args eff result -> emitByte 3 . emitList putFnArg args . putEffectRow eff . putTy result
    TForall vars body -> emitByte 4 . emitList putTyVar vars . putTy body
    TVar n -> emitByte 5 . putName n
    TSyn qn expansion -> emitByte 6 . putQName qn . putTy expansion

putFnArg :: FnArg -> Builder
putFnArg (FnArg mult ty) = emitMaybe putMultiplicity mult . putTy ty

putMultiplicity :: Multiplicity -> Builder
putMultiplicity = emitByte . \case Many -> 0; Affine -> 1; Linear -> 2

putTyVar :: TyVar -> Builder
putTyVar (TyVar n kind) = putName n . emitMaybe emitText kind

putEffectRow :: EffectRow -> Builder
putEffectRow (EffectRow effs tail_) = emitList putQName effs . emitMaybe putName tail_

putExpr :: Expr -> Builder
putExpr = \case
    EVar n -> emitByte 0 . putName n
    ELit lit -> emitByte 1 . putLit lit
    ECon qn args -> emitByte 2 . putQName qn . emitList putExpr args
    EApp fn args -> emitByte 3 . putExpr fn . emitList putExpr args
    ELam params body -> emitByte 4 . emitList putLamParam params . putExpr body
    ELet binds body -> emitByte 5 . emitList putLetBind binds . putExpr body
    ECase scrut branches -> emitByte 6 . putExpr scrut . emitList putBranch branches
    ETypeApp e tys -> emitByte 7 . putExpr e . emitList putTy tys
    ETypeLam vars body -> emitByte 8 . emitList putTyVar vars . putExpr body
    EPerform eff op args -> emitByte 9 . putQName eff . emitText op . emitList putExpr args
    EHandle eff body handler -> emitByte 10 . putQName eff . putExpr body . putExpr handler
    ERetain n -> emitByte 11 . putName n
    ERelease n -> emitByte 12 . putName n
    EDrop n -> emitByte 13 . putName n
    EReuse n -> emitByte 14 . putName n
    EDelay e -> emitByte 15 . putExpr e
    EForce e -> emitByte 16 . putExpr e
    ETuple es -> emitByte 17 . emitList putExpr es
    EList es -> emitByte 18 . emitList putExpr es
    ERaise e -> emitByte 19 . putExpr e
    EUnreachable -> emitByte 20

putLit :: Lit -> Builder
putLit = \case
    LitInt n -> emitByte 0 . emitVarInt n
    LitFloat d -> emitByte 1 . emitDouble d
    LitString s -> emitByte 2 . emitText s
    LitBool b -> emitByte 3 . emitBool b

putLamParam :: LamParam -> Builder
putLamParam (LamParam n mty) = putName n . emitMaybe putTy mty

putLetBind :: LetBind -> Builder
putLetBind (LetBind n mty e) = putName n . emitMaybe putTy mty . putExpr e

putBranch :: Branch -> Builder
putBranch (Branch pat body) = putPat pat . putExpr body

putPat :: Pat -> Builder
putPat = \case
    PatCon qn binders -> emitByte 0 . putQName qn . emitList putPatBinder binders
    PatLit lit -> emitByte 1 . putLit lit
    PatVar n mty -> emitByte 2 . putName n . emitMaybe putTy mty
    PatWild -> emitByte 3

putPatBinder :: PatBinder -> Builder
putPatBinder (PatBinder n mty) = putName n . emitMaybe putTy mty

putDataType :: DataType -> Builder
putDataType (DataType qn tparams ctors) =
    putQName qn . emitList putTyVar tparams . emitList putConstructor ctors

putConstructor :: Constructor -> Builder
putConstructor (Constructor qn fields) = putQName qn . emitList putTy fields

putEffectDecl :: EffectDecl -> Builder
putEffectDecl (EffectDecl qn tparams ops) =
    putQName qn . emitList putTyVar tparams . emitList putOperation ops

putOperation :: Operation -> Builder
putOperation (Operation n ty) = emitText n . putTy ty

-- ============================================================================
-- Decoding
-- ============================================================================

type Get a = ByteString -> Either String (a, ByteString)

decodeOrganIR :: ByteString -> Either String OrganIR
decodeOrganIR bs = do
    -- Check magic
    if BS.length bs < 6
        then Left "OrganIR binary: input too short (need at least 6 bytes for header)"
        else do
            let magic = BS.take 4 bs
            if magic /= BS.pack [0x4F, 0x52, 0x49, 0x52]
                then Left $ "OrganIR binary: bad magic number (expected \"ORIR\", got "
                         ++ show (map (\b -> if b >= 0x20 && b < 0x7f then toEnum (fromIntegral b) :: Char else '?') (BS.unpack magic))
                         ++ ")"
                else do
                    let verMaj = BS.index bs 4
                        verMin = BS.index bs 5
                    if verMaj /= 0 || verMin /= 1
                        then Left $ "OrganIR binary: unsupported version "
                                 ++ show verMaj ++ "." ++ show verMin
                                 ++ " (expected 0.1)"
                        else do
                            let rest = BS.drop 6 bs
                            (ir, remainder) <- getOrganIR rest
                            if BS.null remainder
                                then Right ir
                                else Left $ "OrganIR binary: " ++ show (BS.length remainder) ++ " trailing bytes"

getByte :: Get Word8
getByte bs
    | BS.null bs = Left "Unexpected end of input"
    | otherwise = Right (BS.head bs, BS.tail bs)

getVarUInt :: Get Integer
getVarUInt bs = go 0 0 bs
  where
    go :: Integer -> Int -> Get Integer
    go acc shift s = do
        (b, rest) <- getByte s
        let val = acc .|. (fromIntegral (b .&. 0x7f) `shiftL` shift)
        if b .&. 0x80 == 0
            then Right (val, rest)
            else go val (shift + 7) rest

getVarInt :: Get Integer
getVarInt bs = do
    (n, rest) <- getVarUInt bs
    let val = if n `mod` 2 == 0 then n `div` 2 else negate ((n + 1) `div` 2)
    Right (val, rest)

getText :: Get Text
getText bs = do
    (len, rest) <- getVarUInt bs
    let len' = fromIntegral len :: Int
    if BS.length rest < len'
        then Left "Unexpected end of input in text field"
        else do
            let (bytes, rest') = BS.splitAt len' rest
            case TE.decodeUtf8' bytes of
                Left err -> Left $ "Invalid UTF-8: " ++ show err
                Right t -> Right (t, rest')

getBool :: Get Bool
getBool bs = do
    (b, rest) <- getByte bs
    case b of
        0 -> Right (False, rest)
        1 -> Right (True, rest)
        _ -> Left $ "Invalid bool byte: " ++ show b

getDouble :: Get Double
getDouble bs = do
    (t, rest) <- getText bs
    case reads (T.unpack t) of
        [(d, "")] -> Right (d, rest)
        _ -> Left $ "Invalid double: " ++ T.unpack t

getList :: Get a -> Get [a]
getList getElem bs = do
    (len, rest) <- getVarUInt bs
    go (fromIntegral len :: Int) rest []
  where
    go 0 s acc = Right (reverse acc, s)
    go n s acc = do
        (x, s') <- getElem s
        go (n - 1) s' (x : acc)

getMaybe :: Get a -> Get (Maybe a)
getMaybe getVal bs = do
    (tag, rest) <- getByte bs
    case tag of
        0 -> Right (Nothing, rest)
        1 -> do
            (v, rest') <- getVal rest
            Right (Just v, rest')
        _ -> Left $ "Invalid Maybe tag: " ++ show tag

getOrganIR :: Get OrganIR
getOrganIR bs = do
    (meta, rest) <- getMetadata bs
    (modul, rest') <- getModule rest
    Right (OrganIR meta modul, rest')

getMetadata :: Get Metadata
getMetadata bs = do
    (lang, r1) <- getSourceLang bs
    (compVer, r2) <- getMaybe getText r1
    (srcFile, r3) <- getMaybe getText r2
    (shimVer, r4) <- getText r3
    (ts, r5) <- getMaybe getText r4
    Right (Metadata lang compVer srcFile shimVer ts, r5)

getSourceLang :: Get SourceLang
getSourceLang bs = do
    (tag, rest) <- getByte bs
    case tag of
        0 -> Right (LHaskell, rest); 1 -> Right (LRust, rest)
        2 -> Right (LMercury, rest); 3 -> Right (LIdris2, rest)
        4 -> Right (LLean4, rest); 5 -> Right (LKoka, rest)
        6 -> Right (LOCaml, rest); 7 -> Right (LSwift, rest)
        8 -> Right (LErlang, rest); 9 -> Right (LPurescript, rest)
        10 -> Right (LAgda, rest); 11 -> Right (LFSharp, rest)
        12 -> Right (LScala3, rest); 13 -> Right (LJulia, rest)
        14 -> Right (LZig, rest); 15 -> Right (LC, rest)
        16 -> Right (LCpp, rest); 17 -> Right (LFortran, rest)
        18 -> Right (LAda, rest); 19 -> Right (LSml, rest)
        20 -> Right (LCommonLisp, rest); 21 -> Right (LScheme, rest)
        22 -> Right (LProlog, rest); 23 -> Right (LLua, rest)
        24 -> Right (LForth, rest)
        _ -> Left $ "Unknown SourceLang tag: " ++ show tag

getModule :: Get Module
getModule bs = do
    (nm, r1) <- getText bs
    (exports, r2) <- getList getText r1
    (imports, r3) <- getList getQName r2
    (defs, r4) <- getList getDefinition r3
    (dts, r5) <- getList getDataType r4
    (eds, r6) <- getList getEffectDecl r5
    Right (Module nm exports imports defs dts eds, r6)

getName :: Get Name
getName bs = do
    (t, r1) <- getText bs
    (u, r2) <- getVarInt r1
    Right (Name t (fromIntegral u), r2)

getQName :: Get QName
getQName bs = do
    (m, r1) <- getText bs
    (n, r2) <- getName r1
    Right (QName m n, r2)

getDefinition :: Get Definition
getDefinition bs = do
    (qn, r1) <- getQName bs
    (ty, r2) <- getTy r1
    (expr, r3) <- getExpr r2
    (sort, r4) <- getSort r3
    (vis, r5) <- getVisibility r4
    (arity, r6) <- getVarUInt r5
    Right (Definition qn ty expr sort vis (fromIntegral arity), r6)

getSort :: Get Sort
getSort bs = do
    (tag, rest) <- getByte bs
    case tag of
        0 -> Right (SFun, rest); 1 -> Right (SVal, rest)
        2 -> Right (SExternal, rest); 3 -> Right (SCon, rest)
        _ -> Left $ "Unknown Sort tag: " ++ show tag

getVisibility :: Get Visibility
getVisibility bs = do
    (tag, rest) <- getByte bs
    case tag of
        0 -> Right (Public, rest); 1 -> Right (Private, rest)
        _ -> Left $ "Unknown Visibility tag: " ++ show tag

getTy :: Get Ty
getTy bs = do
    (tag, rest) <- getByte bs
    case tag of
        0 -> Right (TAny, rest)
        1 -> do
            (qn, r) <- getQName rest
            Right (TCon qn, r)
        2 -> do
            (qn, r1) <- getQName rest
            (args, r2) <- getList getTy r1
            Right (TApp qn args, r2)
        3 -> do
            (args, r1) <- getList getFnArg rest
            (eff, r2) <- getEffectRow r1
            (result, r3) <- getTy r2
            Right (TFn args eff result, r3)
        4 -> do
            (vars, r1) <- getList getTyVar rest
            (body, r2) <- getTy r1
            Right (TForall vars body, r2)
        5 -> do
            (n, r) <- getName rest
            Right (TVar n, r)
        6 -> do
            (qn, r1) <- getQName rest
            (expansion, r2) <- getTy r1
            Right (TSyn qn expansion, r2)
        _ -> Left $ "Unknown Ty tag: " ++ show tag

getFnArg :: Get FnArg
getFnArg bs = do
    (mult, r1) <- getMaybe getMultiplicity bs
    (ty, r2) <- getTy r1
    Right (FnArg mult ty, r2)

getMultiplicity :: Get Multiplicity
getMultiplicity bs = do
    (tag, rest) <- getByte bs
    case tag of
        0 -> Right (Many, rest); 1 -> Right (Affine, rest); 2 -> Right (Linear, rest)
        _ -> Left $ "Unknown Multiplicity tag: " ++ show tag

getTyVar :: Get TyVar
getTyVar bs = do
    (n, r1) <- getName bs
    (kind, r2) <- getMaybe getText r1
    Right (TyVar n kind, r2)

getEffectRow :: Get EffectRow
getEffectRow bs = do
    (effs, r1) <- getList getQName bs
    (tail_, r2) <- getMaybe getName r1
    Right (EffectRow effs tail_, r2)

getExpr :: Get Expr
getExpr bs = do
    (tag, rest) <- getByte bs
    case tag of
        0 -> do (n, r) <- getName rest; Right (EVar n, r)
        1 -> do (lit, r) <- getLit rest; Right (ELit lit, r)
        2 -> do
            (qn, r1) <- getQName rest
            (args, r2) <- getList getExpr r1
            Right (ECon qn args, r2)
        3 -> do
            (fn, r1) <- getExpr rest
            (args, r2) <- getList getExpr r1
            Right (EApp fn args, r2)
        4 -> do
            (params, r1) <- getList getLamParam rest
            (body, r2) <- getExpr r1
            Right (ELam params body, r2)
        5 -> do
            (binds, r1) <- getList getLetBind rest
            (body, r2) <- getExpr r1
            Right (ELet binds body, r2)
        6 -> do
            (scrut, r1) <- getExpr rest
            (branches, r2) <- getList getBranch r1
            Right (ECase scrut branches, r2)
        7 -> do
            (e, r1) <- getExpr rest
            (tys, r2) <- getList getTy r1
            Right (ETypeApp e tys, r2)
        8 -> do
            (vars, r1) <- getList getTyVar rest
            (body, r2) <- getExpr r1
            Right (ETypeLam vars body, r2)
        9 -> do
            (eff, r1) <- getQName rest
            (op, r2) <- getText r1
            (args, r3) <- getList getExpr r2
            Right (EPerform eff op args, r3)
        10 -> do
            (eff, r1) <- getQName rest
            (body, r2) <- getExpr r1
            (handler, r3) <- getExpr r2
            Right (EHandle eff body handler, r3)
        11 -> do (n, r) <- getName rest; Right (ERetain n, r)
        12 -> do (n, r) <- getName rest; Right (ERelease n, r)
        13 -> do (n, r) <- getName rest; Right (EDrop n, r)
        14 -> do (n, r) <- getName rest; Right (EReuse n, r)
        15 -> do (e, r) <- getExpr rest; Right (EDelay e, r)
        16 -> do (e, r) <- getExpr rest; Right (EForce e, r)
        17 -> do (es, r) <- getList getExpr rest; Right (ETuple es, r)
        18 -> do (es, r) <- getList getExpr rest; Right (EList es, r)
        19 -> do (e, r) <- getExpr rest; Right (ERaise e, r)
        20 -> Right (EUnreachable, rest)
        _ -> Left $ "Unknown Expr tag: " ++ show tag

getLit :: Get Lit
getLit bs = do
    (tag, rest) <- getByte bs
    case tag of
        0 -> do (n, r) <- getVarInt rest; Right (LitInt n, r)
        1 -> do (d, r) <- getDouble rest; Right (LitFloat d, r)
        2 -> do (s, r) <- getText rest; Right (LitString s, r)
        3 -> do (b, r) <- getBool rest; Right (LitBool b, r)
        _ -> Left $ "Unknown Lit tag: " ++ show tag

getLamParam :: Get LamParam
getLamParam bs = do
    (n, r1) <- getName bs
    (mty, r2) <- getMaybe getTy r1
    Right (LamParam n mty, r2)

getLetBind :: Get LetBind
getLetBind bs = do
    (n, r1) <- getName bs
    (mty, r2) <- getMaybe getTy r1
    (e, r3) <- getExpr r2
    Right (LetBind n mty e, r3)

getBranch :: Get Branch
getBranch bs = do
    (pat, r1) <- getPat bs
    (body, r2) <- getExpr r1
    Right (Branch pat body, r2)

getPat :: Get Pat
getPat bs = do
    (tag, rest) <- getByte bs
    case tag of
        0 -> do
            (qn, r1) <- getQName rest
            (binders, r2) <- getList getPatBinder r1
            Right (PatCon qn binders, r2)
        1 -> do (lit, r) <- getLit rest; Right (PatLit lit, r)
        2 -> do
            (n, r1) <- getName rest
            (mty, r2) <- getMaybe getTy r1
            Right (PatVar n mty, r2)
        3 -> Right (PatWild, rest)
        _ -> Left $ "Unknown Pat tag: " ++ show tag

getPatBinder :: Get PatBinder
getPatBinder bs = do
    (n, r1) <- getName bs
    (mty, r2) <- getMaybe getTy r1
    Right (PatBinder n mty, r2)

getDataType :: Get DataType
getDataType bs = do
    (qn, r1) <- getQName bs
    (tparams, r2) <- getList getTyVar r1
    (ctors, r3) <- getList getConstructor r2
    Right (DataType qn tparams ctors, r3)

getConstructor :: Get Constructor
getConstructor bs = do
    (qn, r1) <- getQName bs
    (fields, r2) <- getList getTy r1
    Right (Constructor qn fields, r2)

getEffectDecl :: Get EffectDecl
getEffectDecl bs = do
    (qn, r1) <- getQName bs
    (tparams, r2) <- getList getTyVar r1
    (ops, r3) <- getList getOperation r2
    Right (EffectDecl qn tparams ops, r3)

getOperation :: Get Operation
getOperation bs = do
    (n, r1) <- getText bs
    (ty, r2) <- getTy r1
    Right (Operation n ty, r2)
