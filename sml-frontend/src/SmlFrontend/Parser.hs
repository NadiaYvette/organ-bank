-- | Recursive descent parser for SML core language.
module SmlFrontend.Parser (parseSml) where

import Data.Char (isAsciiUpper)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import SmlFrontend.Lexer
import SmlFrontend.Syntax.AST
import SmlFrontend.Syntax.Const
import SmlFrontend.Syntax.Ident

-- | Parser state: remaining tokens.
newtype PState = PState [Token]

type PResult a = Either String a

parseSml :: [Token] -> PResult Program
parseSml toks = do
    (decs, _) <- parseDecs (PState toks)
    return (Program decs)

-- Peek at the current token kind.
peek :: PState -> TokenKind
peek (PState []) = KEOF
peek (PState (t : _)) = tokKind t

peekPos :: PState -> Pos
peekPos (PState []) = noPos
peekPos (PState (t : _)) = tokPos t

-- Advance past one token.
advance :: PState -> PState
advance (PState []) = PState []
advance (PState (_ : ts)) = PState ts

-- Expect a specific token.
expect :: TokenKind -> PState -> PResult PState
expect k st
    | peek st == k = Right (advance st)
    | otherwise =
        Left $
            "Expected "
                ++ show k
                ++ " but got "
                ++ show (peek st)
                ++ " at "
                ++ showPos (peekPos st)

showPos :: Pos -> String
showPos (Pos l c) = show l ++ ":" ++ show c

-- Try to consume a token, returning whether it was consumed.
tryConsume :: TokenKind -> PState -> (Bool, PState)
tryConsume k st
    | peek st == k = (True, advance st)
    | otherwise = (False, st)

-- Get identifier text
getId :: PState -> PResult (Text, PState)
getId st = case peek st of
    KId t -> Right (t, advance st)
    KStar -> Right ("*", advance st)
    -- \* is a valid tycon
    _ -> Left $ "Expected identifier at " ++ showPos (peekPos st)

getVId :: PState -> PResult (VId, PState)
getVId st = do (t, st') <- getId st; return (VId t, st')

getSymOrId :: PState -> PResult (VId, PState)
getSymOrId st = case peek st of
    KId t -> Right (VId t, advance st)
    KSymId t -> Right (VId t, advance st)
    KStar -> Right (VId "*", advance st)
    KEquals -> Right (VId "=", advance st)
    _ -> Left $ "Expected identifier at " ++ showPos (peekPos st)

-- Parse longvid: strid1.strid2...vid
-- Also handles qualified names like Foo.bar or Int.max
parseLongVId :: PState -> PResult (LongVId, PState)
parseLongVId st = do
    (VId first, st') <- getSymOrId st
    collectQual (first :| []) st'
  where
    collectQual parts s = case peek s of
        KDot -> case peek (advance s) of
            KId _ -> do (t, s') <- getId (advance s); collectQual (parts <> (t :| [])) s'
            KSymId _ -> let KSymId t = peek (advance s) in collectQual (parts <> (t :| [])) (advance (advance s))
            _ -> finish parts s
        _ -> finish parts s
    finish parts s =
        let (prefixes, vid) = (NE.init parts, NE.last parts)
         in return (LongVId (map StrId prefixes) (VId vid), s)

parseLongTyCon :: PState -> PResult (LongTyCon, PState)
parseLongTyCon st = do
    (t, st') <- getId st
    collectQualTc (t :| []) st'
  where
    collectQualTc parts s = case peek s of
        KDot -> case peek (advance s) of
            KId _ -> do (t, s') <- getId (advance s); collectQualTc (parts <> (t :| [])) s'
            _ -> finish parts s
        _ -> finish parts s
    finish parts s =
        let (prefixes, tc) = (NE.init parts, NE.last parts)
         in return (LongTyCon (map StrId prefixes) (TyCon tc), s)

-- | Parse a sequence of declarations.
parseDecs :: PState -> PResult ([Dec], PState)
parseDecs st = go st []
  where
    go s acc = case peek s of
        KVal -> do (d, s') <- parseValDec s; go s' (d : acc)
        KFun -> do (d, s') <- parseFunDec s; go s' (d : acc)
        KType -> do (d, s') <- parseTypeDec s; go s' (d : acc)
        KDatatype -> do (d, s') <- parseDatatypeDec s; go s' (d : acc)
        KException -> do (d, s') <- parseExceptionDec s; go s' (d : acc)
        KLocal -> do (d, s') <- parseLocalDec s; go s' (d : acc)
        KInfix -> do (d, s') <- parseInfixDec s; go s' (d : acc)
        KInfixr -> do (d, s') <- parseInfixrDec s; go s' (d : acc)
        KNonfix -> do (d, s') <- parseNonfixDec s; go s' (d : acc)
        KOpen -> do (d, s') <- parseOpenDec s; go s' (d : acc)
        KSemicolon -> go (advance s) acc -- skip optional semicolons
        _ -> return (reverse acc, s)

-- val [rec] pat = exp [and ...]
parseValDec :: PState -> PResult (Dec, PState)
parseValDec st = do
    st1 <- expect KVal st
    -- Optional tyvar sequence (skip for now)
    (binds, st2) <- parseValBinds st1
    return (DVal [] binds, st2)

parseValBinds :: PState -> PResult ([ValBind], PState)
parseValBinds st = do
    let (isRec, st1) = tryConsume KRec st
    (pat, st2) <- parsePat st1
    st3 <- expect KEquals st2
    (exp_, st4) <- parseExp st3
    let bind = ValBind isRec pat exp_
        (hasAnd, st5) = tryConsume KAnd st4
    if hasAnd
        then do
            (more, st6) <- parseValBinds st5
            return (bind : more, st6)
        else return ([bind], st4)

-- fun f pat1 ... patn = exp | f pat1' ... = exp' [and ...]
parseFunDec :: PState -> PResult (Dec, PState)
parseFunDec st = do
    st1 <- expect KFun st
    (binds, st2) <- parseFunBinds st1
    return (DFun [] binds, st2)

parseFunBinds :: PState -> PResult ([FunBind], PState)
parseFunBinds st = do
    (clauses, st1) <- parseFunClauses st
    let (hasAnd, st2) = tryConsume KAnd st1
    if hasAnd
        then do
            (more, st3) <- parseFunBinds st2
            return (FunBind clauses : more, st3)
        else return ([FunBind clauses], st1)

parseFunClauses :: PState -> PResult (NonEmpty FunClause, PState)
parseFunClauses st = do
    (clause, st1) <- parseFunClause st
    let (hasBar, st2) = tryConsume KBar st1
    if hasBar
        then do
            (more, st3) <- parseFunClauses st2
            return (clause :| NE.toList more, st3)
        else return (clause :| [], st1)

parseFunClause :: PState -> PResult (FunClause, PState)
parseFunClause st = do
    (name, st1) <- getVId st
    (pats, st2) <- parseAtomPats st1
    -- Optional return type annotation
    let (retTy, st3) = case peek st2 of
            KColon -> case parseTy (advance st2) of
                Right (ty, s) -> (Just ty, s)
                Left _ -> (Nothing, st2)
            _ -> (Nothing, st2)
    st4 <- expect KEquals st3
    (body, st5) <- parseExp st4
    return (FunClause name pats retTy body, st5)

parseAtomPats :: PState -> PResult ([Pat], PState)
parseAtomPats st = go st []
  where
    go s acc = case tryAtomPat s of
        Just (p, s') -> go s' (p : acc)
        Nothing -> return (reverse acc, s)

-- type tycon = ty [and ...]
parseTypeDec :: PState -> PResult (Dec, PState)
parseTypeDec st = do
    st1 <- expect KType st
    (binds, st2) <- parseTypBinds st1
    return (DType binds, st2)

parseTypBinds :: PState -> PResult ([TypBind], PState)
parseTypBinds st = do
    -- Optional tyvar(s) before tycon
    (tyvars, st1) <- parseTyVarSeq st
    (TyCon name, st2) <- parseTyCon st1
    st3 <- expect KEquals st2
    (ty, st4) <- parseTy st3
    let bind = TypBind tyvars (TyCon name) ty
        (hasAnd, st5) = tryConsume KAnd st4
    if hasAnd
        then do
            (more, st6) <- parseTypBinds st5
            return (bind : more, st6)
        else return ([bind], st4)

parseTyCon :: PState -> PResult (TyCon, PState)
parseTyCon st = do (t, st') <- getId st; return (TyCon t, st')

parseTyVarSeq :: PState -> PResult ([TyVar], PState)
parseTyVarSeq st = case peek st of
    KTyVar name eq -> return ([TyVar name eq], advance st)
    KLParen -> case peek (advance st) of
        KTyVar _ _ -> parseTyVarList (advance st)
        _ -> return ([], st)
    _ -> return ([], st)

parseTyVarList :: PState -> PResult ([TyVar], PState)
parseTyVarList st = case peek st of
    KTyVar name eq -> do
        let st1 = advance st
        case peek st1 of
            KComma -> do
                (more, st2) <- parseTyVarList (advance st1)
                return (TyVar name eq : more, st2)
            KRParen -> return ([TyVar name eq], advance st1)
            _ -> Left $ "Expected , or ) in tyvar list at " ++ showPos (peekPos st1)
    _ -> Left $ "Expected type variable at " ++ showPos (peekPos st)

-- datatype tycon = C [of ty] | ... [and ...]
parseDatatypeDec :: PState -> PResult (Dec, PState)
parseDatatypeDec st = do
    st1 <- expect KDatatype st
    (binds, st2) <- parseDatBinds st1
    return (DDatatype binds, st2)

parseDatBinds :: PState -> PResult ([DatBind], PState)
parseDatBinds st = do
    (tyvars, st1) <- parseTyVarSeq st
    (TyCon name, st2) <- parseTyCon st1
    st3 <- expect KEquals st2
    (cons, st4) <- parseConBinds st3
    let bind = DatBind tyvars (TyCon name) cons
        (hasAnd, st5) = tryConsume KAnd st4
    if hasAnd
        then do
            (more, st6) <- parseDatBinds st5
            return (bind : more, st6)
        else return ([bind], st4)

parseConBinds :: PState -> PResult (NonEmpty ConBind, PState)
parseConBinds st = do
    (con, st1) <- parseConBind st
    let (hasBar, st2) = tryConsume KBar st1
    if hasBar
        then do
            (more, st3) <- parseConBinds st2
            return (con :| NE.toList more, st3)
        else return (con :| [], st1)

parseConBind :: PState -> PResult (ConBind, PState)
parseConBind st = do
    (VId name, st1) <- getVId st
    case peek st1 of
        KOf -> do
            (ty, st2) <- parseTy (advance st1)
            return (ConBind (VId name) (Just ty), st2)
        _ -> return (ConBind (VId name) Nothing, st1)

-- exception C [of ty]
parseExceptionDec :: PState -> PResult (Dec, PState)
parseExceptionDec st = do
    st1 <- expect KException st
    (VId name, st2) <- getVId st1
    case peek st2 of
        KOf -> do
            (ty, st3) <- parseTy (advance st2)
            return (DException [ExNew (VId name) (Just ty)], st3)
        _ -> return (DException [ExNew (VId name) Nothing], st2)

-- local dec in dec end
parseLocalDec :: PState -> PResult (Dec, PState)
parseLocalDec st = do
    st1 <- expect KLocal st
    (decs1, st2) <- parseDecs st1
    st3 <- expect KIn st2
    (decs2, st4) <- parseDecs st3
    st5 <- expect KEnd st4
    return (DLocal decs1 decs2, st5)

parseInfixDec :: PState -> PResult (Dec, PState)
parseInfixDec st = do
    st1 <- expect KInfix st
    let (prec, st2) = case peek st1 of
            KInt n -> (fromIntegral n, advance st1)
            _ -> (0, st1)
    (vids, st3) <- parseVIdList st2
    return (DInfix prec vids, st3)

parseInfixrDec :: PState -> PResult (Dec, PState)
parseInfixrDec st = do
    st1 <- expect KInfixr st
    let (prec, st2) = case peek st1 of
            KInt n -> (fromIntegral n, advance st1)
            _ -> (0, st1)
    (vids, st3) <- parseVIdList st2
    return (DInfixr prec vids, st3)

parseNonfixDec :: PState -> PResult (Dec, PState)
parseNonfixDec st = do
    st1 <- expect KNonfix st
    (vids, st2) <- parseVIdList st1
    return (DNonfix vids, st2)

parseVIdList :: PState -> PResult ([VId], PState)
parseVIdList st = case peek st of
    KId _ -> do (v, st') <- getSymOrId st; (vs, st'') <- parseVIdList st'; return (v : vs, st'')
    KSymId _ -> do (v, st') <- getSymOrId st; (vs, st'') <- parseVIdList st'; return (v : vs, st'')
    _ -> return ([], st)

parseOpenDec :: PState -> PResult (Dec, PState)
parseOpenDec st = do
    st1 <- expect KOpen st
    (t, st2) <- getId st1
    return (DOpen [LongStrId [] (StrId t)], st2)

-- | Parse an expression.
parseExp :: PState -> PResult (Exp, PState)
parseExp = parseExpHandle

-- Handle: exp handle match
parseExpHandle :: PState -> PResult (Exp, PState)
parseExpHandle st = do
    (e, st1) <- parseExpOrelse st
    case peek st1 of
        KHandle -> do
            (rules, st2) <- parseMatch (advance st1)
            return (EHandle e rules, st2)
        _ -> return (e, st1)

parseExpOrelse :: PState -> PResult (Exp, PState)
parseExpOrelse st = do
    (e, st1) <- parseExpAndalso st
    case peek st1 of
        KOrelse -> do
            (e2, st2) <- parseExpOrelse (advance st1)
            return (EOrelse e e2, st2)
        _ -> return (e, st1)

parseExpAndalso :: PState -> PResult (Exp, PState)
parseExpAndalso st = do
    (e, st1) <- parseExpTyped st
    case peek st1 of
        KAndalso -> do
            (e2, st2) <- parseExpAndalso (advance st1)
            return (EAndalso e e2, st2)
        _ -> return (e, st1)

-- Typed: exp : ty
parseExpTyped :: PState -> PResult (Exp, PState)
parseExpTyped st = do
    (e, st1) <- parseExpInfix st
    case peek st1 of
        KColon -> do
            (ty, st2) <- parseTy (advance st1)
            return (ETyped e ty, st2)
        _ -> return (e, st1)

-- Infix expressions (left-to-right, no precedence resolution yet)
parseExpInfix :: PState -> PResult (Exp, PState)
parseExpInfix st = do
    (e, st1) <- parseExpApp st
    infixCont e st1

infixCont :: Exp -> PState -> PResult (Exp, PState)
infixCont lhs st = case peek st of
    KSymId op | op /= "=>" && op /= "->" && op /= "|" -> do
        let st1 = advance st
        (rhs, st2) <- parseExpApp st1
        infixCont (EInfix lhs (VId op) rhs) st2
    KStar -> do
        let st1 = advance st
        (rhs, st2) <- parseExpApp st1
        infixCont (EInfix lhs (VId "*") rhs) st2
    KEquals -> do
        let st1 = advance st
        (rhs, st2) <- parseExpApp st1
        infixCont (EInfix lhs (VId "=") rhs) st2
    _ -> return (lhs, st)

-- Application: fexp aexp aexp ...
parseExpApp :: PState -> PResult (Exp, PState)
parseExpApp st = do
    (f, st1) <- parseExpAtom st
    appCont f st1

appCont :: Exp -> PState -> PResult (Exp, PState)
appCont f st = case tryAtomExp st of
    Just (arg, st') -> appCont (EApp f arg) st'
    Nothing -> return (f, st)

-- Can we parse an atomic expression here?
tryAtomExp :: PState -> Maybe (Exp, PState)
tryAtomExp st = case peek st of
    KInt _ -> Just (ESCon (SInt n), advance st) where KInt n = peek st
    KReal _ -> Just (ESCon (SReal d), advance st) where KReal d = peek st
    KString _ -> Just (ESCon (SString s), advance st) where KString s = peek st
    KChar _ -> Just (ESCon (SChar c), advance st) where KChar c = peek st
    KId _ -> case parseLongVId st of Right (v, st') -> Just (EVar v, st'); _ -> Nothing
    KOp -> case getSymOrId (advance st) of
        Right (vid, st') -> Just (EVar (LongVId [] vid), st')
        _ -> Nothing
    KLParen -> case parseParenExp (advance st) of Right r -> Just r; _ -> Nothing
    KLBrack -> case parseListExp (advance st) of Right r -> Just r; _ -> Nothing
    KHash -> case getId (advance st) of
        Right (lab, st') -> Just (ESelector (Lab lab), st')
        _ -> Nothing
    _ -> Nothing

-- Atomic expressions
parseExpAtom :: PState -> PResult (Exp, PState)
parseExpAtom st = case peek st of
    KInt n -> return (ESCon (SInt n), advance st)
    KReal d -> return (ESCon (SReal d), advance st)
    KString s -> return (ESCon (SString s), advance st)
    KChar c -> return (ESCon (SChar c), advance st)
    KId _ -> do (v, st') <- parseLongVId st; return (EVar v, st')
    KOp -> do
        (vid, st') <- getSymOrId (advance st)
        return (EVar (LongVId [] vid), st')
    KLParen -> parseParenExp (advance st)
    KLBrack -> parseListExp (advance st)
    KLet -> parseLetExp st
    KIf -> parseIfExp st
    KCase -> parseCaseExp st
    KFn -> parseFnExp st
    KRaise -> do
        (e, st') <- parseExp (advance st)
        return (ERaise e, st')
    KWhile -> parseWhileExp st
    KHash -> do
        (lab, st') <- getId (advance st)
        return (ESelector (Lab lab), st')
    _ -> Left $ "Expected expression at " ++ showPos (peekPos st) ++ ", got " ++ show (peek st)

-- (exp) or (exp, ..., exp) or () or (exp; ...; exp)
parseParenExp :: PState -> PResult (Exp, PState)
parseParenExp st = case peek st of
    KRParen -> return (ETuple [], advance st) -- unit
    _ -> do
        (e, st1) <- parseExp st
        case peek st1 of
            KRParen -> return (e, advance st1)
            KComma -> do
                (es, st2) <- parseExpCommaList st1
                return (ETuple (e : es), st2)
            KSemicolon -> do
                (es, st2) <- parseExpSemiList st1
                return (ESeq (e : es), st2)
            _ -> Left $ "Expected ) or , at " ++ showPos (peekPos st1)

parseExpCommaList :: PState -> PResult ([Exp], PState)
parseExpCommaList st = do
    st1 <- expect KComma st
    (e, st2) <- parseExp st1
    case peek st2 of
        KComma -> do (more, st3) <- parseExpCommaList st2; return (e : more, st3)
        KRParen -> return ([e], advance st2)
        _ -> Left $ "Expected , or ) at " ++ showPos (peekPos st2)

parseExpSemiList :: PState -> PResult ([Exp], PState)
parseExpSemiList st = do
    st1 <- expect KSemicolon st
    (e, st2) <- parseExp st1
    case peek st2 of
        KSemicolon -> do (more, st3) <- parseExpSemiList st2; return (e : more, st3)
        KRParen -> return ([e], advance st2)
        _ -> Left $ "Expected ; or ) at " ++ showPos (peekPos st2)

-- [exp, ..., exp]
parseListExp :: PState -> PResult (Exp, PState)
parseListExp st = case peek st of
    KRBrack -> return (EList [], advance st)
    _ -> do
        (e, st1) <- parseExp st
        case peek st1 of
            KRBrack -> return (EList [e], advance st1)
            KComma -> do (es, st2) <- parseListTail st1; return (EList (e : es), st2)
            _ -> Left $ "Expected ] or , at " ++ showPos (peekPos st1)

parseListTail :: PState -> PResult ([Exp], PState)
parseListTail st = do
    st1 <- expect KComma st
    (e, st2) <- parseExp st1
    case peek st2 of
        KComma -> do (more, st3) <- parseListTail st2; return (e : more, st3)
        KRBrack -> return ([e], advance st2)
        _ -> Left $ "Expected , or ] at " ++ showPos (peekPos st2)

-- let dec in exp [; exp ...] end
parseLetExp :: PState -> PResult (Exp, PState)
parseLetExp st = do
    st1 <- expect KLet st
    (decs, st2) <- parseDecs st1
    st3 <- expect KIn st2
    (e, st4) <- parseExp st3
    -- Allow semicolons for sequential expressions in let body
    (es, st5) <- parseLetBody st4
    st6 <- expect KEnd st5
    let body = case es of [] -> e; _ -> ESeq (e : es)
    return (ELet decs body, st6)

parseLetBody :: PState -> PResult ([Exp], PState)
parseLetBody st = case peek st of
    KSemicolon -> do
        let st1 = advance st
        case peek st1 of
            KEnd -> return ([], st1)
            _ -> do
                (e, st2) <- parseExp st1
                (more, st3) <- parseLetBody st2
                return (e : more, st3)
    _ -> return ([], st)

-- if exp then exp else exp
parseIfExp :: PState -> PResult (Exp, PState)
parseIfExp st = do
    st1 <- expect KIf st
    (cond, st2) <- parseExp st1
    st3 <- expect KThen st2
    (thn, st4) <- parseExp st3
    st5 <- expect KElse st4
    (els, st6) <- parseExp st5
    return (EIf cond thn els, st6)

-- case exp of match
parseCaseExp :: PState -> PResult (Exp, PState)
parseCaseExp st = do
    st1 <- expect KCase st
    (e, st2) <- parseExp st1
    st3 <- expect KOf st2
    (rules, st4) <- parseMatch st3
    return (ECase e rules, st4)

-- fn match
parseFnExp :: PState -> PResult (Exp, PState)
parseFnExp st = do
    st1 <- expect KFn st
    (rules, st2) <- parseMatch st1
    return (EFn rules, st2)

-- while exp do exp
parseWhileExp :: PState -> PResult (Exp, PState)
parseWhileExp st = do
    st1 <- expect KWhile st
    (cond, st2) <- parseExp st1
    st3 <- expect KDo st2
    (body, st4) <- parseExp st3
    return (EWhile cond body, st4)

-- match: pat => exp [| pat => exp ...]
parseMatch :: PState -> PResult (NonEmpty MRule, PState)
parseMatch st = do
    -- Optional leading bar
    let st0 = snd (tryConsume KBar st)
    (pat, st1) <- parsePat st0
    st2 <- expect KDArrow st1
    (e, st3) <- parseExp st2
    let (hasBar, st4) = tryConsume KBar st3
    if hasBar
        then do
            (more, st5) <- parseMatchCont st4
            return (MRule pat e :| NE.toList more, st5)
        else return (MRule pat e :| [], st3)

parseMatchCont :: PState -> PResult (NonEmpty MRule, PState)
parseMatchCont st = do
    (pat, st1) <- parsePat st
    st2 <- expect KDArrow st1
    (e, st3) <- parseExp st2
    let (hasBar, st4) = tryConsume KBar st3
    if hasBar
        then do
            (more, st5) <- parseMatchCont st4
            return (MRule pat e :| NE.toList more, st5)
        else return (MRule pat e :| [], st3)

-- | Parse a pattern.
parsePat :: PState -> PResult (Pat, PState)
parsePat st = do
    (p, st1) <- parsePatTyped st
    -- Check for infix patterns (e.g., x :: xs)
    case peek st1 of
        KSymId op -> do
            let st2 = advance st1
            (p2, st3) <- parsePat st2
            return (PInfix p (VId op) p2, st3)
        _ -> return (p, st1)

parsePatTyped :: PState -> PResult (Pat, PState)
parsePatTyped st = do
    (p, st1) <- parsePatAtom st
    case peek st1 of
        KColon -> do
            (ty, st2) <- parseTy (advance st1)
            return (PTyped p ty, st2)
        _ -> return (p, st1)

tryAtomPat :: PState -> Maybe (Pat, PState)
tryAtomPat st = case peek st of
    KUnderscore -> Just (PWild, advance st)
    KId t -> Just (PVar (VId t), advance st)
    KInt n -> Just (PSCon (SInt n), advance st)
    KString s -> Just (PSCon (SString s), advance st)
    KChar c -> Just (PSCon (SChar c), advance st)
    KLParen -> case parseParenPat (advance st) of Right r -> Just r; _ -> Nothing
    KLBrack -> case parseListPat (advance st) of Right r -> Just r; _ -> Nothing
    _ -> Nothing

parsePatAtom :: PState -> PResult (Pat, PState)
parsePatAtom st = case peek st of
    KUnderscore -> return (PWild, advance st)
    KInt n -> return (PSCon (SInt n), advance st)
    KString s -> return (PSCon (SString s), advance st)
    KChar c -> return (PSCon (SChar c), advance st)
    KOp -> do
        (vid, st1) <- getSymOrId (advance st)
        case tryAtomPat st1 of
            Just (arg, st2) -> return (PCon (LongVId [] vid) (Just arg), st2)
            Nothing -> return (PVar vid, st1)
    KId _ -> do
        (VId name, st1) <- getVId st
        -- Check for "as" pattern
        case peek st1 of
            KAs -> do
                (p, st2) <- parsePat (advance st1)
                return (PAs (VId name) p, st2)
            _ -> case tryAtomPat st1 of
                -- If followed by another atom pattern, treat as constructor application
                Just (arg, st2)
                    | isConstructorName name ->
                        return (PCon (LongVId [] (VId name)) (Just arg), st2)
                _ -> return (PVar (VId name), st1)
    KLParen -> parseParenPat (advance st)
    KLBrack -> parseListPat (advance st)
    _ -> Left $ "Expected pattern at " ++ showPos (peekPos st)

isConstructorName :: Text -> Bool
isConstructorName t = case T.uncons t of
    Just (c, _) -> isAsciiUpper c
    Nothing -> False

parseParenPat :: PState -> PResult (Pat, PState)
parseParenPat st = case peek st of
    KRParen -> return (PTuple [], advance st) -- unit
    _ -> do
        (p, st1) <- parsePat st
        case peek st1 of
            KRParen -> return (p, advance st1)
            KComma -> do
                (ps, st2) <- parsePatCommaList st1
                return (PTuple (p : ps), st2)
            _ -> Left $ "Expected ) or , in pattern at " ++ showPos (peekPos st1)

parsePatCommaList :: PState -> PResult ([Pat], PState)
parsePatCommaList st = do
    st1 <- expect KComma st
    (p, st2) <- parsePat st1
    case peek st2 of
        KComma -> do (more, st3) <- parsePatCommaList st2; return (p : more, st3)
        KRParen -> return ([p], advance st2)
        _ -> Left $ "Expected , or ) at " ++ showPos (peekPos st2)

parseListPat :: PState -> PResult (Pat, PState)
parseListPat st = case peek st of
    KRBrack -> return (PList [], advance st)
    _ -> do
        (p, st1) <- parsePat st
        case peek st1 of
            KRBrack -> return (PList [p], advance st1)
            KComma -> do
                (ps, st2) <- parsePatListTail st1
                return (PList (p : ps), st2)
            _ -> Left $ "Expected ] or , at " ++ showPos (peekPos st1)

parsePatListTail :: PState -> PResult ([Pat], PState)
parsePatListTail st = do
    st1 <- expect KComma st
    (p, st2) <- parsePat st1
    case peek st2 of
        KComma -> do (more, st3) <- parsePatListTail st2; return (p : more, st3)
        KRBrack -> return ([p], advance st2)
        _ -> Left $ "Expected , or ] at " ++ showPos (peekPos st2)

-- | Parse a type expression.
parseTy :: PState -> PResult (Ty, PState)
parseTy st = do
    (ty, st1) <- parseTyTuple st
    case peek st1 of
        KArrow -> do
            (ty2, st2) <- parseTy (advance st1)
            return (TyFun ty ty2, st2)
        _ -> return (ty, st1)

parseTyTuple :: PState -> PResult (Ty, PState)
parseTyTuple st = do
    (ty, st1) <- parseTyApp st
    case peek st1 of
        KStar -> do
            (tys, st2) <- parseTyStarList st1
            return (TyTuple (ty : tys), st2)
        _ -> return (ty, st1)

parseTyStarList :: PState -> PResult ([Ty], PState)
parseTyStarList st = do
    st1 <- expect KStar st
    (ty, st2) <- parseTyApp st1
    case peek st2 of
        KStar -> do (more, st3) <- parseTyStarList st2; return (ty : more, st3)
        _ -> return ([ty], st2)

-- Type application: ty tycon or (ty, ty) tycon
parseTyApp :: PState -> PResult (Ty, PState)
parseTyApp st = do
    (ty, st1) <- parseTyAtom st
    tyAppCont ty st1

tyAppCont :: Ty -> PState -> PResult (Ty, PState)
tyAppCont ty st = case peek st of
    KId _ -> do
        (tc, st') <- parseLongTyCon st
        tyAppCont (TyApp [ty] tc) st'
    _ -> return (ty, st)

parseTyAtom :: PState -> PResult (Ty, PState)
parseTyAtom st = case peek st of
    KTyVar name eq -> return (TyTyVar (TyVar name eq), advance st)
    KId _ -> do
        (tc, st') <- parseLongTyCon st
        return (TyApp [] tc, st')
    KLParen -> do
        (ty, st1) <- parseTy (advance st)
        case peek st1 of
            KRParen -> return (ty, advance st1)
            KComma -> do
                (tys, st2) <- parseTyCommaList st1
                st3 <- expect KRParen st2
                -- This is (ty, ty, ...) tycon
                case peek st3 of
                    KId _ -> do
                        (tc, st4) <- parseLongTyCon st3
                        return (TyApp (ty : tys) tc, st4)
                    _ -> Left $ "Expected type constructor after (...) at " ++ showPos (peekPos st3)
            _ -> Left $ "Expected ) or , at " ++ showPos (peekPos st1)
    KLBrace -> parseRecordTy (advance st)
    _ -> Left $ "Expected type at " ++ showPos (peekPos st) ++ ", got " ++ show (peek st)

parseTyCommaList :: PState -> PResult ([Ty], PState)
parseTyCommaList st = do
    st1 <- expect KComma st
    (ty, st2) <- parseTy st1
    case peek st2 of
        KComma -> do (more, st3) <- parseTyCommaList st2; return (ty : more, st3)
        _ -> return ([ty], st2)

parseRecordTy :: PState -> PResult (Ty, PState)
parseRecordTy st = case peek st of
    KRBrace -> return (TyRecord [], advance st)
    _ -> do
        (fields, st1) <- parseRecordTyFields st
        st2 <- expect KRBrace st1
        return (TyRecord fields, st2)

parseRecordTyFields :: PState -> PResult ([(Lab, Ty)], PState)
parseRecordTyFields st = do
    (lab, st1) <- getId st
    st2 <- expect KColon st1
    (ty, st3) <- parseTy st2
    case peek st3 of
        KComma -> do
            (more, st4) <- parseRecordTyFields (advance st3)
            return ((Lab lab, ty) : more, st4)
        _ -> return ([(Lab lab, ty)], st3)
