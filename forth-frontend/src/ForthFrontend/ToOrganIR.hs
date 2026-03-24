{- | Translate Forth AST to OrganIR.
Strategy: colon definitions → functions, control flow → eIf/eSeq/loops,
stack words → function applications on an implicit stack.
-}
module ForthFrontend.ToOrganIR (emitForthIR) where

import Data.Text (Text)
import Data.Text qualified as T
import ForthFrontend.AST
import OrganIR.Build qualified as IR
import OrganIR.Json (renderOrganIR)
import OrganIR.Types qualified as IR

-- | Emit OrganIR JSON for a Forth program.
emitForthIR :: String -> FilePath -> [ForthItem] -> Text
emitForthIR modName srcFile items =
    renderOrganIR $
        IR.simpleOrganIR IR.LForth "forth-frontend-0.1" (T.pack modName) srcFile $
            concatMap itemToDefs items

itemToDefs :: ForthItem -> [IR.Definition]
itemToDefs = \case
    FDef nm body -> [IR.funDefNA nm 0 (IR.eSeq (map itemToIR body))]
    FVariable nm -> [IR.valDefSimple nm (IR.EApp (IR.eVar "variable") [IR.eString nm])]
    FConstant nm -> [IR.valDefSimple nm (IR.EApp (IR.eVar "constant") [IR.eString nm])]
    _ -> [] -- top-level literals/words outside definitions → skip

-- | Translate a Forth item to an OrganIR expression.
itemToIR :: ForthItem -> IR.Expr
itemToIR = \case
    FLitInt n -> IR.EApp (IR.eVar "push") [IR.eInt n]
    FLitFloat d -> IR.EApp (IR.eVar "push") [IR.eFloat d]
    FLitString s -> IR.EApp (IR.eVar "push") [IR.eString s]
    FWord w -> IR.EApp (IR.eVar (T.toLower w)) []
    FDef nm body ->
        -- Nested definition (non-standard but handle it)
        IR.EApp (IR.eVar "define") [IR.eString nm, IR.eSeq (map itemToIR body)]
    FIf thenBranch elseBranch ->
        IR.eIf
            (IR.EApp (IR.eVar "pop_flag") [])
            (IR.eSeq (map itemToIR thenBranch))
            (maybe IR.eNil (IR.eSeq . map itemToIR) elseBranch)
    FBeginUntil body ->
        IR.EApp (IR.eVar "begin_until") [IR.ELam [] (IR.eSeq (map itemToIR body))]
    FBeginWhile test body ->
        IR.EApp
            (IR.eVar "begin_while")
            [ IR.ELam [] (IR.eSeq (map itemToIR test))
            , IR.ELam [] (IR.eSeq (map itemToIR body))
            ]
    FDoLoop body ->
        IR.EApp (IR.eVar "do_loop") [IR.ELam [] (IR.eSeq (map itemToIR body))]
    FVariable nm -> IR.EApp (IR.eVar "variable") [IR.eString nm]
    FConstant nm -> IR.EApp (IR.eVar "constant") [IR.eString nm]
