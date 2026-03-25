{- | Idris 2 Case Tree Extraction Shim

Runs idris2 --dumpcases to get post-erasure case trees, then
parses the text output and emits OrganIR JSON.

Idris 2's --dumpcases output format:
  Main.factorial =
    \{arg:0} =>
      case arg:0 of
        0 => 1
        _ => (* arg:0 (Main.factorial (- arg:0 1)))

We parse this into definitions with case-tree bodies.
-}
module OrganBank.Idris2Shim (
    extractOrganIR,
) where

import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as T
import OrganIR.Build qualified as IR
import OrganIR.Json (renderOrganIR)
import OrganIR.Types qualified as IR
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName)
import System.Process (readProcessWithExitCode)

-- | Extract Idris 2 case trees from a source file and emit OrganIR JSON.
extractOrganIR :: FilePath -> IO (Either Text Text)
extractOrganIR inputPath = do
    let casesFile = "build/exec/" ++ takeBaseName inputPath ++ ".cases"
    -- First compile to get the case trees
    (ec, _stdout, stderrOut) <-
        readProcessWithExitCode
            "idris2"
            ["--dumpcases", casesFile, inputPath]
            ""
    case ec of
        ExitSuccess -> do
            dump <- readFile casesFile
            let modName = takeBaseName inputPath
                defs = parseCasesDump dump
            pure $ Right $ emitOrganIR modName defs
        ExitFailure _ -> do
            -- Try alternative: --dump-anf or --dump-lifted
            (ec2, _stdout2, _stderr2) <-
                readProcessWithExitCode
                    "idris2"
                    ["--dumpcases", casesFile, "--check", inputPath]
                    ""
            case ec2 of
                ExitSuccess -> do
                    dump <- readFile casesFile
                    let modName = takeBaseName inputPath
                        defs = parseCasesDump dump
                    pure $ Right $ emitOrganIR modName defs
                ExitFailure _ ->
                    pure $ Left $ T.pack $ "idris2 failed: " ++ stderrOut

-- | A parsed Idris 2 definition from --dumpcases output.
data Idris2Def = Idris2Def
    { idris2DefName :: String -- fully qualified name
    , idris2DefArity :: Int -- number of lambda parameters
    , idris2DefHasCase :: Bool -- whether body contains case expressions
    , idris2DefBodyText :: String -- raw case tree text
    }
    deriving (Show)

{- | Parse --dumpcases output into definitions.
Format:
  Qualified.Name =
    \{arg:0} => \{arg:1} =>
      <body>

  Next.Name =
    ...
-}
parseCasesDump :: String -> [Idris2Def]
parseCasesDump dump =
    let ls = lines dump
     in extractDefs ls

extractDefs :: [String] -> [Idris2Def]
extractDefs [] = []
extractDefs (l : ls)
    | " =" `isSuffixOf'` stripped && not (null defn) =
        let (body, rest) = spanBody ls
            arity = countLambdas body
            hasCase = any (("case " `isPrefixOf`) . dropWhile isSpace) body
            bodyText = unlines body
         in Idris2Def defn arity hasCase bodyText : extractDefs rest
    | otherwise = extractDefs ls
  where
    stripped = dropWhile isSpace l
    defn = takeWhile (\c -> isAlphaNum c || c == '.' || c == '_') stripped

isSuffixOf' :: String -> String -> Bool
isSuffixOf' suffix s =
    let n = length suffix
        m = length s
     in m >= n && drop (m - n) s == suffix

spanBody :: [String] -> ([String], [String])
spanBody = span (\l -> case l of { [] -> True; (c:_) -> isSpace c })

countLambdas :: [String] -> Int
countLambdas ls =
    length [() | l <- ls, "\\{" `isPrefixOf` dropWhile isSpace l]

-- | Convert a parsed Idris2Def to an OrganIR Definition.
defToIR :: Int -> Idris2Def -> IR.Definition
defToIR uid def' =
    let n = T.pack (idris2DefName def')
        arity = idris2DefArity def'
        qname = IR.QName "" (IR.Name n (fromIntegral uid))
        ty
            | arity == 0 = IR.TAny
            | otherwise = IR.TFn (replicate arity (IR.FnArg (Just IR.Many) IR.TAny)) IR.pureEffect IR.TAny
        bodyText = T.pack (idris2DefBodyText def')
        bodyExpr
            | idris2DefHasCase def' =
                IR.EApp (IR.eVar "case_tree") [IR.eString bodyText]
            | T.null (T.strip bodyText) =
                IR.EApp (IR.eVar "case_tree") [IR.eString "<empty>"]
            | otherwise =
                IR.EApp (IR.eVar "case_tree") [IR.eString bodyText]
     in IR.Definition
            { IR.defName = qname
            , IR.defType = ty
            , IR.defExpr = bodyExpr
            , IR.defSort = IR.SFun
            , IR.defVisibility = IR.Public
            , IR.defArity = arity
            }

-- | Emit OrganIR JSON from parsed definitions.
emitOrganIR :: String -> [Idris2Def] -> Text
emitOrganIR modName defs =
    renderOrganIR $ IR.organIR IR.LIdris2 "idris2-shim-0.1" (T.pack modName) (zipWith defToIR [1 ..] defs)
