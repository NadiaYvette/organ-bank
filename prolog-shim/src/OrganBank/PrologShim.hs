-- | Extract GNU Prolog WAM bytecode and emit OrganIR JSON.
module OrganBank.PrologShim (extractOrganIR) where

import Control.Exception (SomeException, catch)
import Data.Text (Text)
import Data.Text qualified as T
import OrganIR.Build qualified as IR
import OrganIR.Json (renderOrganIR)
import OrganIR.Types qualified as IR
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName)
import System.Process (readProcessWithExitCode)

-- | Detect gplc version by running @gplc --version@.
detectCompilerVersion :: IO Text
detectCompilerVersion = do
    (ec, out, _) <- readProcessWithExitCode "gplc" ["--version"] ""
    pure $ case ec of
        ExitSuccess | (l : _) <- lines out -> "prolog-shim-0.1 (" <> T.strip (T.pack l) <> ")"
        _ -> "prolog-shim-0.1"
  `catch` \(_ :: SomeException) -> pure "prolog-shim-0.1"

extractOrganIR :: FilePath -> IO (Either String Text)
extractOrganIR inputPath = do
    shimVer <- detectCompilerVersion
    (ec, out, err) <- readProcessWithExitCode "gplc" ["-W", inputPath] ""
    case ec of
        ExitFailure _ -> pure $ Left $ "gplc failed: " ++ err
        ExitSuccess -> do
            let defs = parseWam (T.pack out)
                modName = takeBaseName inputPath
                ir = IR.simpleOrganIR IR.LProlog shimVer (T.pack modName) inputPath (map toIRDef defs)
            pure $ Right $ renderOrganIR ir

toIRDef :: WamPred -> IR.Definition
toIRDef p =
    IR.Definition
        { IR.defName = IR.localName (wpName p <> "/" <> T.pack (show (wpArity p)))
        , IR.defType = IR.TAny
        , IR.defExpr = IR.EApp (IR.eVar "wam_body") (map IR.eString (wpInstructions p))
        , IR.defSort = IR.SFun
        , IR.defVisibility = if wpVisibility p == "public" then IR.Public else IR.Private
        , IR.defArity = wpArity p
        }

data WamPred = WamPred
    { wpName :: Text
    , wpArity :: Int
    , wpVisibility :: Text
    , wpInstructions :: [Text]
    }
    deriving (Show)

-- Parse gplc -W output:
-- predicate(name/arity,nclauses,static,public/private,monofile,global,[
--     instruction1,
--     instruction2,
--     ...])
parseWam :: Text -> [WamPred]
parseWam input = parsePreds (T.lines input)

parsePreds :: [Text] -> [WamPred]
parsePreds [] = []
parsePreds (l : ls)
    | "predicate(" `T.isPrefixOf` T.strip l =
        let (instrLines, rest) = collectUntilClose ls
            allText = T.concat (l : instrLines)
            prd = parsePredicate allText
         in prd : parsePreds rest
    | otherwise = parsePreds ls

collectUntilClose :: [Text] -> ([Text], [Text])
collectUntilClose [] = ([], [])
collectUntilClose (l : ls)
    | "])" `T.isInfixOf` l = ([l], ls)
    | otherwise = let (more, rest) = collectUntilClose ls in (l : more, rest)

parsePredicate :: Text -> WamPred
parsePredicate t =
    let
        -- Extract predicate(name/arity, ...)
        afterPred = T.drop 10 (T.strip t) -- drop "predicate("
        -- Get name/arity
        (nameArity, rest1) = T.breakOn "," afterPred
        (pname, arityStr) = T.breakOn "/" nameArity
        arity = case reads (T.unpack (T.drop 1 arityStr)) :: [(Int, String)] of
            [(n, _)] -> n
            _ -> 0
        -- Find visibility (public or private)
        vis = if "public" `T.isInfixOf` rest1 then "public" else "private"
        -- Extract instruction list between [ and ]
        instrText = extractBetween '[' ']' rest1
        instrs = parseInstructions instrText
     in
        WamPred (T.strip pname) arity vis instrs

extractBetween :: Char -> Char -> Text -> Text
extractBetween open close t =
    let afterOpen = T.drop 1 $ T.dropWhile (/= open) t
        beforeClose = T.takeWhile (/= close) afterOpen
     in beforeClose

parseInstructions :: Text -> [Text]
parseInstructions t =
    let parts = splitInstructions (T.strip t)
     in filter (not . T.null) $ map T.strip parts

-- Split on commas but respect parentheses
splitInstructions :: Text -> [Text]
splitInstructions t = go t (0 :: Int) "" []
  where
    go remaining depth current acc
        | T.null remaining = reverse (T.pack (reverse current) : acc)
        | otherwise =
            let c = T.head remaining
                rest = T.tail remaining
             in case c of
                    '(' -> go rest (depth + 1) (c : current) acc
                    ')' -> go rest (depth - 1) (c : current) acc
                    ',' | depth == 0 -> go rest 0 "" (T.pack (reverse current) : acc)
                    '\n' -> go rest depth current acc
                    _ -> go rest depth (c : current) acc
