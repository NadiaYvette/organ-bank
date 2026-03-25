-- | organ-playground: Minimal web playground for OrganIR extraction.
module Main (main) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Types (status200, status400, status404, status405)
import Network.Wai (Application, Request, Response, pathInfo, requestMethod,
                    responseLBS, lazyRequestBody)
import Network.Wai.Handler.Warp (run)

-- organ-ir
import OrganIR.Json (renderOrganIR)
import OrganIR.Parse (parseOrganIR)
import OrganIR.Pretty (ppOrganIR)
import OrganIR.Validate (validateOrganIR, Warning(..), Severity(..))

-- SML frontend
import SmlFrontend.Basis (initialConEnv, initialEnv)
import SmlFrontend.Elab.Infer (inferProgram, initInferState)
import SmlFrontend.Lexer (lexSml)
import SmlFrontend.Parser (parseSml)
import SmlFrontend.ToOrganIR (emitOrganIR)

-- Erlang frontend
import ErlangFrontend.Lexer (lexErlang)
import ErlangFrontend.Parser (parseForms)
import ErlangFrontend.ToOrganIR (emitErlangIR)

-- Scheme frontend
import SchemeFrontend.Desugar (desugarProgram)
import SchemeFrontend.Lexer (lexScheme)
import SchemeFrontend.Reader (readDatums)
import SchemeFrontend.ToOrganIR (emitSchemeIR)

-- Prolog frontend
import PrologFrontend.Lexer (lexProlog)
import PrologFrontend.Parser (parseSentences)
import PrologFrontend.ToOrganIR (emitPrologIR)

-- Lua frontend
import LuaFrontend.Lexer (lexLua)
import LuaFrontend.Parser (parseLua)
import LuaFrontend.ToOrganIR (emitLuaIR)

-- Forth frontend
import ForthFrontend.Lexer (lexForth)
import ForthFrontend.Parser (parseForth)
import ForthFrontend.ToOrganIR (emitForthIR)

main :: IO ()
main = do
    putStrLn "organ-playground running on http://localhost:8080"
    run 8080 app

app :: Application
app req respond = case (requestMethod req, pathInfo req) of
    ("GET",  [])          -> respond indexResponse
    ("GET",  [""])         -> respond indexResponse
    ("POST", ["extract"]) -> handleExtract req respond
    ("GET",  _)           -> respond notFound
    ("POST", _)           -> respond notFound
    _                     -> respond methodNotAllowed

indexResponse :: Response
indexResponse = responseLBS status200
    [("Content-Type", "text/html; charset=utf-8")]
    (LBS.fromStrict (TE.encodeUtf8 indexHtml))

notFound :: Response
notFound = responseLBS status404
    [("Content-Type", "application/json")]
    "{\"error\": \"not found\"}"

methodNotAllowed :: Response
methodNotAllowed = responseLBS status405
    [("Content-Type", "application/json")]
    "{\"error\": \"method not allowed\"}"

handleExtract :: Request -> (Response -> IO a) -> IO a
handleExtract req respond = do
    body <- lazyRequestBody req
    let bodyText = TE.decodeUtf8 (LBS.toStrict body)
    case parseRequest bodyText of
        Left err -> respond $ responseLBS status400
            [("Content-Type", "application/json")]
            (LBS.fromStrict (TE.encodeUtf8 (jsonError err)))
        Right (lang, source) ->
            case extractIR lang source of
                Left err -> respond $ responseLBS status200
                    [("Content-Type", "application/json")]
                    (LBS.fromStrict (TE.encodeUtf8 (jsonError err)))
                Right jsonText ->
                    case parseOrganIR jsonText of
                        Left parseErr -> respond $ responseLBS status200
                            [("Content-Type", "application/json")]
                            (LBS.fromStrict (TE.encodeUtf8 (jsonResult jsonText "" [
                                Warning Error "parse" ("re-parse failed: " <> parseErr)])))
                        Right ir ->
                            let pretty = ppOrganIR ir
                                warnings = validateOrganIR ir
                                -- Re-render to get canonical JSON
                                canonical = renderOrganIR ir
                            in respond $ responseLBS status200
                                [("Content-Type", "application/json")]
                                (LBS.fromStrict (TE.encodeUtf8 (jsonResult canonical pretty warnings)))

-- | Tiny JSON request parser: expects {"language": "...", "source": "..."}
parseRequest :: Text -> Either Text (Text, Text)
parseRequest t =
    let lang = extractField "language" t
        src  = extractField "source" t
    in case (lang, src) of
        (Just l, Just s) -> Right (l, s)
        _ -> Left "expected JSON with \"language\" and \"source\" fields"

-- | Extract a string field value from simple JSON (no nesting in values).
extractField :: Text -> Text -> Maybe Text
extractField field json =
    let needle = "\"" <> field <> "\""
    in case T.breakOn needle json of
        (_, rest)
            | T.null rest -> Nothing
            | otherwise ->
                let afterKey = T.drop (T.length needle) rest
                    afterColon = T.dropWhile (\c -> c == ':' || c == ' ' || c == '\t') afterKey
                in case T.uncons afterColon of
                    Just ('"', valRest) -> Just (extractJsonString valRest)
                    _ -> Nothing

-- | Extract a JSON string value, handling escape sequences.
extractJsonString :: Text -> Text
extractJsonString = go mempty
  where
    go acc t = case T.uncons t of
        Nothing -> acc
        Just ('"', _) -> acc
        Just ('\\', rest) -> case T.uncons rest of
            Just ('n', rest') -> go (acc <> "\n") rest'
            Just ('t', rest') -> go (acc <> "\t") rest'
            Just ('\\', rest') -> go (acc <> "\\") rest'
            Just ('"', rest') -> go (acc <> "\"") rest'
            Just (c, rest') -> go (acc <> "\\" <> T.singleton c) rest'
            Nothing -> acc
        Just (c, rest) -> go (acc <> T.singleton c) rest

-- | Route extraction to the appropriate frontend.
extractIR :: Text -> Text -> Either String Text
extractIR lang source = case lang of
    "sml"     -> extractSml source
    "erlang"  -> extractErlang source
    "scheme"  -> extractScheme source
    "prolog"  -> extractProlog source
    "lua"     -> extractLua source
    "forth"   -> extractForth source
    _         -> Left ("unsupported language: " <> T.unpack lang)

extractSml :: Text -> Either String Text
extractSml src = do
    tokens <- lexSml src
    ast <- parseSml tokens
    let st0 = initInferState initialEnv initialConEnv
    st <- inferProgram st0 ast
    Right (emitOrganIR "playground" "<playground>" ast st)

extractErlang :: Text -> Either String Text
extractErlang src = do
    tokens <- lexErlang src
    forms <- parseForms tokens
    Right (emitErlangIR "playground" "<playground>" forms)

extractScheme :: Text -> Either String Text
extractScheme src = do
    tokens <- lexScheme src
    datums <- readDatums tokens
    tops <- desugarProgram datums
    Right (emitSchemeIR "playground" "<playground>" tops)

extractProlog :: Text -> Either String Text
extractProlog src = do
    tokens <- lexProlog src
    sentences <- parseSentences tokens
    Right (emitPrologIR "playground" "<playground>" sentences)

extractLua :: Text -> Either String Text
extractLua src = do
    tokens <- lexLua src
    block <- parseLua tokens
    Right (emitLuaIR "playground" "<playground>" block)

extractForth :: Text -> Either String Text
extractForth src = do
    tokens <- lexForth src
    items <- parseForth tokens
    Right (emitForthIR "playground" "<playground>" items)

-- | Build a JSON error response.
jsonError :: Text -> Text
jsonError msg = "{\"error\": " <> jsonStr msg <> "}"

-- | Build a successful JSON response.
jsonResult :: Text -> Text -> [Warning] -> Text
jsonResult irJson pretty warnings =
    "{\"organir_json\": " <> jsonStr irJson
    <> ", \"organir_pretty\": " <> jsonStr pretty
    <> ", \"validation\": " <> renderWarnings warnings
    <> "}"

renderWarnings :: [Warning] -> Text
renderWarnings ws = "[" <> T.intercalate ", " (map renderWarning ws) <> "]"

renderWarning :: Warning -> Text
renderWarning w =
    "{\"severity\": " <> jsonStr (sevText (wSeverity w))
    <> ", \"path\": " <> jsonStr (wPath w)
    <> ", \"message\": " <> jsonStr (wMessage w)
    <> "}"

sevText :: Severity -> Text
sevText = \case
    Info -> "info"
    Warn -> "warn"
    Error -> "error"

-- | Escape a Text value as a JSON string.
jsonStr :: Text -> Text
jsonStr t = "\"" <> T.concatMap esc t <> "\""
  where
    esc '"'  = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\t' = "\\t"
    esc '\r' = "\\r"
    esc c    = T.singleton c

-- | The inline HTML/CSS/JS for the playground UI.
indexHtml :: Text
indexHtml = T.unlines
    [ "<!DOCTYPE html>"
    , "<html lang=\"en\">"
    , "<head>"
    , "<meta charset=\"UTF-8\">"
    , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
    , "<title>OrganIR Playground</title>"
    , "<style>"
    , "*, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }"
    , "body {"
    , "  font-family: 'SF Mono', 'Cascadia Code', 'Fira Code', monospace;"
    , "  background: #1a1a2e; color: #e0e0e0;"
    , "  min-height: 100vh; padding: 20px;"
    , "}"
    , "h1 { color: #e94560; margin-bottom: 4px; font-size: 1.5em; }"
    , ".subtitle { color: #666; font-size: 0.85em; margin-bottom: 20px; }"
    , ".controls {"
    , "  display: flex; gap: 12px; align-items: center; margin-bottom: 16px;"
    , "}"
    , "select, button {"
    , "  font-family: inherit; font-size: 0.9em; padding: 8px 16px;"
    , "  border: 1px solid #333; border-radius: 4px;"
    , "  background: #16213e; color: #e0e0e0; cursor: pointer;"
    , "}"
    , "button {"
    , "  background: #e94560; color: #fff; border-color: #e94560;"
    , "  font-weight: bold;"
    , "}"
    , "button:hover { background: #c73652; }"
    , "button:disabled { opacity: 0.5; cursor: not-allowed; }"
    , "textarea {"
    , "  width: 100%; height: 200px; font-family: inherit; font-size: 0.85em;"
    , "  background: #0f3460; color: #e0e0e0; border: 1px solid #333;"
    , "  border-radius: 4px; padding: 12px; resize: vertical;"
    , "  tab-size: 2;"
    , "}"
    , ".output-grid {"
    , "  display: grid; grid-template-columns: 1fr 1fr; gap: 12px; margin-top: 16px;"
    , "}"
    , ".output-panel {"
    , "  background: #0f3460; border: 1px solid #333; border-radius: 4px;"
    , "  padding: 12px; min-height: 150px; max-height: 400px; overflow: auto;"
    , "}"
    , ".output-panel.wide { grid-column: 1 / -1; }"
    , ".output-panel h3 {"
    , "  color: #e94560; font-size: 0.85em; margin-bottom: 8px;"
    , "  text-transform: uppercase; letter-spacing: 1px;"
    , "}"
    , ".output-panel pre {"
    , "  font-size: 0.8em; white-space: pre-wrap; word-break: break-all;"
    , "  line-height: 1.5;"
    , "}"
    , ".error { color: #ff6b6b; }"
    , ".warn { color: #ffd93d; }"
    , ".info { color: #6bcb77; }"
    , ".empty { color: #555; font-style: italic; }"
    , "</style>"
    , "</head>"
    , "<body>"
    , "<h1>OrganIR Playground</h1>"
    , "<p class=\"subtitle\">Paste source code, select a language, and extract the OrganIR representation.</p>"
    , "<div class=\"controls\">"
    , "  <select id=\"lang\">"
    , "    <option value=\"sml\">SML</option>"
    , "    <option value=\"erlang\">Erlang</option>"
    , "    <option value=\"scheme\">Scheme</option>"
    , "    <option value=\"prolog\">Prolog</option>"
    , "    <option value=\"lua\">Lua</option>"
    , "    <option value=\"forth\">Forth</option>"
    , "  </select>"
    , "  <button id=\"extract\" onclick=\"doExtract()\">Extract</button>"
    , "</div>"
    , "<textarea id=\"source\" spellcheck=\"false\">fun fact 0 = 1\n  | fact n = n * fact (n - 1)</textarea>"
    , "<div class=\"output-grid\">"
    , "  <div class=\"output-panel\">"
    , "    <h3>Pretty</h3>"
    , "    <pre id=\"out-pretty\" class=\"empty\">Click Extract to see output</pre>"
    , "  </div>"
    , "  <div class=\"output-panel\">"
    , "    <h3>Validation</h3>"
    , "    <pre id=\"out-validation\" class=\"empty\">Click Extract to see output</pre>"
    , "  </div>"
    , "  <div class=\"output-panel wide\">"
    , "    <h3>JSON</h3>"
    , "    <pre id=\"out-json\" class=\"empty\">Click Extract to see output</pre>"
    , "  </div>"
    , "</div>"
    , "<script>"
    , "const examples = {"
    , "  sml: 'fun fact 0 = 1\\n  | fact n = n * fact (n - 1)',"
    , "  erlang: '-module(demo).\\n-export([fact/1]).\\nfact(0) -> 1;\\nfact(N) -> N * fact(N - 1).',"
    , "  scheme: '(define (fact n)\\n  (if (= n 0) 1 (* n (fact (- n 1)))))',"
    , "  prolog: 'fact(0, 1).\\nfact(N, F) :- N > 0, N1 is N - 1, fact(N1, F1), F is N * F1.',"
    , "  lua: 'function fact(n)\\n  if n == 0 then return 1 end\\n  return n * fact(n - 1)\\nend',"
    , "  forth: ': fact ( n -- n! ) dup 0= if drop 1 else dup 1- recurse * then ;'"
    , "};"
    , "document.getElementById('lang').addEventListener('change', function() {"
    , "  document.getElementById('source').value = examples[this.value] || '';"
    , "});"
    , "async function doExtract() {"
    , "  const btn = document.getElementById('extract');"
    , "  btn.disabled = true; btn.textContent = 'Extracting...';"
    , "  const lang = document.getElementById('lang').value;"
    , "  const source = document.getElementById('source').value;"
    , "  try {"
    , "    const resp = await fetch('/extract', {"
    , "      method: 'POST',"
    , "      headers: {'Content-Type': 'application/json'},"
    , "      body: JSON.stringify({language: lang, source: source})"
    , "    });"
    , "    const data = await resp.json();"
    , "    if (data.error) {"
    , "      document.getElementById('out-pretty').innerHTML = '<span class=\"error\">' + esc(data.error) + '</span>';"
    , "      document.getElementById('out-json').innerHTML = '<span class=\"error\">' + esc(data.error) + '</span>';"
    , "      document.getElementById('out-validation').innerHTML = '';"
    , "    } else {"
    , "      document.getElementById('out-pretty').textContent = data.organir_pretty || '(empty)';"
    , "      try {"
    , "        document.getElementById('out-json').textContent = JSON.stringify(JSON.parse(data.organir_json), null, 2);"
    , "      } catch(e) {"
    , "        document.getElementById('out-json').textContent = data.organir_json || '(empty)';"
    , "      }"
    , "      const val = data.validation || [];"
    , "      if (val.length === 0) {"
    , "        document.getElementById('out-validation').innerHTML = '<span class=\"info\">No warnings</span>';"
    , "      } else {"
    , "        document.getElementById('out-validation').innerHTML = val.map(w =>"
    , "          '<span class=\"' + w.severity + '\">[' + w.severity.toUpperCase() + '] ' + esc(w.path) + ': ' + esc(w.message) + '</span>'"
    , "        ).join('\\n');"
    , "      }"
    , "    }"
    , "  } catch(e) {"
    , "    document.getElementById('out-pretty').innerHTML = '<span class=\"error\">Network error: ' + esc(e.message) + '</span>';"
    , "  } finally {"
    , "    btn.disabled = false; btn.textContent = 'Extract';"
    , "  }"
    , "}"
    , "function esc(s) { const d = document.createElement('div'); d.textContent = s; return d.innerHTML; }"
    , "</script>"
    , "</body>"
    , "</html>"
    ]
