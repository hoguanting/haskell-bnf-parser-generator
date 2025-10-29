module Assignment (bnfParser, generateHaskellCode, validate, ADT, getTime, firstRuleName) where

import Control.Applicative (empty, many, optional, some, (<|>))
import Data.Char (isAlphaNum, toLower, toUpper)
import Data.List (intercalate, nub, sort, unsnoc)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Instances (Parser (..))
import Parser
    ( inlineSpace
    , inlineSpaces
    , is
    , lower
    , satisfy
    , string
    )

-- | The entire grammar is represented as a list of rule definitions.
type ADT = [Rule]

-- | A single grammar rule consisting of its name, optional parameters, and definition.
data Rule = Rule
    { ruleName :: String
    , ruleParams :: [String]
    , ruleDefinition :: Definition
    }
    deriving (Show, Eq)

-- | A definition is a list of alternatives separated by '|'.
type Definition = [Alternative]

-- | An alternative is an ordered sequence of terms.
type Alternative = [Term]

-- | The building blocks of an alternative.
data Term
    = TNonTerminal String
    | TTerminal String
    | TMacro MacroType
    | TParamRef String
    | TParamUsage String [Term]
    | TModified Term Modifier
    deriving (Show, Eq)

-- | Supported builtin macro references.
data MacroType
    = MInt
    | MAlpha
    | MNewline
    | MSpace
    deriving (Show, Eq)

-- | Supported postfix modifiers.
data Modifier
    = MTok
    | MStar
    | MPlus
    | MQuestion
    deriving (Show, Eq)

-- | Parser for the full BNF document.
bnfParser :: Parser ADT
bnfParser =
    skipBlankLines
        *> some (pRule <* skipBlankLines)
        <* inlineSpaces

-- | Generate Haskell code from a validated ADT.
generateHaskellCode :: ADT -> String
generateHaskellCode adt =
    let uniqueRules = keepFirstRules adt
        filteredRules = filterInvalidRules uniqueRules
        dataDecls = map renderDataDecl filteredRules
        parserDecls = map renderParserDecl filteredRules
        sections = filter (not . null) [intercalate "\n\n" dataDecls, intercalate "\n\n" parserDecls]
        body = intercalate "\n\n" sections
    in body ++ "\n"

-- | Validate the ADT and produce user-facing warnings.
validate :: ADT -> [String]
validate adt =
    let uniqueRules = keepFirstRules adt
        dupWarnings = duplicateWarnings adt
        undefinedWarns =
            map ("Undefined nonterminal: " ++) (undefinedNames uniqueRules)
        leftRecWarns =
            map ("Left recursion in: " ++) (leftRecursiveRuleNames uniqueRules)
        arityWarns =
            [ "Incorrect number of arguments when calling "
                ++ target
                ++ " (expected "
                ++ show expected
                ++ ", got "
                ++ show actual
                ++ ") in rule "
                ++ caller
            | (caller, target, expected, actual) <- parameterArityWarnings uniqueRules
            ]
    in dupWarnings ++ undefinedWarns ++ leftRecWarns ++ arityWarns

-- | Provided helper to format the current time for filenames.
getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getCurrentTime

-- ------------------------------
-- Helper parsers and utilities
-- ------------------------------

-- | Consume at least one inline (non-newline) whitespace character.
inlineSpaces1 :: Parser String
inlineSpaces1 = some inlineSpace

-- | Skip any number of blank lines (made of spaces followed by a newline).
skipBlankLines :: Parser ()
skipBlankLines = () <$ many blankLine
  where
    blankLine = inlineSpaces *> is '\n'

-- | Variant of 'sepBy' that requires at least one item.
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

-- | Parse zero or more occurrences of @p@ separated by @sep@.
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

-- | Parse an item enclosed by the given opening and closing characters.
betweenChars :: Char -> Char -> Parser a -> Parser a
betweenChars open close p = do
    _ <- is open
    result <- p
    _ <- is close
    pure result

-- | Specialisation of 'betweenChars' for parentheses.
betweenParens :: Parser a -> Parser a
betweenParens = betweenChars '(' ')'

-- | Parse a non-terminal identifier (lowercase letter followed by @[_a-zA-Z0-9]@).
identifier :: Parser String
identifier = do
    first <- lower
    rest <- many identChar
    pure (first : rest)
  where
    identChar = satisfy (\c -> isAlphaNum c || c == '_')

-- | Parse a single-letter parameter name used in rule definitions.
ruleParamName :: Parser String
ruleParamName = (: []) <$> lower

-- | Parse comma-separated occurrences of @p@ with optional surrounding spaces.
commaSeparated :: Parser a -> Parser [a]
commaSeparated p =
    sepBy p (inlineSpaces *> is ',' *> inlineSpaces)

-- ------------------------------
-- Term parsers
-- ------------------------------

-- | Parse a logical term, capturing any trailing modifiers.
pTerm :: Parser Term
pTerm = do
    base <- pTok <|> pBareTerm
    modifiers <- many pModifier
    pure (foldl wrap base modifiers)
  where
    wrap term modifier = TModified term modifier

-- | Parse an unmodified term.
pBareTerm :: Parser Term
pBareTerm =
    pParamUsage
        <|> pNonTerminal
        <|> pTerminal
        <|> pBracketTerm

-- | Parse a non-terminal reference, rejecting parameterised forms.
pNonTerminal :: Parser Term
pNonTerminal =
    angleTerm (pure [])
        >>= \(name, args) ->
            maybe (pure (TNonTerminal name)) (const empty) args

-- | Parse a quoted terminal string.
pTerminal :: Parser Term
pTerminal =
    TTerminal <$> betweenChars '"' '"' (many (satisfy (/= '"')))

-- | Parse either a macro or parameter reference inside square brackets.
pBracketTerm :: Parser Term
pBracketTerm = betweenChars '[' ']' (pMacro <|> pParamRef)

-- | Parse the built-in macro names.
pMacro :: Parser Term
pMacro =
    TMacro
        <$> (   string "int" *> pure MInt
            <|> string "alpha" *> pure MAlpha
            <|> string "newline" *> pure MNewline
            <|> string "space" *> pure MSpace
            )

-- | Parse a parameter reference such as @[a]@.
pParamRef :: Parser Term
pParamRef = TParamRef . (: []) <$> lower

-- | Parse a parameterised non-terminal usage, e.g. @\<pair([int], [int])\>@.
pParamUsage :: Parser Term
pParamUsage =
    angleTerm usageArgs
        >>= \(name, args) ->
            maybe empty (pure . TParamUsage name) args

-- | Parse a comma-separated list of terms within parentheses.
usageArgs :: Parser [Term]
usageArgs =
    inlineSpaces *> commaSeparated (inlineSpaces *> pTerm <* inlineSpaces)
        <* inlineSpaces

-- | Parse the @tok@ keyword followed by a bare term.
pTok :: Parser Term
pTok =
    TModified
        <$> (string "tok" *> inlineSpaces1 *> pBareTerm)
        <*> pure MTok

-- | Parse a single postfix modifier token and return its constructor.
pModifier :: Parser Modifier
pModifier =
    MStar <$ is '*'
        <|> MPlus <$ is '+'
        <|> MQuestion <$ is '?'

-- ------------------------------
-- Rule parsers
-- ------------------------------

-- | Parse a complete grammar rule including definition and trailing newline.
pRule :: Parser Rule
pRule =
    inlineSpaces
        *> (uncurry Rule <$> pRuleName)
            <*> (inlineSpaces *> string "::=" *> inlineSpaces *> pDefinition)
        <* inlineSpaces
        <* optional (is '\n')

-- | Parse the rule name plus any parameter list inside @\<...>@.
pRuleName :: Parser (String, [String])
pRuleName = do
    (name, args) <- angleTerm ruleParamsParser
    let params = maybe [] id args
    pure (name, params)
  where
    ruleParamsParser =
        inlineSpaces *> commaSeparated (inlineSpaces *> ruleParamName <* inlineSpaces)
            <* inlineSpaces

-- | Parse the right-hand side of a rule as one-or-more alternatives.
pDefinition :: Parser Definition
pDefinition =
    sepBy1 pAlternative pipeSeparator

-- | Parse a single alternative comprising one-or-more terms.
pAlternative :: Parser Alternative
pAlternative =
    (:) <$> pTerm <*> many (inlineSpaces1 *> pTerm) <* inlineSpaces

-- | Parse the @|@ separator, which may optionally appear on a new line.
pipeSeparator :: Parser ()
pipeSeparator =
    simpleSep <|> newlineSep
  where
    simpleSep = inlineSpaces *> is '|' *> inlineSpaces *> pure ()
    newlineSep =
        inlineSpaces *> is '\n' *> inlineSpaces *> is '|' *> inlineSpaces *> pure ()

-- | Parse an angle-bracketed name with optional arguments.
angleTerm :: Parser a -> Parser (String, Maybe a)
angleTerm argsParser = do
    _ <- is '<'
    name <- identifier
    args <- optional $ do
        _ <- inlineSpaces
        betweenParens argsParser
    _ <- inlineSpaces
    _ <- is '>'
    pure (name, args)

-- ------------------------------
-- Code generation helpers
-- ------------------------------

-- | Produce the Haskell @data@/@newtype@ declaration corresponding to a rule.
renderDataDecl :: Rule -> String
renderDataDecl (Rule name params defn) =
    case defn of
        [[term]] ->
            let typeName = toTypeName name
                header = typeHeader typeName params
                fieldType = formatTypeField (termType term)
            in "newtype " ++ header ++ " = " ++ typeName ++ " " ++ fieldType ++ "\n    deriving Show"
        _ ->
            let typeName = toTypeName name
                header = typeHeader typeName params
                constructors =
                    zipWith (renderConstructor typeName) [1 ..] defn
                firstLine =
                    case constructors of
                        [] -> "data " ++ header
                        (c : cs) ->
                            let intro = "data " ++ header ++ " = " ++ c
                                indent = replicate (length ("data " ++ header) + 1) ' '
                                rest = map (\con -> indent ++ "| " ++ con) cs
                            in intercalate "\n" (intro : rest)
            in firstLine ++ "\n    deriving Show"
  where
    renderConstructor :: String -> Int -> Alternative -> String
    renderConstructor typeName idx terms =
        let constructorName = typeName ++ show idx
            fields = map (formatTypeField . termType) terms
        in unwords (constructorName : fields)

-- | Render the parser function for a given rule.
-- This function translates a single BNF Rule into a Haskell parser function string.
renderParserDecl :: Rule -> String
renderParserDecl rule@(Rule name params defn) =
    let -- <my_rule> -> "pMyRule"
        funcName = toParserName name
        -- <my_rule> -> "MyRule"
        typeName = toTypeName name
        
        -- Build the type signature, e.g., "pMyRule :: Parser MyRule"
        -- or "pPair :: Parser a -> Parser b -> Parser (Pair a b)"
        signature = funcName ++ " :: " ++ renderParserType typeName params
        
        -- Get the argument names for the function, e.g., " a b"
        argsPart = if null params then "" else " " ++ unwords params
        
        -- The left-hand side of the '=', e.g., "pMyRule" or "pPair a b"
        lhs = funcName ++ argsPart
        
        -- Indentation for the '<|>' lines, to match the '='
        indent = replicate (length lhs + 1) ' '
        
        -- Build the list of parser "alternatives" as strings.
        -- Pairs each alternative with its index (1, 2, 3...)
        ordered =
            zipWith (renderAlt rule) [1 ..] defn
            
        -- Assemble the function body (the right-hand side of the '=')
        body =
            case ordered of
                -- No alternatives? This parser should always fail.
                [] -> lhs ++ " = empty"
                
                -- One or more alternatives.
                (a : rest) -> -- 'a' is the first, 'rest' are the others.
                    let -- The first line, e.g., "pMyRule = (MyRule1 <$> string "a")"
                        firstLine = lhs ++ " = " ++ a
                        -- All other lines, e.g., " <|> (MyRule2 <$> int)"
                        others = map (\altTxt -> indent ++ "<|> " ++ altTxt) rest
                    -- Join them all with newlines.
                    in intercalate "\n" (firstLine : others)
                    
    -- Put the signature and the body together.
    in signature ++ "\n" ++ body
  where
    -- | Helper to render a *single* alternative (one side of a '<|>')
    -- e.g., "MyRule1 <$> string "a" <*> int"
    renderAlt :: Rule -> Int -> Alternative -> String
    renderAlt (Rule rName _ _) idx terms =
        -- Get the correct data constructor name (e.g., "MyRule" or "MyRule1")
        let constructorName = ctorName rName defn idx
        in case terms of
            -- This alternative is empty (e.g., <rule> ::= ).
            -- It parses nothing and just "purely" returns the constructor.
            [] -> "pure " ++ constructorName
            
            -- This alternative has one or more terms.
            (t : ts) ->
                -- This is the Applicative parser style.
                -- "MyRule1 <$> (parser for t)"
                constructorName ++ " <$> " ++ termParser t
                    -- For all the rest, chain them with '<*>'
                    -- " <*> (parser for ts[0]) <*> (parser for ts[1]) ..."
                    ++ concatMap (\term -> " <*> " ++ termParser term) ts

    -- | Helper to build the type signature string for the parser function.
    renderParserType :: String -> [String] -> String
    renderParserType typeName typeParams =
        let -- The final *result* type, e.g., "MyRule" or "(MyRule a b)"
            resultType =
                if null typeParams
                    then typeName
                    else typeName ++ " " ++ unwords typeParams
            -- The parser that returns that result, e.g., "Parser (MyRule a b)"
            result =
                if null typeParams
                    then "Parser " ++ resultType
                    else "Parser (" ++ resultType ++ ")"
        -- Chain all parameter types and the result type with " -> "
        -- e.g., ["Parser a", "Parser b", "Parser (MyRule a b)"]
        --   -> "Parser a -> Parser b -> Parser (MyRule a b)"
        in intercalate " -> " (map (\p -> "Parser " ++ p) typeParams ++ [result])

    -- | Helper to get the *correct* data constructor name.
    ctorName :: String -> Definition -> Int -> String
    ctorName rName definition idx
        -- If it's a newtype, the constructor name is the *same* as the type name.
        -- e.g., newtype MyRule = MyRule Other
        | isNewtypeRule definition = toTypeName rName
        -- If it's a data type, the constructor name has an index.
        -- e.g., data MyRule = MyRule1 ...
        | otherwise = toTypeName rName ++ show idx

-- | Check the shape of a definition to decide between newtype/data.
isNewtypeRule :: Definition -> Bool
isNewtypeRule [[_]] = True
isNewtypeRule _ = False

-- | Translate a term into its corresponding Haskell type.
termType :: Term -> String
termType term = case term of
    TNonTerminal name -> toTypeName name
    TTerminal _ -> "String"
    TMacro macro -> case macro of
        MInt -> "Int"
        MAlpha -> "String"
        MNewline -> "Char"
        MSpace -> "Char"
    TParamRef x -> x
    TParamUsage name args ->
        let argTypes = map termType args
        in unwords (toTypeName name : argTypes)
    TModified inner modifier -> case modifier of
        MTok -> termType inner
        MStar -> "[" ++ termType inner ++ "]"
        MPlus -> "[" ++ termType inner ++ "]"
        MQuestion -> "Maybe " ++ termType inner

-- | Translate a grammar term into the applicative parser expression that parses it.
termParser :: Term -> String
termParser term = case term of
    TNonTerminal name -> toParserName name
    TTerminal s -> "(string " ++ show s ++ ")"
    TMacro macro -> case macro of
        MInt -> "int"
        MAlpha -> "(some alpha)"
        MNewline -> "(is '\\n')"
        MSpace -> "(is ' ')"
    TParamRef x -> x
    TParamUsage name args ->
        "(" ++ unwords (toParserName name : map termParser args) ++ ")"
    TModified inner modifier -> case modifier of
        MTok -> tokExpr inner
        MStar -> "(many " ++ termParser inner ++ ")"
        MPlus -> "(some " ++ termParser inner ++ ")"
        MQuestion -> "(optional " ++ termParser inner ++ ")"
  where
    -- | Translate the tok-modifier using the supplied helper parsers.
    tokExpr :: Term -> String
    tokExpr inner = case inner of
        TTerminal s -> "(stringTok " ++ show s ++ ")"
        _ -> "(tok " ++ termParser inner ++ ")"

-- | Ensure field types are parenthesised where needed (e.g. tuples or applications).
formatTypeField :: String -> String
formatTypeField t
    | needsParens t = "(" ++ stripParens t ++ ")"
    | otherwise = t
  where
    needsParens :: String -> Bool
    needsParens [] = False
    needsParens s
        | isWrapped '(' ')' s = False
        | isWrapped '[' ']' s = False
        | otherwise = any (== ' ') s

    stripParens :: String -> String
    stripParens s =
        case s of
            '(' : rest ->
                case unsnoc rest of
                    Just (inner, ')') -> inner
                    _ -> s
            _ -> s

    isWrapped :: Char -> Char -> String -> Bool
    isWrapped open close xs =
        case xs of
            y : ys | y == open -> endsWith close ys
            _ -> False

    endsWith :: Char -> String -> Bool
    endsWith _ [] = False
    endsWith c [x] = x == c
    endsWith c (_ : xs) = endsWith c xs

-- | Build the textual head of a data/newtype declaration, including parameters.
typeHeader :: String -> [String] -> String
typeHeader typeName params =
    typeName ++ if null params then "" else " " ++ unwords params

-- | Normalise a BNF rule name into a Haskell data constructor name.
toTypeName :: String -> String
toTypeName = concatMap capitalize . splitOn '_'

-- | Normalise a BNF rule name into a Haskell parser function name.
toParserName :: String -> String
toParserName str =
    case splitOn '_' str of
        [] -> ""
        (x : xs) -> lowercaseFirst x ++ concatMap capitalize xs

-- | Split a string on a specific delimiter character.
splitOn :: Char -> String -> [String]
splitOn delim = go ""
  where
    go acc [] = [reverse acc]
    go acc (c : cs)
        | c == delim = reverse acc : go "" cs
        | otherwise = go (c : acc) cs

-- | Capitalise the first character in a string.
capitalize :: String -> String
capitalize [] = []
capitalize (c : cs) = toUpper c : cs

-- | Lowercase the first character in a string.
lowercaseFirst :: String -> String
lowercaseFirst [] = []
lowercaseFirst (c : cs) = toLower c : cs

-- ------------------------------
-- Validation and filtering
-- ------------------------------

-- | Keep only the first definition for each rule name, preserving order.
keepFirstRules :: ADT -> ADT
keepFirstRules =
    reverse . snd . foldl step ([], [])
  where
    step (seen, acc) r
        | name `elem` seen = (seen, acc)
        | otherwise = (name : seen, r : acc)
      where
        name = ruleName r

-- | Report user-facing warnings about duplicate rule declarations.
duplicateWarnings :: ADT -> [String]
duplicateWarnings rules =
    let names = map ruleName rules
        duplicates =
            sort
                [ name
                | name <- nub names
                , countOcc name names > 1
                ]
    in map ("Duplicate rule: " ++) duplicates
  where
        countOcc x = length . filter (== x)

-- | Names of nonterminals that are referenced but never defined.
undefinedNames :: ADT -> [String]
undefinedNames rules =
    let defined = map ruleName rules
        used = nub (concatMap ruleReferences rules)
    in sort [name | name <- used, name `notElem` defined]

-- | Identify rules that reference undefined nonterminals.
rulesWithUndefinedRefs :: ADT -> [String]
rulesWithUndefinedRefs rules =
    nub
        [ ruleName rule
        | rule <- rules
        , any (`notElem` defined) (ruleReferences rule)
        ]
  where
    defined = map ruleName rules

-- | Names of parameterless rules that form a left-recursive cycle.
-- Left-recursion (e.g., <rule> ::= <rule> "a") causes infinite loops in parsers.
-- This function finds all rules that are part of such a cycle.
leftRecursiveRuleNames :: ADT -> [String]
leftRecursiveRuleNames rules =
    -- First, we only check rules that don't have parameters (e.g., <rule>, not <rule(a)>).
    -- Parameterized rules behave differently and aren't part of this check.
    let nonParamRules = filter (null . ruleParams) rules
        
        -- Get a simple list of just the names of these rules, for quick lookups.
        nonParamNames = map ruleName nonParamRules

        -- This is the core logic: build a "left-most dependency graph" (adjacency list).
        -- This graph map shows, for each rule, which *other* rules can appear
        -- as the VERY FIRST non-terminal in one of its alternatives.
        -- Example: For <a_rule> ::= <b_rule> "x" | <c_rule>, the entry would be ("a_rule", ["b_rule", "c_rule"]).
        -- Example: For <safe> ::= "a" <safe>, the entry would be ("safe", []).
        adjacency =
            [ -- For each rule 'r', create a tuple: (RuleName, [ListOfFirstDependencies])
              ( ruleName r
              , -- To get the list of dependencies:
                -- 1. Get the definition (list of alternatives) for rule 'r'.
                -- 2. For each alternative, find its *first* non-terminal (e.g., <THIS_ONE> ...).
                --    This is what `mapMaybe firstNonterminalInAlt` does. It ignores alternatives
                --    that start with terminals (like "a").
                -- 3. Filter this list to *only* include other non-parameterized rules.
                filter (`elem` nonParamNames)
                    (mapMaybe firstNonterminalInAlt (ruleDefinition r))
              )
            -- Do this for every non-parameterized rule.
            | r <- nonParamRules
            ]

        -- Now that we have the graph, find all rules that have a cycle.
        recursive =
            [ -- For each non-parameterized rule...
              name
            | r <- nonParamRules
            , let name = ruleName r
            -- ...run the cycle-detection search (`hasLeftRec`) starting from that rule.
            -- If it returns True, the rule is recursive, so keep it in this list.
            , hasLeftRec adjacency name
            ]
            
    -- Finally, remove any duplicates from the list of recursive rules and sort it for a clean output.
    in sort (nub recursive)
  where
    -- | Depth-first search (DFS) to find a cycle in the 'adj' graph.
    -- It checks if we can get from 'start' back to 'start' by following the graph's edges.
    hasLeftRec adj start = go [] start
      where
        -- 'go' is the recursive worker for the DFS.
        -- 'visited' is the path we've taken so far (to avoid getting stuck in other, unrelated cycles).
        -- 'current' is the node (rule name) we are currently looking at.
        go visited current =
            -- Look up the dependencies (targets) for the 'current' node in the graph.
            case lookup current adj of
                -- This rule has no left-most dependencies (e.g., <rule> ::= "a"). It's a dead end.
                -- No cycle can be formed from here.
                Nothing -> False
                
                -- This rule has dependencies (e.g., 'targets' is a list like ["b_rule", "c_rule"])
                Just targets ->
                    -- Check `any` of the targets. Does *any* target lead back to the 'start' node?
                    any
                        ( \target ->
                            -- Case 1: The target *is* the start node!
                            -- (e.g., we are checking "a" and find a dependency "a" -> "a")
                            -- We found a direct cycle. Return True.
                            target == start
                            
                            -- Case 2: The target is not the start node.
                            -- We need to search deeper, *if* we haven't visited this target already
                            -- on this *current path* (to prevent looping in the search itself).
                                || (target `notElem` visited && go (target : visited) target)
                        )
                        targets

-- | Iteratively drop rules that cannot be compiled (undefined refs, left recursion, wrong arity).
filterInvalidRules :: ADT -> ADT
filterInvalidRules = go
  where
    go current =
        let bad =
                nub
                    ( rulesWithUndefinedRefs current
                        ++ leftRecursiveRuleNames current
                        ++ rulesWithWrongArity current
                    )
        in if null bad
            then current
            else
                let next = filter (\r -> ruleName r `notElem` bad) current
                in if length next == length current
                    then current
                    else go next

-- | Gather the nonterminal references used within a rule.
ruleReferences :: Rule -> [String]
ruleReferences = definitionReferences . ruleDefinition

-- | Gather unique nonterminal references within a definition.
definitionReferences :: Definition -> [String]
definitionReferences = nub . concatMap alternativeReferences

-- | Gather unique nonterminal references within an alternative.
alternativeReferences :: Alternative -> [String]
alternativeReferences = nub . concatMap termReferences

-- | Gather the nonterminal references within a term (including nested parameter usage).
termReferences :: Term -> [String]
termReferences term = case term of
    TNonTerminal name -> [name]
    TParamUsage name args -> nub (name : concatMap termReferences args)
    TModified inner _ -> termReferences inner
    _ -> []

-- | Flag parameterised calls where the supplied arguments do not match the rule arity.
parameterArityWarnings :: ADT -> [(String, String, Int, Int)]
parameterArityWarnings rules =
    let paramCounts =
            [(ruleName r, length (ruleParams r)) | r <- rules]
        raw =
            [ (caller, target, expected, actual)
            | rule <- rules
            , let caller = ruleName rule
            , (target, actual) <- ruleParamUsages rule
            , Just expected <- [lookup target paramCounts]
            , expected /= actual
            ]
    in sort (nub raw)

-- | Rules that contain at least one incorrect parameter arity usage.
rulesWithWrongArity :: ADT -> [String]
rulesWithWrongArity =
    nub . map (\(caller, _, _, _) -> caller) . parameterArityWarnings

-- | Unique parameter usages appearing in a rule definition.
ruleParamUsages :: Rule -> [(String, Int)]
ruleParamUsages =
    nub . definitionParamUsages . ruleDefinition

-- | Parameter usages appearing across all alternatives in a definition.
definitionParamUsages :: Definition -> [(String, Int)]
definitionParamUsages = nub . concatMap alternativeParamUsages

-- | Parameter usages appearing within a single alternative.
alternativeParamUsages :: Alternative -> [(String, Int)]
alternativeParamUsages = nub . concatMap termParamUsages

-- | Parameter usages appearing within a term (including nested parameters).
termParamUsages :: Term -> [(String, Int)]
termParamUsages term = case term of
    TParamUsage name args ->
        nub ((name, length args) : concatMap termParamUsages args)
    TModified inner _ -> termParamUsages inner
    _ -> []

-- | Extract the first nonterminal reference from an alternative, if present.
firstNonterminalInAlt :: Alternative -> Maybe String
firstNonterminalInAlt [] = Nothing
firstNonterminalInAlt (t : _) = firstNonterminalInTerm t

-- | Extract the first nonterminal reference from a term, if present.
firstNonterminalInTerm :: Term -> Maybe String
firstNonterminalInTerm term = case term of
    TNonTerminal name -> Just name
    TParamUsage name _ -> Just name
    TModified inner _ -> firstNonterminalInTerm inner
    _ -> Nothing

-- | Fetch the name of the first rule, if available.
firstRuleName :: ADT -> Maybe String
firstRuleName = fmap ruleName . listToMaybe
