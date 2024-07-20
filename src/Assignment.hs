{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- Please do not change the names of the parseExerciseX functions, as they
-- are used by the test suite.

module Assignment where
import Control.Applicative (Alternative (..), optional)

import Instances
import Parser
import Prelude
import GHC.Base
import Text.Read (Lexeme(String))
import Data.List (intercalate)
import Debug.Trace
import Control.Monad

------------------------------------------- ALGEBRAIC DATA TYPE DECLARATION -----------------------------------------------
data ADT =
  Plus ADT ADT
  | Minus ADT ADT
  | Times ADT ADT
  | Divide ADT ADT
  | Number Int
  | Declaration String
  | TrueExpr
  | FalseExpr
  | Not ADT
  | And ADT ADT
  | Or ADT ADT
  | Equals ADT ADT
  | NotEquals ADT ADT
  | GreaterThan ADT ADT
  | LessThan ADT ADT
  | StringLiteral String
  | List [ADT]
  | Ternary ADT ADT ADT
  | EmptyString String
  | Empty
  | FunctionDeclaration String [ADT] [ADT]
  | ReturnStatement ADT
  | FunctionCall String [ADT]
  | CodeBlock [ADT]
  | ConstDeclaration String ADT
  | Declarations [ADT]  -- new ADT for 'const' declarations
  | IfStatement ADT ADT (Maybe ADT) -- condition, thenBlock, elseBlock
  | ManualCodeBlock [ADT]
  | WholeBlock [ADT]
  deriving (Show, Eq)
------------------------------------------------------------------------------------------------------------


------------------------------------------------PRETTYFYING FUNCTOINS --------------------------------------
indent :: String -> String
indent = unlines . map ("\t" ++) . lines

adtToString :: ADT -> String
adtToString (Number n)     = show n
adtToString (Plus l r)     = "(" ++ adtToString l ++ " + " ++ adtToString r ++ ")"
adtToString (Minus l r)    = "(" ++ adtToString l ++ " - " ++ adtToString r ++ ")"
adtToString (Times l r)    = "(" ++ adtToString l ++ " * " ++ adtToString r ++ ")"
adtToString (Divide l r)    = "(" ++ adtToString l ++ " / " ++ adtToString r ++ ")"
adtToString TrueExpr       = "true"
adtToString FalseExpr      = "false"
adtToString (Not expr)     = "(!" ++ adtToString expr ++ ")"
adtToString (And l r)      = "(" ++ adtToString l ++ " && " ++ adtToString r ++ ")"
adtToString (Or l r)       = "(" ++ adtToString l ++ " || " ++ adtToString r ++ ")"
adtToString (Equals l r)     = "(" ++ adtToString l ++ " === " ++ adtToString r ++ ")"
adtToString (NotEquals l r)  = "(" ++ adtToString l ++ " !== " ++ adtToString r ++ ")"
adtToString (GreaterThan l r) = "(" ++ adtToString l ++ " > " ++ adtToString r ++ ")"
adtToString (LessThan l r)  = "(" ++ adtToString l ++ " < " ++ adtToString r ++ ")"
adtToString (StringLiteral s)  = "\"" ++ s ++ "\""
adtToString (EmptyString _)  = "{ }"
adtToString (ManualCodeBlock statements) =
    let
        indentedStatements = map ("  " ++) (lines $ concatMap adtToString statements)
    in
        if null indentedStatements
        then "{ }"
        else "{\n" ++ unlines indentedStatements ++ "}"
adtToString (FunctionCall name args) = name ++ "(" ++ intercalate ", " (map adtToString args) ++ ")"
adtToString (List items) = "[" ++ Data.List.intercalate ", " (map adtToString items) ++ "]"
adtToString (Ternary condition thenCase elseCase) = "(" ++ adtToString condition ++ " ? " ++ adtToString thenCase ++ " : " ++ adtToString elseCase ++ ")"
adtToString (ConstDeclaration varName value) = "const " ++ varName ++ " = " ++ adtToString value ++ ";"
adtToString (Declarations decls) = unlines (map adtToString decls)
adtToString (Declaration s) = s
adtToString (CodeBlock statements) =  "{\n" ++ indent (concatMap adtToString statements) ++ "}\n"
adtToString (WholeBlock statements) =   concatMap adtToString statements
adtToString (ReturnStatement exprInside) = "return (" ++ adtToString exprInside ++ ");"
adtToString (FunctionDeclaration name params body) =
    "function " ++ name ++ "(" ++ intercalate ", " (map adtToString params) ++ ") {\n" ++ indent (concatMap adtToString body) ++ "}"

adtToString (IfStatement condition thenBlock maybeElseBlock) =
    let
        singleLineRep =
            "if ( " ++ adtToString condition ++ " ) " ++
            adtToString thenBlock ++
            maybe "" (\elseBlock -> "else " ++ adtToString elseBlock) maybeElseBlock

        multiLineRep =
            "if ( " ++ adtToString condition ++ " ) " ++
            adtToString thenBlock ++ -- keep the ending of the thenBlock as is
            maybe "" (\elseBlock -> "else " ++ adtToString elseBlock) maybeElseBlock
            -- removed the newline before "else"

    in
        if length singleLineRep > 45 then multiLineRep else singleLineRep
adtToString _          = "e"

--------------------------------------------------------------------------------------------------------------------


--------------------------------------GENERAL PURPOSE PARSERS --------------------------------------------------

-- Compute the length of the string, excluding newline characters
lengthWithoutNewlines :: String -> Int
lengthWithoutNewlines = length . filter (/= '\n')

-- Determines if a given ADT block should be pretty-printed over multiple lines
-- based on its length.
shouldUseMultiline :: ADT -> Bool
shouldUseMultiline block =
    lengthWithoutNewlines (adtToString block) > 45

-- List of JavaScript reserved words.
reservedWords :: [String]
reservedWords = ["const", "if", "else", "function", "return", "true", "false"]

-- Parser for JavaScript variable names, ensuring they aren't reserved words.
varName :: Parser String
varName = do
    name <- liftA2 (:) firstChar followingChars
    guard (name `notElem` reservedWords)
    return name
  where
    firstChar = alpha <|> is '_' <|> is '$'  -- First character can be a letter, underscore, or dollar sign.
    followingChars = many (alpha <|> digit <|> is '_' <|> is '$') -- Following chars can also include digits.

-- Attempts to parse with the given parser but returns Nothing if it fails.
optionalParser :: Parser a -> Parser (Maybe a)
optionalParser = optional


-- Parser for simple addable expressions like numbers, function calls, variable names, 
-- and expressions within brackets.
addable :: Parser ADT
addable =
    number
    <|> functionCall
    <|> Declaration <$> varName
    <|> bracketed returnExpr  -- allows for more complex expressions within parentheses


-- Wraps a given parser with spaces on both sides.
spacesOp :: Parser (a -> a -> a) -> Parser (a -> a -> a)
spacesOp op = do
    spaces
    operation <- op
    spaces
    return operation

-- Looks ahead in the input and tries to parse with the given parser 
-- without consuming any input.
lookAhead :: Parser a -> Parser a
lookAhead (Parser p) = Parser $ \input ->
    case p input of
        Result _ a -> Result input a
        Error err -> Error err

-- Parser for function names, using the variable name parser.
funcName :: Parser String
funcName = varName

-- Parser for code blocks enclosed in braces.
block :: Parser ADT
block = do
    _ <- charTok '{'
    statements <- many (singleExpr <* spaces)
    _ <- charTok '}'
    return $ CodeBlock statements

-- Parser to consume and ignore white spaces.
parseWhitespace :: Parser ADT
parseWhitespace = EmptyString <$> many (is ' ')


-- Parser for 'const' variable declarations in JavaScript.
constDeclaration :: Parser ADT
constDeclaration = do
    _ <- stringTok "const"  -- Using 'stringTok' to handle spaces after the keyword
    var <- varName          -- Parse variable name
    spaces
    _ <- is '='
    spaces
    value <- expr           -- Parse the value of the declaration
    spaces
    _ <- is ';'
    spaces
    return $ ConstDeclaration var value

-- Parser for multiple 'const' variable declarations.
constDeclarations :: Parser ADT
constDeclarations = Declarations <$> constDeclaration `sepBy1` spaces


-- Parser for code blocks with diagnostic traces for debugging.
codeBlock :: Parser ADT
codeBlock = do
    trace "Entering codeBlock parser" $ return ()
    spaces
    _ <- charTok '{'
    trace "Parsed opening brace" $ return ()
    statements <- expr `sepBy` someSpaces   -- Parse all statements inside the block
    trace "Parsed statements inside block" $ return ()
    _ <- charTok '}'
    trace "Parsed closing brace" $ return ()
    return $ CodeBlock statements

-- Parser for manually parsed code blocks enclosed in braces.
manualCodeBlock :: Parser ADT
manualCodeBlock = do
    spaces
    _ <- charTok '{'
    statements <- expr `sepBy` someSpaces   -- Parse all statements inside the block
    _ <- charTok '}'
    return $ ManualCodeBlock statements

-- Parser to consume and expect at least one space.
someSpaces :: Parser ()
someSpaces = do
    trace "Trying to match some spaces" $ return ()
    void (some space)
    trace "Matched some spaces" $ return ()

-- Helper to create a parser that processes a chain of operations.
chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain p op = foldl applyOp <$> p <*> many (liftA2 (,) op p)
  where
    applyOp :: a -> (a->a->a, a) -> a
    applyOp x (op, y) = op x y

-- Wraps a parser to expect the parsed content to be within brackets.
bracketed :: Parser a -> Parser a
bracketed p = do
    spaces
    _ <- is '('
    spaces
    result <- p
    spaces
    _ <- is ')'
    spaces
    return result

-- Trace parser that logs the remaining input.
debugWithRemaining :: String -> Parser ()
debugWithRemaining s = Parser $ \input -> trace (s ++ " Remaining: " ++ input) (Result input ())

-- Parser that tries to look ahead for a function name.
lookAheadFunctionName :: Parser String
lookAheadFunctionName = do
    name <- varName
    _ <- lookAhead (is '(')
    return name

-- Term parser for mathematical expressions with higher precedence.
term :: Parser ADT
term = chain factor times

-- Wrapper around a parser that returns the initial state in case of an error.
tryIt :: Parser a -> Parser a
tryIt (Parser p) = Parser $ \input ->
    case p input of
        Error _ -> Error UnexpectedEof  -- reverting to the initial state
        success -> success


-- Creates a parser that expects one or more of the parsed elements separated by another parser.
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (p `sepBy1` sep) <|> pure []

-- Creates a parser that expects one or more of the parsed elements separated by another parser.
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = liftA2 (:) p (many (spaces >> sep >> spaces >> p))



-- Extracts return statements from a function body, including those from nested blocks.
opStr :: String -> Parser String
opStr s = do
    spaces
    string s
    spaces
    return s

-- Helper parsers to consume specific strings surrounded by spaces.
opStr2 :: String -> Parser String
opStr2 s = do
    spaces
    string s
    spaces
    return s

-- This function is used to extract all return statements from a function body, including nested ones.
extractReturnStatements :: [ADT] -> [ADT]
extractReturnStatements [] = []
extractReturnStatements (x:xs) = case x of
    -- If we find a block, we extract the return statements from its inner content and move on to the rest of the list.
    WholeBlock innerBody -> extractReturnStatements innerBody ++ extractReturnStatements xs
    -- If we find a return statement, we keep it and move on to the rest of the list.
    ReturnStatement _ -> x : extractReturnStatements xs
    -- For all other types of statements, we simply move on to the rest of the list.
    _ -> extractReturnStatements xs


-- A parser for operators with low precedence in logic expressions.
logicLowest :: Parser ADT
logicLowest = chain (chain factor andExpr) orExpr

-----------------------------------------------------------------------------------------------------


---------------------------------------- FUNCTION PARSERS -----------------------------------------

-- The `functionBlock` parser handles the parsing of JavaScript function declarations.
functionBlock :: Parser ADT
functionBlock = do
    _ <- stringTok "function"              -- Expecting the "function" keyword
    name <- varName                        -- Parsing the function name
    params <- bracketed (expr `sepBy` charTok ',') -- Parsing function parameters enclosed in brackets and separated by commas
    body <- block                          -- Parsing the function body
    let bodyStatements = case body of      -- Extracting statements from the function body
                            CodeBlock stmts -> stmts
                            singleStmt      -> [singleStmt]
    followingStatements <- many (spaces >> singleExpr) -- Parsing any statements following the function declaration
    let funcDecl = FunctionDeclaration name params bodyStatements
    return $ case followingStatements of   -- Returning either just the function declaration or a block including subsequent statements
        [] -> funcDecl
        _  -> CodeBlock (funcDecl : followingStatements)

-- `functionArgument` parses a single argument of a function call. It can be a term, a function call,
-- a number, or a logic expression.
functionArgument :: Parser ADT
functionArgument =
    chain term addop
    <|> functionCall
    <|> number
    <|> logicExpr


-- `funcArgs` is a parser for the arguments list of a function call.
-- It expects the arguments to be enclosed in parentheses and separated by commas.
funcArgs :: Parser [ADT]
funcArgs = bracketed (expr `sepBy` charTok ',')


-- `functionCall` parses function calls with space-allowed before arguments.
-- It first looks ahead to ensure it's a function call by checking the function name and the opening bracket.
-- Then it extracts the function name and its arguments.
functionCall :: Parser ADT
functionCall = do
    _ <- lookAhead $ do
        _ <- funcName
        _ <- is '('
        return ()
    name <- funcName
    args <- funcArgs
    spaces
    return $ FunctionCall name args

-- `functionCallNoSpace` is a variant of the above parser, but it doesn't expect spaces between the function name and its arguments.
functionCallNoSpace :: Parser ADT
functionCallNoSpace = do
    name <- funcName
    FunctionCall name <$> funcArgs

-----------------------------------------------------------------------------------------------------

------------------------------ IF STATEMENT PARSERS -----------------------------------------------

-- The `ifStatement` parser handles the parsing of JavaScript "if-else" statements.
-- It expects the keyword "if", a condition within parentheses, and then a block of code.
-- An optional "else" block can also be provided. 
ifStatement :: Parser ADT
ifStatement = do
    _ <- stringTok "if"
    condition <- bracketed expr            -- Parse the condition inside brackets
    thenBlock <- block                     -- Parse the "then" block
    elseBlock <- optionalParser (stringTok "else" >> block) -- Optionally parse the "else" block
    followingStatements <- many (spaces >> singleExpr) -- Capture statements after the else or if block
    let ifStmt = IfStatement condition thenBlock elseBlock
    case followingStatements of            -- Return the if statement, or if there are following statements, combine them into a WholeBlock
        [] -> return ifStmt
        stmts -> return $ WholeBlock (ifStmt : stmts)

-- `returnExpr` represents a variety of expressions that can be part of a return statement. 
returnExpr :: Parser ADT
returnExpr =
    chain addable (spacesOp addop)         -- Chain 'addable' elements with addition operator
    <|> functionCall
    <|> chain (chain factor (spacesOp andExpr)) (spacesOp orExpr) -- Handle logical expressions
    <|> number
    <|> logicExpr
    <|> arithExpr

-- `returnStatement` is a parser for JavaScript's "return" statements. 
-- It looks for the "return" keyword, then an expression, and ends with a semicolon.
returnStatement :: Parser ADT
returnStatement = do
    _ <- stringTok "return"
    exprInside <- returnExpr               -- Parse the expression inside the return
    spaces                                -- Ensure spaces are skipped after the expression
    _ <- charTok ';'
    return $ ReturnStatement exprInside   -- Return the parsed statement

-- `ternaryExpr` is a parser for JavaScript's ternary expressions.
-- It expects a condition, then an expression for the "true" branch, and finally one for the "false" branch.
ternaryExpr :: Parser ADT
ternaryExpr = do
    _ <- is '('
    spaces
    condition <- expr <|> bracketed expr  -- Try to parse condition with brackets, then without
    spaces
    _ <- is '?'
    spaces
    trueExpr <- expr <|> bracketed expr   -- Parse the true branch
    spaces
    _ <- is ':'
    spaces
    falseExpr <- expr <|> bracketed expr  -- Parse the false branch
    spaces
    _ <- is ')'
    return $ Ternary condition trueExpr falseExpr


-----------------------------------------------------------------------------------------------------


------------------------------------------ DATATYPE PARSERS ----------------------------------------

-- `number` parses numerical literals. It expects a sequence of digits possibly surrounded by spaces.
number :: Parser ADT
number = do
    spaces
    digits <- some digit
    spaces
    return $ Number (read digits)   -- Convert the sequence of digits to a Number ADT

-- `stringLiteral` parses string literals, which are sequences of characters surrounded by double quotes.
stringLiteral :: Parser ADT
stringLiteral = do
    spaces
    _ <- is '\"'
    content <- many (noneof "\"")  -- Grab every character until another double quote is encountered
    _ <- is '\"'
    spaces
    return $ StringLiteral content

-- `op` is a general-purpose operator parser that tries to match a given character (used for single character operators).
op :: Char -> Parser Char
op c = do
   spaces
   is c
   spaces
   pure c

-- `times` tries to parse either the multiplication '*' or division '/' operators.
times :: Parser (ADT -> ADT -> ADT)
times = (op '*' >> pure Times) <|> (op '/' >> pure Divide)

-- `addop` tries to parse either the addition '+' or subtraction '-' operators.
addop :: Parser (ADT -> ADT -> ADT)
addop = (op '+' >> pure Plus) <|> (op '-' >> pure Minus)

-- `arithExpr` is a parser for arithmetic expressions, which are a sequence of terms separated by addition or subtraction operators.
-- It expects the expression to be enclosed in parentheses.
arithExpr :: Parser ADT
arithExpr = bracketed (chain term addop)

-- `list` parses JavaScript array literals. It expects a sequence of values separated by commas and enclosed in square brackets.
list :: Parser ADT
list = do
    _ <- is '['
    spaces
    values <- listItem `sepBy` charTok ','  -- Parses a list of items separated by commas
    spaces
    _ <- is ']'
    return $ List values

-- `listItem` defines what can be inside a list (array). It can be a number, boolean values, or string literals.
listItem :: Parser ADT
listItem = number <|> trueExpr <|> falseExpr <|> stringLiteral

----------------------------------------------------------------------------------------------------


------------------------------------- LOGIC EXPRESSION PARSERS --------------------------------------


-- `combinedLogicAndComparison` attempts to parse a comparison expression first. If it fails, it tries to parse a logic expression.
combinedLogicAndComparison :: Parser ADT
combinedLogicAndComparison =
    tryIt comparison  -- Try to parse comparison first 
    <|> logicExpr     -- If comparison fails, try logic expression

-- `logicExpr` parses logical expressions. These can be complex expressions involving logical operators like `&&` (and) and `||` (or).
-- It expects the logical expression to be enclosed in parentheses.
logicExpr :: Parser ADT
logicExpr = bracketed (chain (chain combinedLogicAndComparison andExpr) orExpr)

-- `trueExpr` is a parser for the boolean `true` value.
trueExpr :: Parser ADT
trueExpr = spaces >> string "true" >> pure TrueExpr  -- Match the string "true" and return TrueExpr ADT

-- `falseExpr` is a parser for the boolean `false` value.
falseExpr :: Parser ADT
falseExpr = spaces >> string "false" >> pure FalseExpr  -- Match the string "false" and return FalseExpr ADT

-- `notExpr` parses logical NOT expressions. It expects the `!` operator followed by another expression.
notExpr :: Parser ADT
notExpr = do
    _ <- opStr "!"  -- Match the `!` operator
    Not <$> factor  -- Parse the following expression and apply the Not constructor

-- `andExpr` is a parser for the logical AND (`&&`) operator.
andExpr :: Parser (ADT -> ADT -> ADT)
andExpr = opStr "&&" >> pure And  -- Match the string "&&" and return the And function

-- `orExpr` is a parser for the logical OR (`||`) operator.
orExpr :: Parser (ADT -> ADT -> ADT)
orExpr = opStr "||" >> pure Or    -- Match the string "||" and return the Or function



----------------------------------------------------------------------------------------------------


---------------------------------------- COMPARISON PARSERS ----------------------------------------

-- `equalsOp` is a parser for the strict equality (`===`) operator.
equalsOp :: Parser (ADT -> ADT -> ADT)
equalsOp = spaces >> opStr "===" >> pure Equals

-- `notEqualsOp` is a parser for the strict inequality (`!==`) operator.
notEqualsOp :: Parser (ADT -> ADT -> ADT)
notEqualsOp = opStr "!==" >> pure NotEquals

-- `greaterThanOp` is a parser for the greater than (`>`) operator.
greaterThanOp :: Parser (ADT -> ADT -> ADT)
greaterThanOp = opStr ">" >> pure GreaterThan

-- `lessThanOp` is a parser for the less than (`<`) operator.
lessThanOp :: Parser (ADT -> ADT -> ADT)
lessThanOp = opStr "<" >> pure LessThan

-- `combinedFactor` combines the `factor` and `stringLiteral` parsers, attempting to parse an expression as a factor or a string literal.
combinedFactor :: Parser ADT
combinedFactor = factor <|> stringLiteral

-- `comparison` parses comparison expressions, including strict equality, inequality, greater than, and less than.
-- The expression is expected to be enclosed in parentheses.
comparison :: Parser ADT
comparison = bracketed (chain combinedFactor comparisonOp)

-- `comparisonOp` attempts to parse any of the four comparison operators in order: strict equality, strict inequality, greater than, or less than.
comparisonOp :: Parser (ADT -> ADT -> ADT)
comparisonOp = equalsOp <|> notEqualsOp <|> greaterThanOp <|> lessThanOp

----------------------------------------------------------------------------------------------------


----------------------------------- RUNNING MAIN PARSERS---------------------------------------------

-- `exprs` attempts to parse a list of expressions separated by spaces. 
exprs :: Parser [ADT]
exprs = concat <$> (many (spaces *> exprList) <* spaces)

-- `singleExpr` tries to match one of several possible expressions in the given order.
singleExpr :: Parser ADT
singleExpr =
    constDeclarations <|>
    logicLowest <|>
    comparison <|>
    arithExpr <|>
    returnStatement <|>
    ifStatement <|>
    functionCall <|>
    functionBlock <|>
    manualCodeBlock <|>
    codeBlock <|>
    functionCallNoSpace <|>
    ternaryExpr <|>
    list

-- `exprList` tries to parse different types of expressions and wraps the result in a list.
exprList :: Parser [ADT]
exprList =
    (pure <$> ifStatement)
    <|> (pure <$> functionBlock)
    <|> (pure <$> singleExpr)
    <|> (pure <$> manualCodeBlock)

-- `factor` matches fundamental units in our language. These can be numbers, boolean values, variable names, 
-- function calls, or more complex expressions surrounded by brackets.
factor :: Parser ADT
factor =
    number
    <|> bracketed (chain term addop)
    <|> trueExpr
    <|> falseExpr
    <|> bracketed expr
    <|> stringLiteral
    <|> notExpr
    <|> tryIt functionCall
    <|> Declaration <$> varName
    <|> combinedLogicAndComparison

-- `factorLogic` is similar to `factor` but focuses more on logical expressions.
factorLogic :: Parser ADT
factorLogic =
    trueExpr
    <|> falseExpr
    <|> functionCall
    <|> Declaration <$> varName
    <|> notExpr
    <|> number
    <|> bracketed expr
    <|> stringLiteral

-- `expr` combines multiple expressions into a single WholeBlock, or just returns a single expression if only one is parsed.
expr :: Parser ADT
expr = do
    statements <- exprs
    case statements of
        [single] -> return single
        _        -> return $ WholeBlock statements


----------------------------------------------------------------------------------------------------


-- | Exercise A

parseExerciseA :: Parser ADT
parseExerciseA = expr


prettyPrintExerciseA :: ADT -> String
prettyPrintExerciseA = adtToString

-- | Exercise B

parseExerciseB :: Parser ADT
parseExerciseB = constDeclarations <|> expr

prettyPrintExerciseB :: ADT -> String
prettyPrintExerciseB = adtToString

-- | Exercise C

isTailRecursive :: String -> Bool
isTailRecursive input =
    case parse (tryIt expr) input of
        -- In case of parsing errors, we return False.
        Error _ -> False

        -- If we successfully parse a function declaration:
        Result _ func@(FunctionDeclaration fName fParams fBody) ->
            -- First, we extract all return statements from the function body.
            let returnStatements = extractReturnStatements fBody

                -- This helper function determines if a given ADT contains a recursive function call.
                containsRecursiveFunctionCalls :: ADT -> Bool
                containsRecursiveFunctionCalls (FunctionCall name _) | name == fName = True
                containsRecursiveFunctionCalls (Plus l r) = containsRecursiveFunctionCalls l || containsRecursiveFunctionCalls r
                containsRecursiveFunctionCalls (Minus l r) = containsRecursiveFunctionCalls l || containsRecursiveFunctionCalls r
                containsRecursiveFunctionCalls _ = False

                -- We verify that all return statements (except the last one) do not contain a recursive function call.
                initialReturnsValid = if null returnStatements
                                      then trace "Return statements is empty!" False
                                      else trace ("Checking initial return statements: " ++ show (init returnStatements)) $
                                           not (any containsRecursiveFunctionCalls (init returnStatements))

                -- We verify that the last return statement (if it exists) is a valid tail recursive call.
                lastReturnValid = if null returnStatements
                                  then trace "Return statements is empty!" False
                                  else case last returnStatements of
                                        ReturnStatement (FunctionCall name args) ->
                                            trace ("Checking last return: FunctionCall " ++ name ++ " with arguments: " ++ show args) $

                                       
                                            name == fName && length args == length fParams && not (any containsRecursiveFunctionCalls args)
                                        ReturnStatement other ->
                                            trace ("Checking other return type: " ++ show other) $
                                            -- For other types of returns, ensure they don't contain a recursive call.
                                            not (containsRecursiveFunctionCalls other)
                                        _ -> trace "Unexpected statement type!" False

            -- If both conditions are met, the function is tail recursive.
            in trace ("initialReturnsValid: " ++ show initialReturnsValid ++ ", lastReturnValid: " ++ show lastReturnValid) $
               initialReturnsValid && lastReturnValid

        -- If the parsed structure doesn't match a function declaration, we return False.
        _ -> trace "Parsed structure doesn't match expected function declaration." False


parseExerciseC :: Parser ADT
parseExerciseC = expr

prettyPrintExerciseC :: ADT -> String
prettyPrintExerciseC = adtToString



