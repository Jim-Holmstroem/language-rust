{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( test
    ) where

import           Data.Char
import           Data.Either

import           Control.Monad
import           Control.Applicative ((<$>))

import qualified Data.Text as Text
import           Data.Text.Read (hexadecimal)

import           Text.Parsec
import           Text.Parsec.Prim
import           Text.Parsec.Combinator

type Parser = Parsec String () -- TODO find its location (not compatability)


unop = [ "+"
       , "-"
       , "!"
       ]

binop = arith_op ++ bitwise_op ++ lazy_bool_op ++ comp_op
arith_op = [ "+"
           , "-"
           , "*"
           , "/"
           , "%"
           ]
bitwise_op = [ "&"
             , "|"
             , "^"
             , "<<"
             , ">>"
             ]
lazy_bool_op = [ "&&"
               , "||"
               ]
comp_op = [ "=="
          , "!="
          , "<"
          , ">"
          , "<="
          , ">="
          ]

-- https://github.com/rust-lang/rust/blob/master/src/doc/grammar.md#keywords
keywords = [ "abstract"
           , "alignof"
           , "as"
           , "become"
           , "box"
           , "break"
           , "const"
           , "continue"
           , "crate"
           , "do"
           , "else"
           , "enum"
           , "extern"
           , "false"
           , "final"
           , "fn"
           , "for"
           , "if"
           , "impl"
           , "in"
           , "let"
           , "loop"
           , "macro"
           , "match"
           , "mod"
           , "move"
           , "mut"
           , "offsetof"
           , "override"
           , "priv"
           , "proc"
           , "pub"
           , "pure"
           , "ref"
           , "return"
           , "Self"
           , "self"
           , "sizeof"
           , "static"
           , "struct"
           , "super"
           , "trait"
           , "true"
           , "type"
           , "typeof"
           , "unsafe"
           , "unsized"
           , "use"
           , "virtual"
           , "where"
           , "while"
           , "yield"
           ]

-- https://github.com/rust-lang/rust/blob/master/src/doc/grammar.md#whitespace
whitespace_char :: Parser ()
whitespace_char = oneOf [ '\x20'
                        , '\x09'
                        , '\x0a'
                        , '\x0d'
                        ] >> return ()
whitespace = many1 whitespace_char

-- https://github.com/rust-lang/rust/blob/master/src/doc/grammar.md#delimiter-restricted-productions
non_null, non_eol, non_single_quote, non_double_quote :: Parser Char
non_null = noneOf "\x00"
non_eol = noneOf "\x00\r\n"
non_single_quote = noneOf "\x00\'"
non_double_quote = noneOf "\x00\""


-- function_name = many1 letter

--data Function = Function { name :: String }
--    deriving (Show, Eq)
--function_def :: Parser Function
--function_def = do
--    string "fn"
--    whitespace
--    name <- function_name
--    optional whitespace
--    char '('
--    char ')'
--    optional whitespace
--    char '{'
--    char '}'
--
--    return $ Function name

--data Statement = DeclareStatement Declaration
--               | ExpressionStatement Expression
--               | EmptyStatement
--    deriving (Show, Eq)

--stmt :: Parser Statement
--stmt = decl_stmt <|> expr_stmt <|> empty_stmt

--empty_stmt :: Parser Statement
--empty_stmt = char ';' >> return EmptyStatement

--data Declaration = Item
--                 | LetDeclaration
--    deriving (Show, Eq)
--decl_stmt :: Parser Declaration
--decl_stmt = item <|> let_decl

--item :: Parser Declaration
--item = return Item
--let_decl :: Parser Declaration
--let_decl = return LetDeclaration

--expr_stmt :: Parser Statement
--expr_stmt = do
--    expression <- expr
--    char ';'
--    return $ ExpressionStatement expression

--data Expression = Literal
--    deriving (Show, Eq)
--expr = literal
--    <|> path
--    <|> tuple_expr
--    <|> unit_expr
--    <|> struct_expr
--    <|> block_expr
--    <|> method_call_expr
--    <|> field_expr
--    <|> array_expr
--    <|> idx_expr
--    <|> range_expr
--    <|> unop_expr
--    <|> binop_expr
--    <|> paren_expr
--    <|> call_expr
--    <|> lambda_expr
--    <|> while_expr
--    <|> loop_expr
--    <|> break_expr
--    <|> continue_expr
--    <|> for_expr
--    <|> if_expr
--    <|> match_expr
--    <|> if_let_expr
--    <|> while_let_expr
--    <|> return_expr

-- https://github.com/rust-lang/rust/blob/master/src/doc/grammar.md#comments
-- data Comment = BlockComment String
--              | LineComment String
--     deriving (Show, Eq)
-- comment :: Parser Comment
-- comment = block_comment <|> line_comment

-- TODO nested block comments
-- block_comment :: Parser Comment
-- block_comment = BlockComment <$> between (string "/*") (string "*/") $ many character

-- character = non_null -- TODO what constitues "character"?

-- TODO check behaviour of between (better with TakeTill ?)

-- line_comment :: Parser Comment
-- line_comment = LineComment <$> between (string "//") eol (many non_eol)

-- https://github.com/rust-lang/rust/blob/master/src/doc/grammar.md#identifiers
data Identifier = Identifier { name :: String }  -- TODO move to module
    deriving (Show, Eq)
xid_start, xid_continue :: Parser Char
xid_start = letter <|> char '_'
xid_continue = xid_start <|> digit
ident :: Parser Identifier
ident = do
    start <- xid_start
    continue <- many xid_continue
    let name = start:continue
    if not $ name `elem` keywords then
        return $ Identifier { name = name }
    else
        fail $ "invalid identifier name, '" ++ name ++ "' is a keyword"

-- https://github.com/rust-lang/rust/blob/master/src/doc/grammar.md#literals
data Literal = StringLiteral String
             | CharLiteral Char
             -- | ByteStringLiteral .. TODO
             -- | ByteLiteral .. TODO
             -- | NumericLiteral Numeric
             | BoolLiteral Bool
    deriving (Show, Eq)
-- TODO "lit_suffix ?" (123i32)
literal :: Parser Literal
literal =  char_lit
       -- string_lit (first of the literal, char_lit 2nd)
       -- <|> byte_string_lit
       -- <|> byte_lit
       -- <|> num_lit
       <|> bool_lit

char_lit :: Parser Literal
char_lit = CharLiteral <$> between (char '\'') (char '\'') char_body
char_body = non_single_quote <|> do
    char '\\'
    choice [ char '\'' >> return '\''
           , common_escape
           -- , unicode_escape -- TODO fix unicode
           ]

common_escape :: Parser Char
common_escape = choice [ char '\\' >> return '\\'
                       , char 'n' >> return '\n'
                       , char 'r' >> return '\r'
                       , char 't' >> return '\t'
                       , char '0' >> return '\0'
                       , char 'x' >> do
                           hexResponse <- (chr . hexadecimal . Text.pack) <$> count 2 hex_digit
                           case hexResponse of
                               Right (char', _) -> return char'
                               Left msg -> fail $ "failed to parse hex '" ++ msg ++ "' for common_escape"
                       ]

-- unicode_escape = do

hex_digit, oct_digit, dec_digit, nonzero_dec :: Parser Char
hex_digit = (oneOf $ ['a'..'f'] ++ ['A'..'F']) <|> dec_digit
oct_digit = oneOf ['0'..'7']
dec_digit = (oneOf "0") <|> nonzero_dec
nonzero_dec = oneOf ['1'..'9']

-- string_lit :: Parser Literal
-- string_lit = between (char '"') (char '"') -- has issues with "\""
    

bool_lit :: Parser Literal
bool_lit = (string "true" >> (return $ BoolLiteral True)) <|> (string "false" >> (return $ BoolLiteral False)) <?> "boolean literal"


test :: IO ()
test = print $ parse ident "main" "sofia"
