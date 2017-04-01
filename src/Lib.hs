module Lib
    ( test
    ) where

import Text.ParserCombinators.Parsec

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

reservedNames = [ "abstract"
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


whitespace_char = oneOf [ '\x20'
                        , '\x09'
                        , '\x0a'
                        , '\x0d'
                        ]
whitespace = many1 whitespace_char

function_name = many1 letter

data Function = Function { name :: String }
    deriving (Show, Eq)
function_def :: Parser Function
function_def = do
    string "fn"
    whitespace
    name <- function_name
    optional whitespace
    char '('
    char ')'
    optional whitespace
    char '{'
    char '}'

    return $ Function name

data Statement = DeclareStatement Declaration
               | ExpressionStatement Expression
               | EmptyStatement
    deriving (Show, Eq)
stmt = decl_stmt <|> expr_stmt <|> char ';'

data Decleration = Item
                 | LetDecleration
    deriving (Show, Eq)
decl_stmt :: Parser Decleration
decl_stmt = item <|> let_decl

item = return Item
let_decl = return LetDecleration

expr_stmt = do
    expr
    char ';'

data Expression = Literal
    deriving (Show, Eq)
expr = literal
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

xid_start, xid_continue :: Parser Char
xid_start = letter
xid_continue = letter
data Identifier = Identifier { name' :: String }  -- TODO move to module
    deriving (Show, Eq)
ident :: Parser Identifier
ident = do
    start <- xid_start
    continue <- many xid_continue
    -- TODO and not in keywords
    return $ Identifier $ start:continue
    
    

test :: IO ()
test = print $ parse function_def "main" "fn main(){}"
