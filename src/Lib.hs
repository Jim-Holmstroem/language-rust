module Lib
    ( test
    ) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

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



test :: IO ()
test = print "hello"
