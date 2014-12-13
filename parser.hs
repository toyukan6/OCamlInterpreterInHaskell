module Parser where

import Control.Applicative ((<$>), (<*>), (<|>), (*>), pure, (<*))
import Text.ParserCombinators.Parsec hiding ((<|>))
import Text.ParserCombinators.Parsec.Expr (buildExpressionParser, Assoc(..), Operator(Infix))
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language

import Syntax (Exp(..), BinOp(..), Identifier(..), Type(..), Program(..), Cond(..))

ocamlStyle = emptyDef {
             commentStart      = "(*"
             , commentEnd      = "*)"
             , commentLine     = ";;"
             , nestedComments  = True
             , identStart      = letter <|> char '_'
             , identLetter     = alphaNum <|> oneOf "_"
             , opStart         = opLetter emptyDef
             , opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
             , reservedOpNames = []
             , reservedNames   = ["if", "then", "else", "let", "rec", "in", "and", "true", "false", "match", "with", "fun"]
             , caseSensitive   = True
             }
lexer = Token.makeTokenParser ocamlStyle

whiteSpace = Token.whiteSpace lexer
lexeme = Token.lexeme lexer
symbol = Token.symbol lexer
natural = Token.natural lexer
identifier = Token.identifier lexer
reserved = Token.reserved lexer
operator = Token.operator lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
braces = Token.braces lexer
squares = Token.squares lexer
semi = Token.semi lexer
comma = Token.comma lexer

parseProgram = try (Expression <$> parseExpr)
               <|> parseDeclaration

parseDeclaration = try parseLetDecl
                   <|> parseLetRecDecl

parseLetDecl = 
    Decl <$> (reserved "let" *> parseDecl `sepBy1` (reserved "and")) <*> optionMaybe parseDeclaration

parseLetRecDecl = 
    RecDecl <$> (reserved "let" *> reserved "rec" *> parseDecl `sepBy1` (reserved "and")) <*> optionMaybe parseDeclaration

parseDecl = (\x y -> (x, y)) <$> parseIdList <*> (reservedOp "=" *> parseExpr)

parseExpr = parseOrExpr

parseOrExpr = buildExpressionParser table (lexeme parseAppExp)

table   = [[op "*" Mul AssocLeft]
          ,[op "+" Plus AssocLeft]
          ,[op "::" Cons AssocRight]
          ,[op "<" Lt AssocLeft]
          ,[op "&&" And AssocLeft]
	  ,[op "||" Or AssocLeft]
          ]
        where op s f assoc = Infix (do{ reservedOp s; return (BinOpExp f)}) assoc

parseAppExp =
    (\l -> if length l == 1 then head l else foldl1 (\ x y -> AppExp x y) l) <$> many1 (lexeme parseAExp)

parseAExp = parseNumber
            <|> parseVar
            <|> parseBool
            <|> parseList
            <|> parseIf
            <|> parseFun
            <|> parseLet
            <|> parseMatch
            <|> parens parseExpr

parseIdentifier = Ident <$> identifier

parseIdList = many1 parseIdentifier

parseNumber = ILit . read <$> many1 digit

parseVar = Var <$> parseIdentifier

parseBool = parseTrue <|> parseFalse
parseTrue = reserved "true" *> pure (BLit True)
parseFalse = reserved "false" *> pure (BLit False)

parseList = LLit <$> squares (parseExpr `sepBy` semi)

parseIf =
    IfExp <$> (reserved "if" *> parseExpr) <*> (reserved "then" *> parseExpr) <*> parseElse

parseElse = reserved "else" *> parseExpr <|> pure Unit

parseFun =
    FunExp <$> (reserved "fun" *> parseIdList) <*> (reservedOp "->" *> parseExpr)

parseLet = try parseLetExp
           <|> parseLetRecExp

parseLetExp =
    LetExp <$> (reserved "let" *> parseDecl `sepBy1` (reserved "and")) <*> (reserved "in" *> parseExpr)

parseLetRecExp =
    LetRecExp <$> (reserved "let" *> reserved "rec" *> parseDecl `sepBy1` (reserved "and")) <*> (reserved "in" *> parseExpr)

parseMatch =
    MatchExp <$> (reserved "match" *> parseExpr) <*> (reserved "with" *> parseMatchPattern `sepBy1` (reservedOp "|"))

parseMatchPattern = (\x y -> (x, y)) <$> (lexeme parseCond) <*> (reservedOp "->" *> parseExpr)

parseCond = try parseConsCond
            <|> parseValCond

parseValCond = parseIntCond
            <|> parseBoolCond
            <|> parseVarCond
            <|> parseListCond

parseIntCond = (\(ILit n) -> IntCond n) <$> parseNumber
parseBoolCond = (\(BLit b) -> BoolCond b) <$> parseBool
parseVarCond = VarCond <$> parseIdentifier
parseConsCond = foldr1 (\ x y -> ConsCond x y) <$> parseValCond `sepBy1` (reservedOp "::")
parseListCond = ListCond <$> squares (parseCond `sepBy` semi)
