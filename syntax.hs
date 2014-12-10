module Syntax where

import Environment (Environment)

data Program = Expression Exp
             | Decl [([Identifier], Exp)] (Maybe Program)
             | RecDecl [([Identifier], Exp)] (Maybe Program)
               deriving (Show)

data Identifier = Ident String
                  deriving (Eq)

instance Show Identifier where
    show (Ident s) = s

data BinOp = Plus
           | Mul
           | Lt
           | And
           | Or
           | Cons
             deriving (Show)

data Type = TyInt
          | TyBool
          | TyVar Identifier
          | TyFunc Type Type
            deriving (Eq)

instance Show Type where
    show TyInt = "int"
    show TyBool = "bool"
    show (TyVar v) = show v
    show (TyFunc (TyFunc type1 type2) type3) =
        "( " ++ show type1 ++ " -> " ++ show type2 ++ " ) -> " ++ show type3
    show (TyFunc type1 type2) =
        show type1 ++ " -> " ++ show type2

data Exp = Var Identifier
         | Unit
         | ILit Integer
         | BLit Bool
         | LLit [Exp]
         | BinOpExp BinOp Exp Exp
         | IfExp Exp Exp Exp
         | FunExp [Identifier] Exp
         | LetExp [([Identifier], Exp)] Exp
         | LetRecExp [([Identifier], Exp)] Exp
         | AppExp [Exp]
           deriving (Show)

data Exval = IntV Integer
           | BoolV Bool
           | ListV [Exval]
           | ProcV Identifier Exp (Environment Dnval)
             deriving (Show)

type Dnval = Exval
