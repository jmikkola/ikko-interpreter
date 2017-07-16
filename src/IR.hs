module IR where

import Data.List (intercalate)

import TopLevel (TL, TFunc)

-- TODO: introduce the concept of Structure/Enum type definitions
-- (and perhaps "newtype" support, though that could be done with a single-field structure)
-- Let the compiler layer deal with resolving type aliases

data Statement
  = Return (Maybe Expression)
    -- Let handles both variable and function declarations
  | Set String {- TypeRef -} Expression
  -- | Assign [String] Expression
  | Block {- (Maybe TypeRef) -} [Statement]
  | Expr Expression
  | If Expression Statement (Maybe Statement)
  | While Expression Statement
  -- | Match Expression [MatchCase]
  deriving (Eq, Show)

{-
data MatchCase
  = MatchCase MatchExpression Statement
  deriving (Eq, Show)

caseExpression :: MatchCase -> MatchExpression
caseExpression (MatchCase expr _) = expr

data MatchExpression
  = MatchAnything
  | MatchVariable String
  | MatchStructure TypeRef [MatchExpression]
  deriving (Eq, Show)
-}

class Display a where
  display :: a -> String

data Value
  = StrVal String
  | BoolVal Bool
  | IntVal Int
  | FloatVal Float
  -- | StructVal TypeRef [(String, Expression)]
  | LambdaVal {- TypeRef -} [String] Statement
  | BuiltIn String BuiltInFn
  | EmptyValue
  deriving (Eq, Show)

instance Display Value where
  display (StrVal s)         = s
  display (BoolVal b)        = show b
  display (IntVal i)         = show i
  display (FloatVal f)       = show f
  display (LambdaVal args _) = "fn (" ++ (intercalate ", " args) ++ ")"
  display (BuiltIn name _)   = name ++ "()"
  display EmptyValue         = "()"

data BuiltInFn = BuiltInFn (TFunc Value Value)

instance Show BuiltInFn where
  show _ = "<builtin>"

instance Eq BuiltInFn where
  (==) _ _ = False

data Expression
  = Paren Expression {- TypeRef -}
  | Val Value
  | Unary {- TypeRef -} UnaryOp Expression
  | Binary {- TypeRef -} BinOp Expression Expression
  | Call {- TypeRef -} Expression [Expression]
  -- | Cast {- TypeRef -} Expression
  | Var {- TypeRef -} String
  | GVar {- TypeRef -} String
  | Arg {- TypeRef -} Int
  -- | Access {- TypeRef -} Expression String
  -- | Lambda {- TypeRef -} [String] Statement
  deriving (Eq, Show)

data UnaryOp
  = BitInvert
  | BoolNot
  deriving (Eq, Show)

data BinOp
  = Plus
  | Minus
  | Times
  | Divide
  | Mod
  | Power
  | BitAnd
  | BitOr
  | BitXor
  | BoolAnd
  | BoolOr
  | Eq
  | NotEq
  | Less
  | LessEq
  | Greater
  | GreaterEq
  | LShift
  | RShift
  | RRShift
  deriving (Eq, Show)
