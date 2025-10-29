newtype Start = Start Expr
    deriving Show

data Expr = Expr1 Term String Expr
          | Expr2 Term
    deriving Show

newtype Term = Term Factor
    deriving Show

data Factor = Factor1 Expr String Factor
            | Factor2 Number
    deriving Show

newtype Number = Number Int
    deriving Show

newtype Duplicated = Duplicated String
    deriving Show

newtype Duplicated = Duplicated String
    deriving Show

start :: Parser Start
start = Start <$> expr

expr :: Parser Expr
expr = Expr1 <$> term <*> (string "+") <*> expr
     <|> Expr2 <$> term

term :: Parser Term
term = Term <$> factor

factor :: Parser Factor
factor = Factor1 <$> expr <*> (string "*") <*> factor
       <|> Factor2 <$> number

number :: Parser Number
number = Number <$> int

duplicated :: Parser Duplicated
duplicated = Duplicated <$> (string "a")

duplicated :: Parser Duplicated
duplicated = Duplicated <$> (string "b")
