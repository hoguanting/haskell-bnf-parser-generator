data Expression = Expression1 Term
                | Expression2 Term String Expression
    deriving Show

data Term = Term1 Factor
          | Term2 Factor String Term
    deriving Show

data Factor = Factor1 String Expression String
            | Factor2 Number
    deriving Show

newtype Number = Number Int
    deriving Show

expression :: Parser Expression
expression = Expression1 <$> term
           <|> Expression2 <$> term <*> (string "+") <*> expression

term :: Parser Term
term = Term1 <$> factor
     <|> Term2 <$> factor <*> (string "*") <*> term

factor :: Parser Factor
factor = Factor1 <$> (string "(") <*> expression <*> (string ")")
       <|> Factor2 <$> number

number :: Parser Number
number = Number <$> int
