newtype Program = Program [Statement]
    deriving Show

data Statement = Statement1 String Expression String (Maybe Comment) Char
    deriving Show

data Expression = Expression1 Term
                | Expression2 Term String Expression
    deriving Show

data Term = Term1 Factor
          | Term2 Factor String Term
    deriving Show

data Factor = Factor1 Number
            | Factor2 Variable
            | Factor3 String Expression String
    deriving Show

newtype Number = Number Int
    deriving Show

newtype Variable = Variable String
    deriving Show

data Pair a b = Pair1 a String b
    deriving Show

newtype NumberPair = NumberPair (Pair Int Int)
    deriving Show

data Comment = Comment1 String String
    deriving Show

program :: Parser Program
program = Program <$> (many statement)

statement :: Parser Statement
statement = Statement1 <$> (string "console.log(") <*> expression <*> (string ");") <*> (optional comment) <*> (is '\n')

expression :: Parser Expression
expression = Expression1 <$> term
           <|> Expression2 <$> term <*> (string "+") <*> expression

term :: Parser Term
term = Term1 <$> factor
     <|> Term2 <$> factor <*> (string "*") <*> term

factor :: Parser Factor
factor = Factor1 <$> (tok number)
       <|> Factor2 <$> (tok variable)
       <|> Factor3 <$> (stringTok "(") <*> expression <*> (stringTok ")")

number :: Parser Number
number = Number <$> int

variable :: Parser Variable
variable = Variable <$> (some alpha)

pair :: Parser a -> Parser b -> Parser (Pair a b)
pair a b = Pair1 <$> a <*> (string ",") <*> b

numberPair :: Parser NumberPair
numberPair = NumberPair <$> (pair int int)

comment :: Parser Comment
comment = Comment1 <$> (string " // ") <*> (some alpha)
