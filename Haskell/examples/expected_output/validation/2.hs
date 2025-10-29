newtype Number = Number Int
    deriving Show

newtype Duplicated = Duplicated String
    deriving Show

number :: Parser Number
number = Number <$> int

duplicated :: Parser Duplicated
duplicated = Duplicated <$> (string "a")
