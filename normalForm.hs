-- Normal Form practice (ch 11)

{--
-- Sum Type
data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show
--}

type Gardener = String

{--
-- Product Type
data Garden = Garden Gardener FlowerType
    deriving Show
--}

-- Sum of Products
data Garden = Gardenia Gardener
            | Daisy Gardener
            | Rose Gardener
            | Lilac Gardener
            deriving Show
