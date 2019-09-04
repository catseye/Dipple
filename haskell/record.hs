data Product = Product {
        desc :: String,
        sku :: String,
        qty :: Int,
        weight :: Float
    } deriving (Show)


doubleQty p@Product{ qty=x } = p { qty=x * 2 }

demo =
   let
       p = Product{ desc="White paint", sku="PW00", qty=1, weight=14.4 }
       p' = p { qty=100 }
       q = weight p'
   in
       show (p', q)
