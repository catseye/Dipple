data Expr = Int Integer
          | Str String
          | Add Expr Expr
          | Neg Expr
    deriving (Eq, Ord, Show)

concInterp (Int i) = Int i
concInterp (Str s) = Str s
concInterp (Add e1 e2) =
    let
        v1 = concInterp e1
        v2 = concInterp e2
    in
        case (v1, v2) of
            (Int i1, Int i2) -> Int (i1 + i2)
            (Str s1, Str s2) -> Str (s1 ++ s2)
concInterp (Neg e) =
    let
        v = concInterp e
    in
        case v of
            Int i -> Int (-1 * i)


data ExprType = IntType
              | StrType
              | BadType
    deriving (Eq, Ord, Show)

absInterp (Int _) = IntType
absInterp (Str _) = StrType
absInterp (Add e1 e2) =
    let
        t1 = absInterp e1
        t2 = absInterp e2
    in
        case (t1, t2) of
            (IntType, IntType) -> IntType
            (StrType, StrType) -> StrType
            _ -> BadType
absInterp (Neg e) =
    let
        t = absInterp e
    in
        case t of
            IntType -> IntType
            _ -> BadType

i x = case absInterp x of
    BadType -> Nothing
    _ ->       Just $ concInterp x
