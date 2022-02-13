{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module TypeChecker
     where
import Parser
    ( Term(..), Type(..), lambdaParser )
import Text.Parsec ( parse )
import Data.List (findIndex, elemIndex)
import GHC.Generics (Constructor(conName))

-- Context ADT --

type Name = String

data Formula = Formula { varN :: Name, varType :: BruijnType }
data Context = Context { typeVar :: [Name], termVar :: [Formula] }

-- Unnamed Types (de Bruijn notation) (but with names) --

data BruijnType =
    BType { varName :: String, distance :: Int }
    | BArrow BruijnType BruijnType
    | BForall String BruijnType
    deriving (Show, Eq)

data BruijnTerm =
    BLVar { bv :: String }
    | BLAbs { babsV :: String, bty :: BruijnType, bterm :: BruijnTerm }
    | BLApp { bte1 :: BruijnTerm, bte2 :: BruijnTerm }
    | BLTAbs { babsT :: String, bte :: BruijnTerm }
    | BLTApp { btyAbs :: BruijnTerm, bttype :: BruijnType }
    -- new kind of terms. for parsing purposes only
    | BTermType BruijnType
    deriving Show

toBruijnType :: Type -> [Name] -> ([Name], BruijnType)
toBruijnType (Type varName) names =
    case elemIndex varName names of
        Just n -> (names, BType varName n)
        _ -> (names ++ [varName], BType varName (length names))
toBruijnType (Arrow t1 t2) names = (newNames2, BArrow bt1 bt2)
    where
        (newNames1, bt1) = toBruijnType t1 names
        (newNames2, bt2) = toBruijnType t2 newNames1
toBruijnType (Forall varName ty) names = (newNames, BForall varName bty)
    where
        newNamesWithVar = varName : names
        (newNames, bty) = toBruijnType ty newNamesWithVar


toBruijn :: Term -> [Name] -> BruijnTerm
toBruijn (LVar name) names = BLVar name
toBruijn (LAbs name ty term) names = BLAbs name bty bterm
    where
        (ns, bty) = toBruijnType ty names
        bterm = toBruijn term ns
toBruijn (LApp te1 te2) names = BLApp bte1 bte2
     where
        bte1 = toBruijn te1 names
        bte2 = toBruijn te2 names
toBruijn (LTAbs name term) names = BLTAbs name bterm
    where
        bterm = toBruijn term (name : names)
toBruijn (LTApp term ty) names = BLTApp bterm bty
    where
        (ns, bty) = toBruijnType ty names
        bterm = toBruijn term ns
toBruijn (TermType ty) names = BTermType $ snd $ toBruijnType ty names

-- Functions for Context --

initContext :: Context
initContext = Context [] []

addTypeVar :: Context -> Name -> Context
addTypeVar (Context tyVar teVar) name = Context (name : tyVar) teVar

addFormula :: Context -> Name -> BruijnType -> Context
addFormula (Context tyVar teVar) name ttype = Context tyVar (formula : teVar)
    where
        formula = Formula name ttype

showType :: Type -> String
showType t = case t of
    Type name -> name
    Forall name ttype ->
        "(Forall " ++ name ++ ". " ++ showType ttype ++ ")"
    Arrow type1 type2 ->
        "(" ++ showType type1 ++ " -> " ++ showType type2 ++ ")"

showBruijnType :: BruijnType -> String
showBruijnType t = case t of
    BType n num -> n
    BForall name ttype ->
        "(Forall " ++ name ++ ". " ++ showBruijnType ttype ++ ")"
    BArrow type1 type2 ->
        "(" ++ showBruijnType type1 ++ " -> " ++ showBruijnType type2 ++ ")"

showFormula :: Formula -> String
showFormula (Formula name ttype) = name ++ " : " ++ showBruijnType ttype

getVarType :: Context -> Int -> Either String Name
getVarType (Context vars _) i | i >= length vars = Left "Error: Type variable lookup failure !!!"
getVarType (Context vars _) i | i < 0 = Left "Error: Negative index....."
getVarType (Context vars _) i = Right $ vars !! i

getType :: Context -> Name -> Either String BruijnType
getType (Context _ []) name = Left "Error: Empty context !!!"
getType ctx name = case filter (\x -> varN x == name) (termVar ctx) of
    [] -> Left $ "Error: Variable lookup failure: " ++ show name ++ " !!!"
    l | null $ tail l ->  Right $ varType $ head l
    l -> Left $ "Error: Ambiguous type binding " ++ show (map showFormula l) ++ " !!!"

-- Functions for type substitution in de Bruijn notation --

tymap :: (Int -> Int -> String -> BruijnType) -> Int -> BruijnType -> BruijnType
tymap onvar c tyT = walk c tyT
    where
        walk :: Int -> BruijnType -> BruijnType
        walk c tyT = case tyT of
            BType v x -> onvar c x v
            BArrow tyT1 tyT2 -> BArrow (walk c tyT1) (walk c tyT2)
            BForall tyX tyT2 -> BForall tyX (walk (c+1) tyT2)

typeShiftAbove :: Int -> Int -> BruijnType -> BruijnType
typeShiftAbove d = tymap
        (\c x v-> if x >= c then BType v (x+d) else BType v x)

typeShift :: Int -> BruijnType -> BruijnType
typeShift d = typeShiftAbove d 0

typeSubst :: BruijnType -> Int -> BruijnType -> BruijnType
typeSubst tyS = tymap
    (\j x v -> if x == j then typeShift j tyS else BType v x)

typeSubstTop :: BruijnType -> BruijnType -> BruijnType
typeSubstTop tyS tyT =
    typeShift (-1) (typeSubst (typeShift 1 tyS) 0 tyT)

-- Type checking --

typeOf :: Context -> BruijnTerm -> Either String BruijnType
typeOf ctx term = do
    case term of
        BLVar name -> getType ctx name

        BLAbs name tyT1 term ->  tyT2 >>= (Right . BArrow tyT1 )
            where
                ctx' = addFormula ctx name tyT1
                tyT2 = typeOf ctx' term

        BLApp term1 term2 -> do
                tT1 <- typeOf ctx term1
                tT2 <- typeOf ctx term2
                case tT1 of
                    BArrow type1 type2 ->
                        if type1 == tT2
                        then Right type2
                        else  Left $ "Error: Parameter type mismatch. "
                                     ++ "Expected " ++ show (showBruijnType type1)
                                     ++ ", but found " ++ show (showBruijnType tT2) ++ " !!!"
                    _ -> Left $ "Error: Arrow type expected, but " ++ show (showBruijnType tT1) ++ " found !!!"

        BLTAbs tyName term -> do
            let ctx' = addTypeVar ctx tyName
            BForall tyName <$> typeOf ctx' term

        BLTApp term typeT2 -> do
            typeT1 <- typeOf ctx term
            case typeT1 of
                BForall var typeT12 -> Right $ typeSubstTop typeT2 typeT12
                _ -> Left $ "Error: Forall type expected, but " ++ show (showBruijnType typeT1) ++ " found !!!"

        BTermType t ->
            Left $ "Error: Type application without operator" ++ show ("[" ++ showBruijnType t ++ "]") ++ " !!!"

checkTypeStr :: String -> [String]
checkTypeStr input  = case parse lambdaParser "" input of
    Left err -> [show err]
    Right terms -> map (\x -> typeCheck $ toBruijn x []) terms
        where
            typeCheck :: BruijnTerm -> String
            typeCheck term = case typeOf initContext term of
                Left err -> err
                Right ttype -> showBruijnType ttype

