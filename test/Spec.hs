import Test.HUnit
import TypeChecker

correctValues :: [String]
correctValues = 
    [ "((Y -> X) -> (Y -> X))"
    , "((A -> (A -> G)) -> (A -> (B -> G)))"
    , "(((A -> G) -> A) -> ((A -> G) -> (B -> G)))"
    , "((A -> (B -> G)) -> ((A -> B) -> (A -> G)))"
    , "((G -> (A -> B)) -> (G -> (((A -> B) -> B) -> B)))"
    , "((A -> (A -> B)) -> (A -> ((B -> G) -> G)))"
    , "((B -> (A -> G)) -> (A -> ((A -> B) -> G)))"
    ]

errorsCheck :: [String]
errorsCheck = 
    [ "Error: Parameter type mismatch. Expected \"Y\", but found \"WrongType\" !!!" 
    , "Error: Arrow type expected, but \"G\" found !!!"
    , "Error: Ambiguous type binding [\"y : B\",\"y : (A -> G)\"] !!!"
    , "Error: Variable lookup failure: \"w\" !!!"
    , "Error: Empty context !!!"
    ]   

main :: IO ()
main = do
    correct <- readFile "./test/resources/correct.txt"
    wrong   <- readFile "./test/resources/wrong.txt"
    let 
        correctTerms = checkTypeStr correct
        testLengthC = TestCase (assertEqual "Length of ouput" (length correctValues) (length correctTerms))
        zippedC =  zip correctValues correctTerms
        testsCorrect = map (\(x, y) -> TestCase (assertEqual ("for " ++ show y) x y)) zippedC
        
        wrongTerms = checkTypeStr wrong
        testLengthW = TestCase (assertEqual "Length of output" (length errorsCheck) (length wrongTerms))
        zippedW =  zip errorsCheck wrongTerms
        testsWrong = map (\(x, y) -> TestCase (assertEqual ("for " ++ show y) x y)) zippedW
        
        bigTest = TestList $ (testLengthC : testsCorrect) ++ (testLengthW : testsWrong)

    _ <- runTestTT bigTest
    return ()