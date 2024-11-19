data FailableDouble = Failure
                      |OK Double
 deriving (Show)


safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
 deriving Show

shoe :: Thing
shoe = Shoe

listO'Things :: [Thing]
listO'Things = [Shoe, SealingWax, King, Cabbage, King]

isSmall :: Thing -> Bool
isSmall Ship = False
isSmall King = False
isSmall _ = True


-- Store a person's name, age and favorite thing
data Person = Person String Int Thing deriving Show

brent :: Person
brent = Person "Brent" 31 SealingWax

stan :: Person
stan = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a

baz :: Person -> String
baz p@(Person n _ _) = "The name of (" ++ show p ++ " )is " ++ n

failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
                    Failure -> 0
                    OK d    -> d

data IntList = Empty | Cons Int IntList

