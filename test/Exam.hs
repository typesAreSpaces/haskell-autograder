
-- methods and data structures used for verifying the solutions and displaying the results

data Problem = Problem {
    title :: String,
    description :: String,
    score :: Double,
    tests :: [Test]
} deriving (Show)

data Test = Test {
    name :: String,
    function :: String,
    expected :: String,
    result :: String
} deriving (Show)
