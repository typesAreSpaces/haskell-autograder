module Grader where

import Color
import Text.Printf

-- methods and data structures used for verifying the solutions 
-- and displaying the results

data Problem = Problem {
    title :: String,
    description :: String,
    tests :: [Test]
} deriving (Show)

data Test = Test {
    name :: String,
    score :: Int,
    function :: String,
    expected :: String,
    result :: String
} deriving (Show)

mapConcat f = foldr (++) [] . map f

line :: String
line = "--------------------------------------------------------------------------------"

outro result = printf "%s\n    Results: %s\n%s\n\n\n" line result line

-- problems \equiv [Problem]
--calculateSolved  problems = sum $ map (solveTests . tests) problems
--calculateGiven   problems = sum $ map (length . tests) problems
--calculateScored  problems = sum $ map scoreProblem problems
--calculateMaximum problems = map (sum . (map score) . tests) problems

calculateSolved :: [Problem] -> Int
calculateSolved  problems = 1
calculateGiven :: [Problem] -> Int
calculateGiven   problems = 1

calculateScored :: [Problem] -> Int
calculateScored  problems = 1
calculateMaximum :: [Problem] -> Int
-- calculateMaximum problems = sum $ map (sum . (map score) . tests) problems
calculateMaximum problems = 1
           
checkTest :: Test -> Bool
checkTest test = True -- TODO using hint

scoreTest :: Test -> Int
scoreTest test = 1 -- TODO using hint

--scoreProblem (Problem _ score tests) =
    --let solved = fromIntegral $ solveTests tests
        --given = fromIntegral $ length tests
    --in score * solved / given

scoreProblem    (Problem _ _ tests) = sum $ map scoreTest $ tests
scoreProblemMax (Problem _ _ tests) = sum $ map score     $ tests

showScore color solved given scored maximum =
    paint color $ printf "[%d/%d] | [%d/%d]" solved given scored maximum

showResult solved given scored maximum =
    showScore (resultColor scored maximum) solved given scored maximum

showLabel solved given scored maximum =
    showScore (labelColor scored maximum) solved given scored maximum

showTest test@(Test name score function expected result) =
    let correct = checkTest test
        label = if correct then paint Green "PASS" else paint Red "FAIL"
        arrow = paint Green "=>"
        corrected = paint Green expected
        mistake = if correct then "" else printf "| %s" (paint Red result)
        header = printf "    %s  %s\n" label name
        body   = printf "          %s %s %s %s\n\n" 
          function arrow corrected mistake
    in header ++ body

showTests :: [Test] -> String
showTests tests = mapConcat showTest tests

showProblem problem@(Problem title description tests) =
    let 
        solved = scoreProblem problem
        given = length tests
        scored = scoreProblem problem
        maximum = scoreProblemMax problem
        label = showLabel solved given scored maximum  
        header = printf "%s    %s\n\n" label title
        body = printf "%s\n\n%s\n\n" description (showTests tests)
    in header ++ body

showProblems problems = mapConcat showProblem problems

showSubmission submission =
    let solved = calculateSolved submission
        given = calculateGiven submission
        scored = calculateScored submission
        maximum = calculateMaximum submission
        result = showResult solved given scored maximum
    in showProblems submission ++ outro result
