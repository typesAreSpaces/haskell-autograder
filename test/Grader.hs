module Grader where

import Color
import Text.Printf

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

mapConcat f = foldr (++) [] . map f

line :: String
line = "--------------------------------------------------------------------------------"

intro :: String
intro = let title = "--== 14th week Haskell test ==--"
        in printf "\n\n%s\n                      %s\n%s\n\n\n" line title line

outro result = printf "%s\n    Results: %s\n%s\n\n\n" line result line

calculateSolved  problems = sum $ map (solveTests . tests) problems
calculateGiven   problems = sum $ map (length . tests) problems
calculateScored  problems = sum $ map scoreProblem problems
calculateMaximum problems = sum $ map score problems
           
solveTests = length . filter solveTest
solveTest (Test _ _ expected result) = expected == result

scoreProblem (Problem _ _ score tests) =
    let solved = fromIntegral $ solveTests tests
        given = fromIntegral $ length tests
    in score * solved / given

displayScore color solved given scored maximum =
    paint color $ printf "[%d/%d] | [%.2f/%.2f]" solved given scored maximum

displayResult solved given scored maximum =
    displayScore (resultColor scored maximum) solved given scored maximum

displayLabel solved given scored maximum =
    displayScore (labelColor scored maximum) solved given scored maximum

displayTest test@(Test name function expected result) =
    let correct = solveTest test
        label = if correct then paint Green "PASS" else paint Red "FAIL"
        arrow = (paint Green "=>")
        corrected = (paint Green expected)
        mistake = if correct then "" else printf "| %s" (paint Red result)

        header = printf "    %s  %s\n" label name
        body = printf "          %s %s %s %s\n\n" function arrow corrected mistake
    in header ++ body

displayTests :: [Test] -> String
displayTests tests = mapConcat displayTest tests

displayProblem problem@(Problem title description score tests) =
    let solved = solveTests tests
        given = length tests
        scored = scoreProblem problem
        maximum = score
        label = displayLabel solved given scored maximum 
        
        header = printf "%s    %s\n\n" label title
        body = printf "%s\n\n%s\n\n" description (displayTests tests)
    in header ++ body

displayProblems problems = mapConcat displayProblem problems

displaySubmission submission =
    let solved = calculateSolved submission
        given = calculateGiven submission
        scored = calculateScored submission
        maximum = calculateMaximum submission
        result = displayResult solved given scored maximum
    in intro ++ displayProblems submission ++ outro result
