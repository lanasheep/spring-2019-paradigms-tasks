import Test.Tasty
import Test.Tasty.HUnit

import Robots

main :: IO ()
main = defaultMain testsRobots

testsRobots :: TestTree
testsRobots = let
        walter = robot "Walter" 50 70
        deadRobot = robot "_" 1 0
        opponent = robot "Opponent" 20 80
    in testGroup "Unit tests for Robots task"
        [ testCase "Test for robot constructor" $
            walter @?= ("Walter", 50, 70)

        , testCase "Test for getName" $
            getName walter @?= "Walter"
        , testCase "Test for getAttack" $
            getAttack walter @?= 50
        , testCase "Test for getHealth" $
            getHealth walter @?= 70

        , testCase "Test for setName" $
            setName "Walter1" walter @?= ("Walter1", 50, 70)
        , testCase "Test for setAttack" $
            setAttack 30 walter @?= ("Walter", 30, 70)
        , testCase "Test for setHealth" $
            setHealth 90 walter @?= ("Walter", 50, 90)

        , testCase "Test for printRobot" $
            printRobot walter @?= "Walter, attack: 50, health: 70"

        , testCase "Test for damage" $
            damage walter 30 @?= ("Walter", 50, 40)

        , testCase "Test for isAlive for dead robot" $
            isAlive deadRobot @?= False
        , testCase "Test for isAlive for alive robot" $
            isAlive walter @?= True

        , testCase "Test for fight with dead robot" $
            fight deadRobot walter @?= walter
        , testCase "Test for fight with alive robot" $
            fight opponent walter @?= ("Walter", 50, 50)

        , testCase "Test for threeRoundFight" $
            threeRoundFight opponent walter @?= ("Opponent", 20, 30)

        , testCase "Test for neueRobotAttak" $
            neueRobotAttak walter @?= ("Walter", 50, -430)

        , testCase "Test for survivors" $
            survivors @?= [("Basya", 424, 666)]
        ]
