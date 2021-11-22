{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Random ( randomIO, uniformR, mkStdGen, StdGen )
import Data.List ( foldl', elemIndex )
import Data.Maybe (fromJust)
import Control.Monad ( when )

nextCustomerTime :: Double -> Double
nextCustomerTime p = -100 * log (1 - p)

customerProcessingTimeF :: Double -> Double -> Double -> Double
customerProcessingTimeF a b p = 200*(p**(a-1))*((1-p)**(b-1))

yellowCustomerTime :: Double -> Double
yellowCustomerTime = customerProcessingTimeF 2 5
redCustomerTime :: Double -> Double
redCustomerTime = customerProcessingTimeF 2 2
blueCustomerTime :: Double -> Double
blueCustomerTime = customerProcessingTimeF 5 1

data ExperimentState = Init { randomGen :: StdGen } | ExperimentState {
    tellerIsFreeAt :: Double,
    currentTime :: Double,
    maxWaitTime :: Double,
    totalWaitTime :: Double,
    events :: [Event],
    randomGen :: StdGen
}

data LeaveArrive = Leave | Arrive deriving Eq
data Event = Event { time :: Double, what :: LeaveArrive }
instance Eq Event where
    (Event x _) == (Event y _) = x == y
instance Ord Event where
    compare (Event x _) (Event y _) = compare x y

get2Random :: StdGen -> (Double, Double, StdGen)
get2Random g = let (p1, g1) = uniformR (0, 1) g; (p2, g') = uniformR (0, 1) g1 in (p1, p2, g')

insertDescending :: (Ord a) => [a] -> a -> [a]
insertDescending [] x = [x]
insertDescending all@(x:xs) y = if y > x then y:all else x:insertDescending xs y

addCustomer :: ExperimentState -> (Double -> Double) -> ExperimentState
addCustomer (Init g) customerProcessingTime =
    let
        (p1, p2, g') = get2Random g -- random numbers p1, p2 between 0 and 1
        x = nextCustomerTime p1; y = customerProcessingTime p2
        c' = x -- new currentTime
        f' = x + y -- updated tellerIsFreeAt time
        t' = 0 -- new totalWaitTime
        m' = 0 -- new maxWaitTime
        es' = [Event f' Leave, Event c' Arrive]
    in ExperimentState f' c' m' t' es' g'
addCustomer (ExperimentState f c mw t es g) customerProcessingTime =
    let
        (p1, p2, g') = get2Random g -- random numbers p1, p2 between 0 and 1
        x = nextCustomerTime p1; y = customerProcessingTime p2
        c' = c + x -- new currentTime
        tellerIsFreeOnArrival = c' >= f
        f' = if tellerIsFreeOnArrival then c' + y else f + y -- updated tellerIsFreeAt time
        waitTime = if tellerIsFreeOnArrival then 0 else f - c'
        t' = t + waitTime -- new totalWaitTime
        m' = max mw waitTime -- new maxWaitTime
        es' = insertDescending (insertDescending es (Event c' Arrive)) (Event f' Leave)
    in ExperimentState f' c' m' t' es' g'

maxAvgQLength :: [Event] -> (Integer, Double) -- weighted average over time
maxAvgQLength es = (maxQueueLength, weightedQueueLengths / totalTime)
    where
        (maxQueueLength, weightedQueueLengths, totalTime, _) = foldr (\(Event eventT eventAL) (accMax, accAvg, accTime, accQueue) ->
            let
                accQueue' = if eventAL == Arrive then accQueue + 1 else accQueue - 1 -- adjust the queue length based on arrival or leaving
                deltaT = eventT - accTime -- elapsed time since last queue change
                accAvg' = accAvg + deltaT * fromIntegral accQueue -- weigh the queue length with elapsed time
                accTime' = eventT
                accMax' = max accMax accQueue'
            in (accMax', accAvg', accTime', accQueue')
            ) (0, 0, 0, 0) es
-- maxAvgQLength :: [Event] -> (Integer, Double) -- average over different queue lengths
-- maxAvgQLength es = (maxQueueLength, fromIntegral totalQueueLengths / totalQueues)
--     where
--         (maxQueueLength, totalQueueLengths, totalQueues, _) = foldr (\(Event eventT eventAL) (accMax, accAvg, accCount, accQueue) ->
--             let
--                 accQueue' = if eventAL == Arrive then accQueue + 1 else accQueue - 1
--                 accAvg' = accAvg + accQueue
--                 accCount' = accCount + 1.0
--                 accMax' = max accMax accQueue'
--             in (accMax', accAvg', accCount', accQueue')
--             ) (0, 0, 0, 0) es

printAll = 1; printWait = 2; printQueue = 3; printNothing = 4

runSimulation :: Int -> (String, Double -> Double, Int) -> IO Double
runSimulation sampleSize (tag, customerTime, printWhat) = do
    r :: Int <- randomIO
    let init = Init $ mkStdGen r
        ExperimentState _ _ m t events _ = foldl' (\acc _ -> addCustomer acc customerTime) init [1..sampleSize]
        (mQ, aQ) = maxAvgQLength events
        avgWait = t / fromIntegral sampleSize
    when (printWhat == printAll || printWhat == printWait) ( do
        print $ tag ++ " customer max wait time: " ++ show m
        print $ tag ++ " customer avg wait time: " ++ show avgWait )
    when (printWhat == printAll || printWhat == printQueue) ( do
        print $ tag ++ " customer max queue length: " ++ show mQ
        print $ tag ++ " customer avg queue length: " ++ show aQ )
    return (m - avgWait)

main :: IO ()
main = do
    let
        sampleSize = 200000
        inputs = [("Yellow", yellowCustomerTime, printWait), ("Red", redCustomerTime, printQueue), ("Blue", blueCustomerTime, printNothing)]
    results <- mapM (runSimulation sampleSize) inputs
    let
        m = minimum results
        i = fromJust $ elemIndex m results
        (tag, _, _) = inputs !! i
    print $ tag ++ " customers have the closest value between the average and maximum customer waiting times"
