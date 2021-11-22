{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Random ( randomIO, uniformR, mkStdGen, StdGen )
import Data.List ( foldl' )


nextCustomerTime :: Double -> Double
nextCustomerTime p = -200 * log (1 - p)

customerProcessingTimeF :: Double -> Double -> Double -> Double
customerProcessingTimeF a b p = 200*p**(a-1)*(1-p)**(b-1)

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

insertDescendingted :: (Ord a) => [a] -> a -> [a]
insertDescendingted [] x = [x]
insertDescendingted all@(x:xs) y = if y > x then y:all else x:insertDescendingted xs y

addCustomer :: ExperimentState -> (Double -> Double) -> ExperimentState
addCustomer (Init g) customerProcessingTime =
    let
        (p1, p2, g') = get2Random g -- random numbers p1, p2 between 0 and 1
        x = nextCustomerTime p1; y = customerProcessingTime p2
        c' = x -- new currentTime
        f' = x + y -- updated tellerIsFreeAt time
        t' = 0 -- new totalWaitTime
        m' = 0 -- new maxWaitTime
        es' = [Event c' Arrive, Event f' Leave]
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
        es' = insertDescendingted (insertDescendingted es (Event c' Arrive)) (Event f' Leave)
    in ExperimentState f' c' m' t' es' g'

maxAvgQLength :: [Event] -> (Integer, Double)
maxAvgQLength es = (m, a / t)
    where
        (m, a, t, q) = foldr (\(Event eventT eventAL) (accMax, accAvg, accTime, accQueue) ->
            let
                accQueue' = if eventAL == Arrive then accQueue + 1 else accQueue - 1
                deltaT = eventT - accTime
                accAvg' = accAvg + deltaT * fromIntegral accQueue
                accTime' = eventT
                accMax' = max accMax accQueue'
            in (accMax', accAvg', accTime', accQueue')
            ) (0, 0, 0, 0) es

runSimulation :: Int -> String -> (Double -> Double) -> IO ()
runSimulation sampleSize tag customerTime = do
    r :: Int <- randomIO
    let init = Init $ mkStdGen r
        ExperimentState _ _ m t events _ = foldl' (\acc _ -> addCustomer acc customerTime) init [1..sampleSize]
        (mQ, aQ) = maxAvgQLength events
    print $ tag ++ " customer max wait time: " ++ show m
    print $ tag ++ " customer avg wait time: " ++ show (t / fromIntegral sampleSize)
    print $ tag ++ " customer max queue length: " ++ show mQ
    print $ tag ++ " customer avg queue length: " ++ show aQ

main :: IO ()
main = do
    let
        sampleSize = 100000
    mapM_ (uncurry $ runSimulation sampleSize) [("Yellow", yellowCustomerTime), ("Red", redCustomerTime), ("Blue", blueCustomerTime)]