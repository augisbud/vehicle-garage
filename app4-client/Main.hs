{-# LANGUAGE DeriveFunctor #-}

module Main (main) where

import Control.Lens hiding (view)
import Control.Monad.Free (Free (..), liftF)
import Control.Monad.State
import Data.String.Conversions (cs)
import Network.Wreq hiding (get)
import Data.ByteString.Lazy.Char8 (ByteString)
import System.Environment (getArgs)

data Command next
  = AddVehicle String String Int Int next
  | PerformMaintenance String String String next
  | SellVehicle String String Int Double next
  | Inventory String (String -> next)
  | View (String -> next)
  deriving Functor

type VehicleDSL = Free Command

addVehicle :: String -> String -> Int -> Int -> VehicleDSL ()
addVehicle vt model year mileage = liftF $ AddVehicle vt model year mileage ()

performMaintenance :: String -> String -> String -> VehicleDSL ()
performMaintenance vt mt duration = liftF $ PerformMaintenance vt mt duration ()

sellVehicle :: String -> String -> Int -> Double -> VehicleDSL ()
sellVehicle vt model year price = liftF $ SellVehicle vt model year price ()

inventory :: String -> VehicleDSL String
inventory vt = liftF $ Inventory vt id

view :: VehicleDSL String
view = liftF $ View id

-- HTTP Request per Command
runHttpSingle :: VehicleDSL a -> IO a
runHttpSingle (Pure a) = return a
runHttpSingle (Free (AddVehicle vt model year mileage next)) = do
  putStrLn $ "Sending request: add_vehicle(" ++ vt ++ ", " ++ model ++ ", " ++ show year ++ ", " ++ show mileage ++ ")"
  _ <- post "http://localhost:3000" (cs $ "add_vehicle(" ++ vt ++ ", " ++ model ++ ", " ++ show year ++ ", " ++ show mileage ++ ")" :: ByteString)
  runHttpSingle next
runHttpSingle (Free (PerformMaintenance vt mt duration next)) = do
  putStrLn $ "Sending request: perform_maintenance(" ++ vt ++ ", " ++ mt ++ ", " ++ duration ++ ")"
  _ <- post "http://localhost:3000" (cs $ "perform_maintenance(" ++ vt ++ ", " ++ mt ++ ", " ++ duration ++ ")" :: ByteString)
  runHttpSingle next
runHttpSingle (Free (SellVehicle vt model year price next)) = do
  putStrLn $ "Sending request: sell_vehicle(" ++ vt ++ ", " ++ model ++ ", " ++ show year ++ ", " ++ show price ++ ")"
  _ <- post "http://localhost:3000" (cs $ "sell_vehicle(" ++ vt ++ ", " ++ model ++ ", " ++ show year ++ ", " ++ show price ++ ")" :: ByteString)
  runHttpSingle next
runHttpSingle (Free (Inventory vt next)) = do
  putStrLn $ "Sending request: inventory(" ++ vt ++ ")"
  resp <- post "http://localhost:3000" (cs $ "inventory(" ++ vt ++ ")" :: ByteString)
  runHttpSingle (next $ cs $ resp ^. responseBody)
runHttpSingle (Free (View next)) = do
  putStrLn "Sending request: view()"
  resp <- post "http://localhost:3000" (cs "view()" :: ByteString)
  runHttpSingle (next $ cs $ resp ^. responseBody)

-- Smart HTTP Batcher
runHttpBatch :: VehicleDSL a -> IO a
runHttpBatch = runHttpBatch' []

runHttpBatch' :: [String] -> VehicleDSL a -> IO a
runHttpBatch' acc (Pure a) = do
  unless (null acc) $ do
    putStrLn $ "Sending batch request: " ++ unlines acc
    _ <- post "http://localhost:3000" (cs $ unlines acc :: ByteString)
    return ()
  return a
runHttpBatch' acc (Free (AddVehicle vt model year mileage next)) =
  runHttpBatch' (acc ++ ["add_vehicle(" ++ vt ++ ", " ++ model ++ ", " ++ show year ++ ", " ++ show mileage ++ ")"]) next
runHttpBatch' acc (Free (PerformMaintenance vt mt duration next)) =
  runHttpBatch' (acc ++ ["perform_maintenance(" ++ vt ++ ", " ++ mt ++ ", " ++ duration ++ ")"]) next
runHttpBatch' acc (Free (SellVehicle vt model year price next)) =
  runHttpBatch' (acc ++ ["sell_vehicle(" ++ vt ++ ", " ++ model ++ ", " ++ show year ++ ", " ++ show price ++ ")"]) next
runHttpBatch' acc (Free (Inventory vt next)) = do
  unless (null acc) $ do
    putStrLn $ "Sending batch request: " ++ unlines acc
    _ <- post "http://localhost:3000" (cs $ unlines acc :: ByteString)
    return ()
  resp <- post "http://localhost:3000" (cs $ "inventory(" ++ vt ++ ")" :: ByteString)
  runHttpBatch' [] (next $ cs $ resp ^. responseBody)
runHttpBatch' acc (Free (View next)) = do
  unless (null acc) $ do
    putStrLn $ "Sending batch request: " ++ unlines acc
    _ <- post "http://localhost:3000" (cs $ unlines acc :: ByteString)
    return ()
  resp <- post "http://localhost:3000" (cs "view()" :: ByteString)
  runHttpBatch' [] (next $ cs $ resp ^. responseBody)

-- In-Memory
type InMemoryState = [(String, String, Int, Int)]

runInMemory :: VehicleDSL a -> State InMemoryState a
runInMemory (Pure a) = return a
runInMemory (Free (AddVehicle vt model year mileage next)) = do
  modify ((vt, model, year, mileage) :)
  runInMemory next
runInMemory (Free (PerformMaintenance _ _ _ next)) = runInMemory next
runInMemory (Free (SellVehicle vt model year _ next)) = do
  modify (filter (\(v, m, y, _) -> not (v == vt && m == model && y == year)))
  runInMemory next
runInMemory (Free (Inventory vt next)) = do
  currentState <- Control.Monad.State.get
  let filteredInventory = filter (\(v, _, _, _) -> v == vt) currentState
  runInMemory (next $ show filteredInventory)
runInMemory (Free (View next)) = do
  currentState <- Control.Monad.State.get
  runInMemory (next $ show currentState)

main :: IO ()
main = do
  args <- getArgs
  let program = do
        addVehicle "Car" "ModelX" 2020 10000
        performMaintenance "Car" "OilChange" "2 hours"
        _ <- inventory "Car"
        _ <- sellVehicle "Car" "ModelX" 2020 30000.0
        view

  case args of
    ["single"] -> do
      putStrLn "Running with HTTP single request per command:"
      _ <- runHttpSingle program
      return ()
    ["batch"] -> do
      putStrLn "Running with HTTP batch requests:"
      _ <- runHttpBatch program
      return ()
    ["memory"] -> do
      putStrLn "Running with in-memory interpreter for testing:"
      let (result, finalState) = runState (runInMemory program) []
      print result
      print finalState
    _ -> putStrLn "Usage: stack exec fp2024-four-client [single|batch|memory]"