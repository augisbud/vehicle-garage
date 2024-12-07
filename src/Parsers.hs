{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parsers
  ( Query(..),
    VehicleType(..),
    MaintenanceType(..),
    Duration(..),
    parseTask,
    parseTaskList,
    skipSpaces,
    parseLiteral,
    parseChar,
    parseString,
    parseInt,
    char,
    Parser,
    parse
  )
where

import Control.Applicative (Alternative (empty), (<|>), optional)
import Data.Char (isDigit)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Class (lift)

-- Data Types for Queries and State
data Query
  = AddVehicle VehicleType String Int Int -- VehicleType, Model, Year, Mileage
  | PerformMaintenance VehicleType MaintenanceType Duration -- VehicleType, MaintenanceType, Duration
  | SellVehicle VehicleType String Int Double -- VehicleType, Model, Year, Price
  | Inventory VehicleType -- VehicleType
  | View
  | Sequence [Query] -- Sequence of queries
  deriving (Eq, Show)

data VehicleType = Car | Truck | Motorcycle | SUV
  deriving (Eq, Show)

data MaintenanceType = OilChange | TireRotation | BrakeInspection | EngineTuneUp
  deriving (Eq, Show)

data Duration = Hours Int | Days Int
  deriving (Eq, Show)

type Parser a = ExceptT String (State String) a

parse :: Parser a -> String -> (Either String a, String)
parse parser = runState (runExceptT parser)

-- <task_list> ::= <task> | <task> ";" <task_list>
parseTaskList :: Parser [Query]
parseTaskList = do
  firstQuery <- parseTask
  rest <- optional (char ';' >> parseTaskList)
  return $ case rest of
    Just otherQueries -> firstQuery : otherQueries
    Nothing -> [firstQuery]

-- <task> ::= <add_vehicle> | <perform_maintenance> | <sell_vehicle> | <inventory> | <view>
parseTask :: Parser Query
parseTask =
  parseAddVehicle <|>
  parsePerformMaintenance <|>
  parseSellVehicle <|>
  parseInventory <|>
  parseView <|>
  parseVehicleGarage

-- <add_vehicle> ::= "add_vehicle" "(" <vehicle_type> "," <model> "," <year> "," <mileage> ")"
parseAddVehicle :: Parser Query
parseAddVehicle = do
  _ <- parseLiteral "add_vehicle"
  _ <- parseChar '('
  vType <- parseVehicleType
  _ <- parseChar ','
  model <- parseString
  _ <- parseChar ','
  year <- parseInt
  _ <- parseChar ','
  mileage <- parseMileage
  _ <- parseChar ')'
  return $ AddVehicle vType model year mileage

parsePerformMaintenance :: Parser Query
parsePerformMaintenance = do
  _ <- parseLiteral "perform_maintenance"
  _ <- parseChar '('
  vType <- parseVehicleType
  _ <- parseChar ','
  mType <- parseMaintenanceType
  _ <- parseChar ','
  duration <- parseDuration
  _ <- parseChar ')'
  return $ PerformMaintenance vType mType duration

-- <sell_vehicle> ::= "sell_vehicle" "(" <vehicle_type> "," <model> "," <year> "," <price> ")"
parseSellVehicle :: Parser Query
parseSellVehicle = do
  _ <- parseLiteral "sell_vehicle"
  _ <- parseChar '('
  vType <- parseVehicleType
  _ <- parseChar ','
  model <- parseString
  _ <- parseChar ','
  year <- parseInt
  _ <- parseChar ','
  price <- parsePrice
  _ <- parseChar ')'
  return $ SellVehicle vType model year price

-- <inventory> ::= "inventory" "(" <vehicle_type> ")"
parseInventory :: Parser Query
parseInventory = do
  _ <- parseLiteral "inventory"
  _ <- parseChar '('
  vType <- parseVehicleType
  _ <- parseChar ')'
  return $ Inventory vType

-- <view> ::= "view" "(" ")"
parseView :: Parser Query
parseView = do
  _ <- parseLiteral "view"
  _ <- parseChar '('
  _ <- parseChar ')'
  return View

-- <vehicle_garage> ::= "vehicle_garage" "(" <task_list> ")"
parseVehicleGarage :: Parser Query
parseVehicleGarage = do
  _ <- parseLiteral "vehicle_garage"
  _ <- parseChar '('
  queryList <- parseTaskList
  _ <- parseChar ')'
  return $ Sequence queryList

-- <vehicle_type> ::= "Car" | "Truck" | "Motorcycle" | "SUV"
parseVehicleType :: Parser VehicleType
parseVehicleType =
  (parseLiteral "Car" >> return Car) <|>
  (parseLiteral "Truck" >> return Truck) <|>
  (parseLiteral "Motorcycle" >> return Motorcycle) <|>
  (parseLiteral "SUV" >> return SUV)

-- <mileage> parser for mileage input (e.g., "123km")
parseMileage :: Parser Int
parseMileage = do
  num <- parseInt
  _ <- parseLiteral "km"
  return num

-- <maintenance_type> ::= "OilChange" | "TireRotation" | "BrakeInspection" | "EngineTuneUp"
parseMaintenanceType :: Parser MaintenanceType
parseMaintenanceType =
  (parseLiteral "OilChange" >> return OilChange) <|>
  (parseLiteral "TireRotation" >> return TireRotation) <|>
  (parseLiteral "BrakeInspection" >> return BrakeInspection) <|>
  (parseLiteral "EngineTuneUp" >> return EngineTuneUp)

-- <duration> ::= <number> "hours" | <number> "days"
parseDuration :: Parser Duration
parseDuration = do
  num <- parseInt
  unit <- parseDurationUnit
  return (unit num)

-- Helper function to parse duration units
parseDurationUnit :: Parser (Int -> Duration)
parseDurationUnit = do
  unit <- parseString
  case unit of
    "days" -> return Days
    "hours" -> return Hours
    _ -> empty

-- <price> ::= <number> "." <number>
parsePrice :: Parser Double
parsePrice = do
  wholePart <- parseInt
  _ <- parseChar '.'
  fractionalPart <- parseInt
  return (read (show wholePart ++ "." ++ show fractionalPart))

sat :: (Char -> Bool) -> Parser Char
sat p = do
  input <- lift get
  case input of
    [] -> throwError "Empty String"
    (x:xs) -> if p x
              then lift (put xs) >> return x
              else throwError $ "Could not recognize: " ++ [x]

char :: Char -> Parser Char
char c = sat (== c)

parseChar :: Char -> Parser Char
parseChar = char

skipSpaces :: String -> String
skipSpaces = dropWhile (== ' ')

skipSpaces' :: Parser ()
skipSpaces' = lift (modify (dropWhile (== ' ')))

parseLiteral :: String -> Parser String
parseLiteral [] = return []
parseLiteral (x : xs) = do
  skipSpaces'
  _ <- parseChar x
  parseLiteral xs

parseString :: Parser String
parseString = do
  input <- lift get
  let input' = skipSpaces input
  if null input'
    then return ""
    else if head input' == '"'
         then parseQuotedString (tail input')
         else let (str, rest) = span (\c -> c /= ' ' && c /= ',' && c /= '(' && c /= ')') input'
              in lift (put rest) >> return str
  where
    parseQuotedString [] = throwError "Unexpected end of input in quoted string"
    parseQuotedString ('"' : rest) = lift (put rest) >> return ""
    parseQuotedString (x : rest) = do
      str <- parseQuotedString rest
      return (x : str)

parseInt :: Parser Int
parseInt = do
  input <- lift get
  let (digits, rest) = span isDigit (skipSpaces input)
  if null digits
    then throwError "Expected an integer"
    else do
      lift (put rest)
      return (read digits)