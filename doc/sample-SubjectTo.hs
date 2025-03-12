module Main where

-- sample application code:
-- §30 Liquor taxes. Subject to §31 and §32, the retail sale of liquor shall be taxed at 18%.
-- §31 Sunday wine. Subject to §32, on Sundays, the retail tax rate of liquor shall be increased by one-third of the existing rate. For example, a 12% rate shall increase by 4% to 16%.
-- §32 Public Holidays. On public holidays, the retail sale of liquor shall be subject to a cap of $36. For example, if a purchase of liquor were priced at $300, and taxed at 18%, the cap would reduce the tax payable from $54 to $36.

import Data.Map (Map, fromList, insert, findWithDefault)
import Control.Monad.RWS
import System.Environment (getArgs)

type MyRWS a = RWS () [String] (Map String Float) a

type F = Float -> Float
type InnerF = F
type OuterF = F

type Operation = InnerF -> MyRWS OuterF

baseComputation :: Operation
baseComputation f = do
  env <- get
  let taxRate = findWithDefault 0 "taxRate" env
  tell ["Applying tax rate: " ++ show taxRate]
  return (f . (* taxRate))

section30 :: Operation
section30 f = do
  modify (insert "taxRate" 0.18)
  tell ["Section 30 applied: tax rate set to 18%"]
  return f

section31 :: Operation
section31 f = do
  env <- get
  let dow = findWithDefault 0 "dayOfWeek" env
  if dow == 7
    then do
      let taxRate = findWithDefault 0 "taxRate" env
      let newTaxRate = taxRate + taxRate / 3
      modify (insert "taxRate" newTaxRate)
      tell ["Section 31 applied: tax rate increased by one-third on Sunday from " ++ show taxRate ++ " to " ++ show newTaxRate]
    else tell ["Section 31 not applied: not a Sunday"]
  return f

section32 :: Operation
section32 f = do
  env <- get
  let isHoliday = findWithDefault 0 "isHoliday" env
  if isHoliday == 1
    then do
      tell ["Section 32 applied: tax capped at $36 on public holidays"]
      return (min 36 . f)
    else do
      tell ["Section 32 not applied: not a public holiday"]
      return f

main :: IO ()
main = do
  args <- getArgs
  let price = if null args then 100 else read (head args) :: Float
  let myenv = fromList [("dayOfWeek", 7), ("isHoliday", 1)]

  let (result, loglines) = evalRWS ( 
                                     section30 id >>=
                                     section31 >>=
                                     section32 >>=
                                     baseComputation
                                    ) () myenv
  mapM_ putStrLn (loglines)
  putStrLn $ "resulting tax: " ++ show (result price)
