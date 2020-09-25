#!/usr/bin/env run-script
module Application.Script.UpdateLastDrank where

import Application.Script.Prelude

import Generated.Types

import Data.Time.Clock
import Data.Time.Calendar

--import Web.Controller.Prelude

--atm last drank is the day after it appears.
--
--date :: IO (Integer,Int,Int) -- :: (year,month,day)
--date = getCurrentTime >>= return . toGregorian . utctDay

run :: Script
run = do
--    let today = date
--    q <- query @ Coffee
--          |> orderBy #lastDrank
--          |> fetch
--          |> set #lastDrank today
--          |> updateRecord 
    putStrLn "Updated Latest Record Entry"
