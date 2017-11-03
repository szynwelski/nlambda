module Nominal.Formula.SmtLogger where

import System.Log.Handler (LogHandler)
import System.Log.Logger (Priority(ERROR, INFO), infoM, setLevel, updateGlobalLogger, addHandler, removeHandler, setHandlers)

smtLogger :: String
smtLogger = "smt"

smtInfo :: String -> IO ()
smtInfo = infoM smtLogger

showSmtInfo :: IO ()
showSmtInfo = updateGlobalLogger smtLogger (setLevel INFO)

hideSmtInfo :: IO ()
hideSmtInfo = updateGlobalLogger smtLogger (setLevel ERROR)

addSmtHandler :: LogHandler a => a -> IO ()
addSmtHandler = updateGlobalLogger smtLogger . addHandler

removeSmtHandler :: IO ()
removeSmtHandler = updateGlobalLogger smtLogger removeHandler

setSmtHandlers :: LogHandler a => [a] -> IO ()
setSmtHandlers = updateGlobalLogger smtLogger . setHandlers
