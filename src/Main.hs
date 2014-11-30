{-# LANGUAGE Arrows #-}
import Control.Monad.IfElse
import FRP.Yampa as Yampa

import Game
import Display
import Input
import Graphics.UI.Extra.SDL
import BloombergProvider
import Finance.Blpapi.Session
import Control.Concurrent

-- TODO: Use MaybeT or ErrorT to report errors
main :: IO ()
main = do

  initializeDisplay
  ibmStockMVar <- newEmptyMVar 
  -- googStockMVar  <- newEmptyMVar 

  print "calling bloomberg"
  forkIO $ runBloombergRequest "IBM US Equity" ibmStockMVar
  -- forkIO $ runBloombergRequest "GOOG US Equity" googStockMVar

  print "Waiting for bloomberg"
  ibmStock <- takeMVar ibmStockMVar
  print "Got ibm stock"
  -- googStock <- takeMVar googStockMVar
  -- print "Got goog stock"

  timeRef       <- initializeTimeRef
  controllerRef <- initializeInputDevices
  res           <- loadResources

  awhen res $ \res' -> do
    reactimate (initGraphs >> senseInput controllerRef)
               (\_ -> do
                  -- Get clock and new input
                  dtSecs <- fmap milisecsToSecs $ senseTimeRef timeRef
                  mInput <- senseInput controllerRef
                  return (dtSecs, Just mInput)
               )
               (\_ e -> render res' e >> return False)
               (wholeGame $ zip ibmStock ibmStock)
 
