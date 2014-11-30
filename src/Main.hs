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
  msStockMVar  <- newEmptyMVar 

  print "calling bloomberg"
  forkIO $ runBloombergRequest (ibmStockMVar, msStockMVar) -- obtainIBMStock

  print "Waiting for bloomberg"
  ibmStock <- takeMVar ibmStockMVar
  print "Got ibm stock"
  -- msStock <- takeMVar msStockMVar
  print "Got msft stock"

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
 

-- ibmStock = [(100.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5)]
-- msStock  = [(100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5), (150.5), (100.5)]
