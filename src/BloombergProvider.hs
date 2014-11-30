{-# LANGUAGE OverloadedStrings #-}
module BloombergProvider where

import           AuthorizationHelper
import           Finance.Blpapi.ElementFormatter
import qualified Finance.Blpapi.ElementParser    as P
import           Finance.Blpapi.Event
import           Finance.Blpapi.PrettyPrint
import           Finance.Blpapi.Session
import           Finance.Blpapi.SessionOptions
import           Finance.Blpapi.Types            as BT

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans             (liftIO)
import           Data.Monoid
import           Data.Text                       (Text)
import qualified Data.Text.Lazy                  as T
import qualified Data.Text.Lazy.IO               as TIO
import           Options.Applicative

data RefDataResponse = RefDataResponse {
    refSecurity :: Text,
    refFields   :: [(Text, String)] -- [name, value]
} deriving (Show)

data HisDataResponse = HisDataResponse {
    hisFields   :: [(String, String)] -- [name, value]
} deriving (Show)

notifyDone :: MVar [Double] -> Event -> Blpapi ()
notifyDone m e
    | eventType e == EventTypeResponse =
        case parseHis (messageData (eventContent e)) of
              Left err -> do liftIO $ print "SuperLeft"
                             liftIO $ putStrLn err
                             liftIO $ putMVar m []
              Right lst -> do liftIO $! putMVar m $ map (read.fst) $ hisFields lst 
                              liftIO $ print $ hisFields lst
    | isEventSessionTerminated e = liftIO $! putMVar m []
    | otherwise = return ()

defaultHandler :: MVar [Double] -> Event -> Blpapi ()
defaultHandler m e = do
  -- liftIO $ printEvent e
  parseRefData e
  notifyDone m e

printEvent :: Event -> IO ()
printEvent e = TIO.putStrLn $ pretty (eventContent e)

parseRefData :: Event -> Blpapi ()
parseRefData e
 | eventType e == EventTypeResponse
     || eventType e == EventTypePartialResponse = do
            case parseHis (messageData (eventContent e)) of
                  Left err -> do liftIO $ print "SuperLeft"
                                 liftIO $ putStrLn err
                  Right lst -> liftIO $ print $ hisFields lst
            return ()
 | otherwise = return ()

parseRef :: Element -> Either String [RefDataResponse]
parseRef el = do
    e <- P.getElement "HistoricalDataResponse" el
    secDatas <- P.getElement "securityData" e >>= P.getArrayElements
    foldM (\acc it -> do
       secName <- P.getElement "security" it >>= P.getValue
       fieldDatas <- P.getElement "fieldData" it >>= P.getAllElements
       fieldValues <- foldM (\ acc2 (fn, it2) -> do
          value <- P.getValue it2
          Right ((fn, value):acc2)) [] fieldDatas
       return (RefDataResponse secName fieldValues:acc)) [] secDatas

parseHis :: Element -> Either String HisDataResponse
parseHis el = do
    e <- P.getElement "HistoricalDataResponse" el
    secData <- P.getElement "securityData" e
    eidData <- P.getElement "eidData" secData --  >>= P.getArrayElements
    fields  <- P.getElement "fieldData" secData >>= P.getArrayElements
    vals <- mapM (\it -> do price <- P.getElement "PX_LAST" it >>= P.getValue
                            date  <- P.getElement "date" it >>= P.getValue
                            return (price :: String, date :: String)
                          ) fields
    return (HisDataResponse vals)

createRefDataRequest :: Text -> Blpapi ()
createRefDataRequest ticker = do
    ser <- openService "//blp/refdata" >>= throwOnError
    -- req <- createRequest ser "ReferenceDataRequest" >>= throwOnError
    -- formatRequest req $! do
    --     formatSubElement "returnEids" $
    --         setValue (BT.BlpBool True)
    --     formatSubElement "fields" $ do
    --         appendValue (BT.BlpString "ASK")
    --         appendValue (BT.BlpString "BID")
    --     formatSubElement "securities" $ do
    --         appendValue (BT.BlpString "IBM US Equity")
    --         appendValue (BT.BlpString "GOOG US Equity")

    req <- createRequest ser "HistoricalDataRequest" >>= throwOnError
    formatRequest req $! do
        formatSubElement "securities" $
            appendValue (BT.BlpString ticker)
        formatSubElement "fields" $ do
            appendValue (BT.BlpString "PX_LAST")
        formatSubElement "periodicityAdjustment"  $
            setValue (BT.BlpString "ACTUAL")
        formatSubElement "periodicitySelection"  $
            setValue (BT.BlpString "MONTHLY")
        formatSubElement "startDate"  $
            setValue (BT.BlpString "20060101")
        formatSubElement "endDate"  $
            setValue (BT.BlpString "20121231")
        formatSubElement "maxDataPoints"  $
            setValue (BT.BlpInt32 100)

    -- Service refDataService = session.getService("//blp/refdata");
    -- Request request = refDataService.createRequest("HistoricalDataRequest");
    -- request.getElement("securities").appendValue("IBM US Equity");
    -- request.getElement("securities").appendValue("MSFT US Equity");
    -- request.getElement("fields").appendValue("PX_LAST");
    -- request.getElement("fields").appendValue("OPEN");
    -- request.set("periodicityAdjustment", "ACTUAL");
    -- request.set("periodicitySelection", "MONTHLY");
    -- request.set("startDate", "20060101");
    -- request.set("endDate", "20061231");
    -- request.set("maxDataPoints", 100);
    -- e <- getElementFromRequest req
    -- liftIO $ prettyPrint e
    sendRequest req Nothing

data CmdOptions = CmdOptions {
  cmdIp   :: String,
  cmdPort :: Int
} deriving (Show)

setupBlpapi :: (MVar [Double], MVar [Double]) -> CmdOptions -> Blpapi ()
setupBlpapi (m1, m2) c = do
  -- m1 <- liftIO newEmptyMVar
  createSession
     (defaultSessionOptions
          {serverAddresses = [ServerAddress (cmdIp c) (cmdPort c)]})
     (defaultHandler m1) >>= throwOnError
  createRefDataRequest "IBM US Equity"

  res1 <- liftIO $! readMVar m1

  -- m2 <- liftIO newEmptyMVar
  createSession
     (defaultSessionOptions
          {serverAddresses = [ServerAddress (cmdIp c) (cmdPort c)]})
     (defaultHandler m2) >>= throwOnError
  createRefDataRequest "GOOG US Equity"

  res2 <- liftIO $! readMVar m2

  return ()

cmdLineParser :: Parser CmdOptions
cmdLineParser = CmdOptions <$> ipParser <*> portParser
  where
    portParser = option $
        value 8194
        <> long "port"
        <> metavar "Port"
        <> help "server port"
    ipParser = strOption $
        value "localhost"
        <> long "ip"
        <> metavar "IP"
        <> help "server name or IP"

runBloombergRequest :: (MVar [Double], MVar [Double]) -> IO ()
runBloombergRequest mvars = do
  cmd <- execParser (info (helper <*> cmdLineParser) mempty)
  runBlpapi $ setupBlpapi mvars cmd
