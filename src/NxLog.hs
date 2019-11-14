{-# language
    DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , GeneralizedNewtypeDeriving
  , OverloadedStrings
  , RecordWildCards
  , StrictData
  #-}

{-# language TypeApplications #-}

-- | NxLog type and parser as per https://nxlog.co/documentation/nxlog-user-guide/im_msvistalog.html#im_msvistalog_fields
module NxLog
  ( -- * NxLog type
    NxLog(..)
  , eventTimeFormat

    -- * Re-exports
  , Xeno.XenoException(..)

    -- * Testing
  , testXmlParser
  , test
  ) where

import Chronos (Datetime, DatetimeFormat(..))
import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import Data.HashMap.Strict (HashMap)
import Data.Text
import GHC.Generics
import Prelude hiding (maybe)
import qualified Chronos
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Xeno.DOM as Xeno
import qualified Xeno.Types as Xeno

-- | https://nxlog.co/documentation/nxlog-user-guide/im_msvistalog.html#im_msvistalog_fields
data NxLog = NxLog
  { _AccountName :: Maybe Text
  , _AccountType :: Maybe Text
  , _ActivityId :: Maybe Text
  , _Category :: Maybe Text
  , _Channel :: Maybe Text
  , _Domain :: Maybe Text
  , _EventId :: Int
  , _EventTime :: Maybe Datetime
  , _EventType :: Maybe Text
  , _EventXml :: Either Xeno.XenoException (HashMap Text Text)
  , _ExecutionProcessId :: Maybe Int
  , _Hostname :: Maybe Text
  , _Keywords :: Maybe Text
  , _LogonId :: Maybe Text
  , _LogonProcessName :: Maybe Text
  , _LogonType :: Maybe Text
  , _Message :: Maybe Text
  , _Opcode :: Maybe Text
  , _OpcodeValue :: Maybe Int
  , _ProviderGuid :: Maybe Text
  , _RecordNumber :: Maybe Int
  , _Severity :: Maybe Text
  , _SeverityValue :: Maybe Int
  , _SourceName :: Maybe Text
  , _TaskValue :: Maybe Int
  , _ThreadId :: Maybe Int
  , _UserId :: Maybe Text
  , _Version :: Maybe Int
  }
  deriving stock (Show, Generic)

-- | The format taken by the 'EventTime' field.
eventTimeFormat :: DatetimeFormat
eventTimeFormat = DatetimeFormat (Just '-') (Just ' ') (Just ':')

instance FromJSON NxLog where
  parseJSON = withObject "NxLog" $ \m -> do
    _AccountName <- maybe m "AccountName" <|> maybe m "Account Name"
    _AccountType <- maybe m "AccountType" <|> maybe m "Account Type"
    _ActivityId <- maybe m "ActivityID"
    _Category <- maybe m "Category"
    _Channel <- maybe m "Channel"
    _Domain <- maybe m "Domain"
    _EventId <- m .: "EventID"
    _EventTime <- do
      etime <- maybe m "EventTime"
      pure $ Chronos.decode_YmdHMS eventTimeFormat =<< etime
    _EventType <- maybe m "EventType"
    _EventXml <- eventXml m
    _ExecutionProcessId <- maybe m "ExecutionProcessID"
    _Hostname <- maybe m "Hostname"
    _Keywords <- maybe m "Keywords"
    _LogonId <- maybe m "LogonID"
    _LogonProcessName <- maybe m "LogonProcessName"
    _LogonType <- maybe m "LogonType"
    _Message <- maybe m "Message"
    _Opcode <- maybe m "Opcode"
    _OpcodeValue <- maybe m "OpcodeValue"
    _ProviderGuid <- maybe m "ProviderGuid"
    _RecordNumber <- maybe m "RecordNumber"
    _Severity <- maybe m "Severity"
    _SeverityValue <- maybe m "SeverityValue"
    _SourceName <- maybe m "SourceName"
    _TaskValue <- maybe m "TaskValue"
    _ThreadId <- maybe m "ThreadID"
    _UserId <- maybe m "UserID"
    _Version <- maybe m "Version"
    pure NxLog{..}

maybe :: FromJSON a => Object -> Text -> Parser (Maybe a)
maybe o n = o .: n <|> pure Nothing
{-# inlineable maybe #-}

eventXml :: Object -> Parser (Either Xeno.XenoException (HashMap Text Text))
eventXml o = do
  mxml <- maybe o "EventXML"
  case mxml of
    Nothing -> pure (Right mempty)
    Just xml -> pure $ case Xeno.parse (TE.encodeUtf8 xml) of
      Left err -> Left err
      Right node -> Right $ nodeToEventXml node

nodeToEventXml :: Xeno.Node -> HashMap Text Text
nodeToEventXml node =
  let contents = Xeno.contents node
  in flip foldMap contents $ \c -> case c of
       Xeno.Element n -> nodeToNameValue n
       _ -> mempty

nodeToNameValue :: Xeno.Node -> HashMap Text Text
nodeToNameValue n = case (Xeno.attributes n, Xeno.contents n) of
  ([("Name",name)],[Xeno.Text value]) -> if value == "-"
    then mempty
    else HM.singleton (TE.decodeUtf8 name) (TE.decodeUtf8 value)
  _ -> mempty

testXmlParser :: Text -> HashMap Text Text
testXmlParser b = case Xeno.parse (TE.encodeUtf8 b) of
  Left err -> error "bad xml lol"
  Right node -> nodeToEventXml node

test :: IO ()
test = do
  x <- TIO.readFile "/home/chessai/development/allsight/lib/allsight-core/nxlog.json"
  print $ _EventXml <$> decodeStrict @NxLog (TE.encodeUtf8 x)
