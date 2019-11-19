{-# language
    DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , GeneralizedNewtypeDeriving
  , OverloadedStrings
  , RecordWildCards
  , ScopedTypeVariables
  , StrictData
  , TypeApplications
  , TypeOperators
  #-}

{-# options_ghc -Wall #-}

-- | NxLog type and parser as per https://nxlog.co/documentation/nxlog-user-guide/im_msvistalog.html#im_msvistalog_fields
module NxLog
  ( -- * NxLog type
    NxLog(..)
  , eventTimeFormat
  ) where

import Chronos (Datetime, DatetimeFormat(..))
import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import Data.Word (Word16)
import GHC.Generics (Generic)
import Net.IPv4 (IPv4)
import Prelude hiding (maybe)
import qualified Chronos

-- | https://nxlog.co/documentation/nxlog-user-guide/im_msvistalog.html#im_msvistalog_fields
data NxLog = NxLog
  { _RequestId :: Maybe Text,
    _ERROREVTUNRESOLVED :: Maybe Bool,
    _Status :: Maybe Text,
    _TicketOptions :: Maybe Text,
    _RequiredSize :: Maybe Text,
    _SubjectLogonId :: Maybe Text,
    _Library :: Maybe Text,
    _TemplateName :: Maybe Text,
    _Context :: Maybe Text,
    _TerminalSessionId :: Maybe Text,
    _ProcessCreationTime :: Maybe Text,
    _Hostname :: Text,
    _TargetUserName :: Maybe Text,
    _IpAddress :: Maybe IPv4,
    _TargetSid :: Maybe Text,
    _KeyType :: Maybe Text,
    _Param11 :: Maybe Text,
    _AdditionalInfo :: Maybe Text,
    _Image :: Maybe Text,
    _ReadOperation :: Maybe Text,
    _ObjectName :: Maybe Text,
    _OldTime :: Maybe Text,
    _ImagePath :: Maybe Text,
    _LogonGuid :: Maybe Text,
    _SourceName :: Text,
    _CA :: Maybe Text,
    _ObjectType :: Maybe Text,
    _EventCountTotal :: Maybe Text,
    _Service :: Maybe Text,
    _PrivilegeList :: Maybe Text,
    _LogonType :: Maybe Text,
    _Operation :: Maybe Text,
    _PackageName :: Maybe Text,
    _GroupMembership :: Maybe Text,
    _Param7 :: Maybe Text,
    _CurrentDirectory :: Maybe Text,
    _Category :: Maybe Text,
    _TargetFilename :: Maybe Text,
    _ActivityID :: Maybe Text,
    _LogonId :: Maybe Text,
    _Domain :: Maybe Text,
    _ObjectServer :: Maybe Text,
    _ServerID :: Maybe Text,
    _KeyName :: Maybe Text,
    _AlgorithmName :: Maybe Text,
    _OriginalFileName :: Maybe Text,
    _TargetLogonGuid :: Maybe Text,
    _ParentImage :: Maybe Text,
    _Severity :: Text,
    _SupportInfo2 :: Maybe Text,
    _Channel :: Text,
    _ExtensionName :: Maybe Text,
    _AccountName :: Maybe Text,
    _TargetUserSid :: Maybe Text,
    _Param9 :: Maybe Text,
    _ProcessID :: Int,
    _IntegrityLevel :: Maybe Text,
    _TargetLogonId :: Maybe Text,
    _User :: Maybe Text,
    _Param6 :: Maybe Text,
    _EventTime :: Datetime,
    _ErrorMsg :: Maybe Text,
    _Hashes :: Maybe Text,
    _KeyFilePath :: Maybe Text,
    _ProcessGuid :: Maybe Text,
    _PreviousTime :: Maybe Text,
    _FileVersion :: Maybe Text,
    _EventType :: Text,
    _UserID :: Maybe Text,
    _ProcessName :: Maybe Text,
    _LogonProcessName :: Maybe Text,
    _ExtensionId :: Maybe Text,
    _Param3 :: Maybe Text,
    _Param8 :: Maybe Text,
    _PreAuthType :: Maybe Text,
    _TargetInfo :: Maybe Text,
    _ClientProcessId :: Maybe Text,
    _ReturnCode :: Maybe Text,
    _HandleId :: Maybe Text,
    _Company :: Maybe Text,
    _SourceModuleType :: Text,
    _ProcessingTimeInMilliseconds :: Maybe Text,
    _UserSid :: Maybe Text,
    _UtcTime :: Maybe Text,
    _SubjectUserName :: Maybe Text,
    _Keywords :: Int,
    _Param5 :: Maybe Text,
    _Opcode :: Maybe Text,
    _Version :: Maybe Int,
    _TSId :: Maybe Text,
    _ServiceName :: Maybe Text,
    _ParentProcessId :: Maybe Text,
    _ObjId :: Maybe Text,
    _BufferSize :: Maybe Text,
    _ErrorCode :: Maybe Text,
    _ServiceType :: Maybe Text,
    _OperationType :: Maybe Text,
    _ParentCommandLine :: Maybe Text,
    _Param2 :: Maybe Text,
    _Task :: Int,
    _ProcessingMode :: Maybe Text,
    _ParentProcessGuid :: Maybe Text,
    _AccessMask :: Maybe Text,
    _SeverityValue :: Int,
    _SourceModuleName :: Text,
    _Type :: Maybe Text,
    _TimeSource :: Maybe Text,
    _IpPort :: Maybe Word16,
    _SupportInfo1 :: Maybe Text,
    _DCName :: Maybe Text,
    _StartType :: Maybe Text,
    _TicketEncryptionType :: Maybe Text,
    _Product :: Maybe Text,
    _Message :: Text,
    _ServiceSid :: Maybe Text,
    _CallerProcessId :: Maybe Text,
    _ThreadID :: Int,
    _SubjectDomainName :: Maybe Text,
    _Workstation :: Maybe Text,
    _Param4 :: Maybe Text,
    _SubjectUserSid :: Maybe Text,
    _TargetServerName :: Maybe Text,
    _Param1 :: Maybe Text,
    _ErrorDescription :: Maybe Text,
    _ProviderGuid :: Maybe Text,
    _Description :: Maybe Text,
    _Param10 :: Maybe Text,
    _EventReceivedTime :: Text,
    _RecordNumber :: Int,
    _TargetName :: Maybe Text,
    _ProviderName :: Maybe Text,
    _CreationUtcTime :: Maybe Text,
    _TargetDomainName :: Maybe Text,
    _AccessList :: Maybe Text,
    _NewTime :: Maybe Text,
    _CallerProcessName :: Maybe Text,
    _EventIdx :: Maybe Text,
    _EventID :: Int,
    _Properties :: Maybe Text,
    _TransmittedServices :: Maybe Text,
    _AccountType :: Maybe Text,
    _CountOfCredentialsReturned :: Maybe Text,
    _OpcodeValue :: Maybe Int,
    _CommandLine :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON NxLog where
  parseJSON (Object v) = NxLog
    <$> maybe v "RequestId"
    <*> maybe v "ERROR_EVT_UNRESOLVED"
    <*> maybe v "Status"
    <*> maybe v "TicketOptions"
    <*> maybe v "RequiredSize"
    <*> maybe v "SubjectLogonId"
    <*> maybe v "Library"
    <*> maybe v "TemplateName"
    <*> maybe v "Context"
    <*> maybe v "TerminalSessionId"
    <*> maybe v "ProcessCreationTime"
    <*> v .:  "Hostname"
    <*> maybe v "TargetUserName"
    <*> maybe v "IpAddress"
    <*> maybe v "TargetSid"
    <*> maybe v "KeyType"
    <*> maybe v "param11"
    <*> maybe v "AdditionalInfo"
    <*> maybe v "Image"
    <*> maybe v "ReadOperation"
    <*> maybe v "ObjectName"
    <*> maybe v "OldTime"
    <*> maybe v "ImagePath"
    <*> maybe v "LogonGuid"
    <*> v .:  "SourceName"
    <*> maybe v "CA"
    <*> maybe v "ObjectType"
    <*> maybe v "EventCountTotal"
    <*> maybe v "Service"
    <*> maybe v "PrivilegeList"
    <*> maybe v "LogonType"
    <*> maybe v "Operation"
    <*> maybe v "PackageName"
    <*> maybe v "GroupMembership"
    <*> maybe v "param7"
    <*> maybe v "CurrentDirectory"
    <*> maybe v "Category"
    <*> maybe v "TargetFilename"
    <*> maybe v "ActivityID"
    <*> maybe v "LogonId"
    <*> maybe v "Domain"
    <*> maybe v "ObjectServer"
    <*> maybe v "ServerID"
    <*> maybe v "KeyName"
    <*> maybe v "AlgorithmName"
    <*> maybe v "OriginalFileName"
    <*> maybe v "TargetLogonGuid"
    <*> maybe v "ParentImage"
    <*> v .:  "Severity"
    <*> maybe v "SupportInfo2"
    <*> v .:  "Channel"
    <*> maybe v "ExtensionName"
    <*> maybe v "AccountName"
    <*> maybe v "TargetUserSid"
    <*> maybe v "param9"
    <*> v .:  "ProcessID"
    <*> maybe v "IntegrityLevel"
    <*> maybe v "TargetLogonId"
    <*> maybe v "User"
    <*> maybe v "param6"
    <*> eventTime v
    <*> maybe v "ErrorMsg"
    <*> maybe v "Hashes"
    <*> maybe v "KeyFilePath"
    <*> maybe v "ProcessGuid"
    <*> maybe v "PreviousTime"
    <*> maybe v "FileVersion"
    <*> v .:  "EventType"
    <*> maybe v "UserID"
    <*> maybe v "ProcessName"
    <*> maybe v "LogonProcessName"
    <*> maybe v "ExtensionId"
    <*> maybe v "param3"
    <*> maybe v "param8"
    <*> maybe v "PreAuthType"
    <*> maybe v "TargetInfo"
    <*> maybe v "ClientProcessId"
    <*> maybe v "ReturnCode"
    <*> maybe v "HandleId"
    <*> maybe v "Company"
    <*> v .:  "SourceModuleType"
    <*> maybe v "ProcessingTimeInMilliseconds"
    <*> maybe v "UserSid"
    <*> maybe v "UtcTime"
    <*> maybe v "SubjectUserName"
    <*> v .:  "Keywords"
    <*> maybe v "param5"
    <*> maybe v "Opcode"
    <*> maybe v "Version"
    <*> maybe v "TSId"
    <*> maybe v "ServiceName"
    <*> maybe v "ParentProcessId"
    <*> maybe v "ObjId"
    <*> maybe v "BufferSize"
    <*> maybe v "ErrorCode"
    <*> maybe v "ServiceType"
    <*> maybe v "OperationType"
    <*> maybe v "ParentCommandLine"
    <*> maybe v "param2"
    <*> v .:  "Task"
    <*> maybe v "ProcessingMode"
    <*> maybe v "ParentProcessGuid"
    <*> maybe v "AccessMask"
    <*> v .:  "SeverityValue"
    <*> v .:  "SourceModuleName"
    <*> maybe v "Type"
    <*> maybe v "TimeSource"
    <*> maybe v "IpPort"
    <*> maybe v "SupportInfo1"
    <*> maybe v "DCName"
    <*> maybe v "StartType"
    <*> maybe v "TicketEncryptionType"
    <*> maybe v "Product"
    <*> v .:  "Message"
    <*> maybe v "ServiceSid"
    <*> maybe v "CallerProcessId"
    <*> v .:  "ThreadID"
    <*> maybe v "SubjectDomainName"
    <*> maybe v "Workstation"
    <*> maybe v "param4"
    <*> maybe v "SubjectUserSid"
    <*> maybe v "TargetServerName"
    <*> maybe v "param1"
    <*> maybe v "ErrorDescription"
    <*> maybe v "ProviderGuid"
    <*> maybe v "Description"
    <*> maybe v "param10"
    <*> v .:  "EventReceivedTime"
    <*> v .:  "RecordNumber"
    <*> maybe v "TargetName"
    <*> maybe v "ProviderName"
    <*> maybe v "CreationUtcTime"
    <*> maybe v "TargetDomainName"
    <*> maybe v "AccessList"
    <*> maybe v "NewTime"
    <*> maybe v "CallerProcessName"
    <*> maybe v "EventIdx"
    <*> v .:  "EventID"
    <*> maybe v "Properties"
    <*> maybe v "TransmittedServices"
    <*> maybe v "AccountType"
    <*> maybe v "CountOfCredentialsReturned"
    <*> maybe v "OpcodeValue"
    <*> maybe v "CommandLine"
  parseJSON _          = mzero

-- | The format taken by the 'EventTime' field.
eventTimeFormat :: DatetimeFormat
eventTimeFormat = DatetimeFormat (Just '-') (Just ' ') (Just ':')

maybe :: FromJSON a => Object -> Text -> Parser (Maybe a)
maybe o n = o .: n <|> pure Nothing
{-# inlineable maybe #-}

eventTime :: Object -> Parser Datetime
eventTime o = do
  etime <- maybe o "EventTime"
  case (Chronos.decode_YmdHMS eventTimeFormat =<< etime) of
    Nothing -> fail "Failed to parse EventTime"
    Just dt -> pure dt

{-
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
-}
