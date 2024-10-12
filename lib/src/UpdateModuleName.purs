module UpdateModuleName
  ( updateModuleName
  , UpdateModuleNameResult(..)
  , updateModuleNameResult_codec
  )
  where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NonEmptyString
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import ModuleName (ModuleName, printModuleName)
import Data.Argonaut.Core as J
import Data.Codec.Argonaut as CA
import Data.Either (Either)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Variant as CAV
import Data.Either (Either(..))
import Data.Profunctor (dimap)
import Data.Variant as V
import Type.Proxy (Proxy(..))
import Data.Tuple (Tuple(..))
import Data.Codec.Argonaut.Sum as CA

replaceModuleName_string :: String -> NonEmptyString -> String
replaceModuleName_string fileContent newModuleName =
  Regex.replace moduleRegexWithGroup ("module " <> NonEmptyString.toString newModuleName) fileContent

moduleRegexWithGroup :: Regex
moduleRegexWithGroup = unsafeRegex """^\s*module\s*([^\s\(\n]*)""" noFlags

matchCurrentModuleName :: String -> Maybe String
matchCurrentModuleName fileContent = do
  (matches :: NonEmptyArray (Maybe String)) <- Regex.match moduleRegexWithGroup fileContent
  join $ NonEmptyArray.index matches 1

data UpdateModuleNameResult
  = UpdateModuleNameResult_Updated { from :: String, newFileContent :: String }
  | UpdateModuleNameResult_Appended { newFileContent :: String }
  | UpdateModuleNameResult_AlreadyUpToDate

derive instance Generic UpdateModuleNameResult _
derive instance Eq UpdateModuleNameResult
derive instance Ord UpdateModuleNameResult
instance Show UpdateModuleNameResult where show = genericShow

updateModuleName :: String -> ModuleName -> UpdateModuleNameResult
updateModuleName fileContent newModuleName =
  let
    newModuleName_printed = printModuleName newModuleName
  in
    case matchCurrentModuleName fileContent of
         Just currentModuleName ->
           if currentModuleName == NonEmptyString.toString newModuleName_printed
           then UpdateModuleNameResult_AlreadyUpToDate
           else UpdateModuleNameResult_Updated { from: currentModuleName, newFileContent: replaceModuleName_string fileContent newModuleName_printed }
         Nothing -> UpdateModuleNameResult_Appended { newFileContent: "module " <> NonEmptyString.toString newModuleName_printed <> " where\n\n" <> fileContent }

data UpdateModuleNameResult_Tag
  = UpdateModuleNameResult_Tag_Updated
  | UpdateModuleNameResult_Tag_Appended
  | UpdateModuleNameResult_Tag_AlreadyUpToDate

derive instance Eq UpdateModuleNameResult_Tag
derive instance Ord UpdateModuleNameResult_Tag
derive instance Generic UpdateModuleNameResult_Tag _
instance Show UpdateModuleNameResult_Tag where show = genericShow

updateModuleNameResult_codec :: CA.JsonCodec UpdateModuleNameResult
updateModuleNameResult_codec = CA.taggedSum "UpdateModuleNameResult"
  printTag
  parseTag
  decodeCase
  encodeCase
  where
    printTag = case _ of
      UpdateModuleNameResult_Tag_Updated -> "updated"
      UpdateModuleNameResult_Tag_Appended -> "appended"
      UpdateModuleNameResult_Tag_AlreadyUpToDate -> "alreadyUpToDate"

    parseTag = case _ of
      "updated" -> Just UpdateModuleNameResult_Tag_Updated
      "appended" -> Just UpdateModuleNameResult_Tag_Appended
      "alreadyUpToDate" -> Just UpdateModuleNameResult_Tag_AlreadyUpToDate
      _ -> Nothing

    updatedCodec :: CA.JsonCodec { from :: String, newFileContent :: String }
    updatedCodec = CA.object "Updated" $ CA.record
      # CA.recordProp (Proxy :: _ "from") CA.string
      # CA.recordProp (Proxy :: _ "newFileContent") CA.string

    appendedCodec :: CA.JsonCodec { newFileContent :: String }
    appendedCodec = CA.object "Appended" $ CA.record
      # CA.recordProp (Proxy :: _ "newFileContent") CA.string

    decodeCase :: UpdateModuleNameResult_Tag -> Either UpdateModuleNameResult (J.Json -> Either CA.JsonDecodeError UpdateModuleNameResult)
    decodeCase = case _ of
      UpdateModuleNameResult_Tag_Updated -> Right $ CA.decode $ map UpdateModuleNameResult_Updated updatedCodec
      UpdateModuleNameResult_Tag_Appended -> Right $ CA.decode $ map UpdateModuleNameResult_Appended appendedCodec
      UpdateModuleNameResult_Tag_AlreadyUpToDate -> Left UpdateModuleNameResult_AlreadyUpToDate

    encodeCase = case _ of
      UpdateModuleNameResult_Updated r -> Tuple UpdateModuleNameResult_Tag_Updated $ Just $ CA.encode updatedCodec r
      UpdateModuleNameResult_Appended r -> Tuple UpdateModuleNameResult_Tag_Appended $ Just $ CA.encode appendedCodec r
      UpdateModuleNameResult_AlreadyUpToDate -> Tuple UpdateModuleNameResult_Tag_AlreadyUpToDate Nothing
