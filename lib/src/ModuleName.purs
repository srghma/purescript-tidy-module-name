module ModuleName
  ( ModuleName
  , unModuleNameAndParts
  , unModuleName
  , ModuleNamePart
  , toModuleNamePart
  , toModuleNamePart'
  , unModuleNamePart
  , isValidModulePart
  , isValidModulePart'
  , parseModuleName_stringSeparatedWithDots
  , parseModuleNameOrThrow_stringSeparatedWithDots
  , parseModuleNameOrThrow_arrayOfString
  , printModuleName
  , printModuleNameWith
  , testModuleName
  , prependMaybeModuleName
  , unsafeModuleName
  )
  where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.CodePoint.Unicode as CodePoint
import Data.Either (Either(..))
import Data.Foldable (all)
import Data.Maybe (Maybe(..))
import Data.String (CodePoint)
import Data.String.Common (split) as String
import Data.String.NonEmpty (NonEmptyString, Pattern(..))
import Data.String.NonEmpty as NonEmptyString
import Data.Traversable (traverse)
import Effect.Exception (Error, error)
import Effect.Unsafe (unsafePerformEffect)
import Options.Applicative (ReadM, eitherReader)
import Partial.Unsafe (unsafePartial)
import Safe.Coerce (coerce)

newtype ModuleNamePart = ModuleNamePart NonEmptyString

derive newtype instance Show ModuleNamePart

-- Validate a module part
isValidModulePart :: NonEmptyString -> Boolean
isValidModulePart = isValidModulePart' <<< NonEmptyString.toNonEmptyCodePointArray

isValidModulePart' :: NonEmptyArray CodePoint -> Boolean
isValidModulePart' part =
  CodePoint.isAsciiUpper (NonEmptyArray.head part) &&
  all CodePoint.isAlphaNum part

toModuleNamePart :: String -> Maybe ModuleNamePart
toModuleNamePart = NonEmptyString.fromString >=> toModuleNamePart'

toModuleNamePart' :: NonEmptyString -> Maybe ModuleNamePart
toModuleNamePart' s = if isValidModulePart s then Just (ModuleNamePart s) else Nothing

unModuleNamePart :: ModuleNamePart -> NonEmptyString
unModuleNamePart (ModuleNamePart s) = s

-- Define the ModuleName type as a NonEmpty of NonEmptyText
newtype ModuleName = ModuleName (NonEmptyArray ModuleNamePart)

derive newtype instance Show ModuleName

unModuleName :: ModuleName -> NonEmptyArray ModuleNamePart
unModuleName = coerce

unModuleNameAndParts :: ModuleName -> NonEmptyArray NonEmptyString
unModuleNameAndParts = coerce

unsafeModuleName :: Partial => Array String -> ModuleName
unsafeModuleName s = unsafePerformEffect $ parseModuleNameOrThrow_arrayOfString s

testModuleName :: ModuleName
testModuleName = unsafePartial $ unsafeModuleName ["Test"]

toModuleName__error :: String
toModuleName__error = "Module name cannot be empty. Each part must start with an uppercase letter and be alphanumeric."

-- Convert array of strings to ModuleName
toModuleName_arrayOfString :: Array String -> Maybe ModuleName
toModuleName_arrayOfString = NonEmptyArray.fromArray >=> traverse toModuleNamePart >>> map ModuleName

toModuleName_stringSeparatedWith :: Pattern -> String -> Maybe ModuleName
toModuleName_stringSeparatedWith separator = toModuleName_arrayOfString <<< String.split separator

toModuleName_stringSeparatedWithDots :: String -> Maybe ModuleName
toModuleName_stringSeparatedWithDots = toModuleName_stringSeparatedWith (Pattern ".")

-- Prepend a Maybe ModuleName
prependMaybeModuleName :: Maybe ModuleName -> ModuleName -> ModuleName
prependMaybeModuleName Nothing moduleName = moduleName
prependMaybeModuleName (Just parentModuleName) moduleName =
  ModuleName (unModuleName parentModuleName <> unModuleName moduleName)

-- Parser for ModuleName
parseModuleName_stringSeparatedWithDots :: ReadM ModuleName
parseModuleName_stringSeparatedWithDots = eitherReader \s ->
  case toModuleName_stringSeparatedWithDots s of
    Nothing -> Left $ toModuleName__error
    Just x -> Right x

parseModuleNameOrThrow_stringSeparatedWithDots :: forall m . MonadThrow Error m => String -> m ModuleName
parseModuleNameOrThrow_stringSeparatedWithDots s =
  case toModuleName_stringSeparatedWithDots s of
    Nothing -> throwError $ error toModuleName__error
    Just x -> pure x

parseModuleNameOrThrow_arrayOfString :: forall m . MonadThrow Error m => Array String -> m ModuleName
parseModuleNameOrThrow_arrayOfString s =
  case toModuleName_arrayOfString s of
    Nothing -> throwError $ error toModuleName__error
    Just x -> pure x

printModuleNameWith :: String -> ModuleName -> NonEmptyString
printModuleNameWith separator moduleName = NonEmptyString.join1With separator (unModuleNameAndParts moduleName)

printModuleName :: ModuleName -> NonEmptyString
printModuleName = printModuleNameWith "."
