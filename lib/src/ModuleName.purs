module ModuleName
  ( ModuleName
  , ModuleNamePart
  , isValidModulePart
  , isValidModulePart'
  , parseModuleNameOrThrow_arrayOfString
  , parseModuleNameOrThrow_stringSeparatedWithDots
  , parseModuleName_stringSeparatedWithDots
  , prependMaybeModuleName
  , printModuleName
  , printModuleNameWith
  , testModuleName
  , toModuleNamePart_fromString
  , toModuleNamePart_fromNonEmptyString
  , toModuleName_fromArrayOfString
  , toModuleName_fromStringSeparatedWithDots
  , unModuleName_toNonEmptyArrayOfModuleNamePart
  , unModuleName_toArrayOfNonEmptyString
  , unModuleName_toArrayOfString
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

toModuleNamePart_fromString :: String -> Maybe ModuleNamePart
toModuleNamePart_fromString = NonEmptyString.fromString >=> toModuleNamePart_fromNonEmptyString

toModuleNamePart_fromNonEmptyString :: NonEmptyString -> Maybe ModuleNamePart
toModuleNamePart_fromNonEmptyString s = if isValidModulePart' s then Just (ModuleNamePart s) else Nothing

unModuleNamePart_toNonEmptyString :: ModuleNamePart -> NonEmptyString
unModuleNamePart_toNonEmptyString = coerce

unModuleNamePart_toString :: ModuleNamePart -> String
unModuleNamePart_toString = coerce

-- Define the ModuleName type as a NonEmpty of NonEmptyText
newtype ModuleName = ModuleName (NonEmptyArray ModuleNamePart)

derive newtype instance Show ModuleName

unModuleName_toNonEmptyArrayOfModuleNamePart :: ModuleName -> NonEmptyArray ModuleNamePart
unModuleName_toNonEmptyArrayOfModuleNamePart = coerce

unModuleName_toArrayOfNonEmptyString :: ModuleName -> Array NonEmptyString
unModuleName_toArrayOfNonEmptyString = coerce

unModuleName_toArrayOfString :: ModuleName -> Array String
unModuleName_toArrayOfString = coerce

unsafeModuleName :: Partial => Array String -> ModuleName
unsafeModuleName s = unsafePerformEffect $ parseModuleNameOrThrow_arrayOfString s

testModuleName :: ModuleName
testModuleName = unsafePartial $ unsafeModuleName ["Test"]

toModuleName__error :: String
toModuleName__error = "Module name cannot be empty. Each part must start with an uppercase letter and be alphanumeric."

-- Convert array of strings to ModuleName
toModuleName_fromArrayOfString :: Array String -> Maybe ModuleName
toModuleName_fromArrayOfString = NonEmptyArray.fromArray >=> traverse toModuleNamePart_fromString >>> map ModuleName

toModuleName_fromStringSeparatedWith :: Pattern -> String -> Maybe ModuleName
toModuleName_fromStringSeparatedWith separator = toModuleName_fromArrayOfString <<< String.split separator

toModuleName_fromStringSeparatedWithDots :: String -> Maybe ModuleName
toModuleName_fromStringSeparatedWithDots = toModuleName_fromStringSeparatedWith (Pattern ".")

-- Prepend a Maybe ModuleName
prependMaybeModuleName :: Maybe ModuleName -> ModuleName -> ModuleName
prependMaybeModuleName Nothing moduleName = moduleName
prependMaybeModuleName (Just parentModuleName) moduleName =
  ModuleName (unModuleName_toNonEmptyArrayOfModuleNamePart parentModuleName <> unModuleName_toNonEmptyArrayOfModuleNamePart moduleName)

-- Parser for ModuleName
parseModuleName_stringSeparatedWithDots :: ReadM ModuleName
parseModuleName_stringSeparatedWithDots = eitherReader \s ->
  case toModuleName_fromStringSeparatedWithDots s of
    Nothing -> Left $ toModuleName__error
    Just x -> Right x

parseModuleNameOrThrow_stringSeparatedWithDots :: forall m . MonadThrow Error m => String -> m ModuleName
parseModuleNameOrThrow_stringSeparatedWithDots s =
  case toModuleName_fromStringSeparatedWithDots s of
    Nothing -> throwError $ error toModuleName__error
    Just x -> pure x

parseModuleNameOrThrow_arrayOfString :: forall m . MonadThrow Error m => Array String -> m ModuleName
parseModuleNameOrThrow_arrayOfString s =
  case toModuleName_fromArrayOfString s of
    Nothing -> throwError $ error toModuleName__error
    Just x -> pure x

printModuleNameWith :: String -> ModuleName -> NonEmptyString
printModuleNameWith separator moduleName = NonEmptyString.join1With separator (unModuleName_toArrayOfNonEmptyString moduleName)

printModuleName :: ModuleName -> NonEmptyString
printModuleName = printModuleNameWith "."

-- moduleNamePart_codec :: CA.JsonCodec ModuleNamePart
-- moduleNamePart_codec = CA.prismaticCodec "ModuleNamePart" toModuleNamePart_fromString unModuleNamePart_toString CA.string

moduleName_codec :: CA.JsonCodec ModuleName
moduleName_codec = CA.prismaticCodec "ModuleName" toModuleName_fromArrayOfString unModuleName_toArrayOfString (CA.array CA.string)
