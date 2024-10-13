module Bin.Worker where

import Prelude

import Bin.Timing (hrtime, hrtimeDiff, toMilliseconds)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..), either, fromRight')
import Data.Lazy (Lazy)
import Data.Lazy as Lazy
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (power)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff, runAff_, throwError)
import Effect.Class (liftEffect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.WorkerBees as Worker
import Partial.Unsafe (unsafeCrashWith)
import Data.Argonaut.Core as J
import UpdateModuleName

type WorkerData =
  { isCheckMode :: Boolean
  }

type WorkerInput_Implementation =
  { filePath :: FilePath
  , newModuleName :: ModuleName
  }

type WorkerOutput_Implementation =
  { filePath :: FilePath
  , result :: UpdateModuleNameResult
  }

type WorkerInput =
  { filePath :: FilePath
  , newModuleName :: String
  }

type WorkerOutput =
  { filePath :: FilePath
  , result :: J.Json
  }

formatInPlaceCommand_Implementation :: Boolean -> WorkerInput_Implementation -> Aff WorkerOutput_Implementation
formatInPlaceCommand_Implementation isCheckMode { filePath, newModuleName } = do
  contents <- FS.readTextFile UTF8 filePath
  let result = updateModuleName contents newModuleName
  pure { filePath, result }

decodeWorkerInput :: J.Json -> Either String WorkerInput_Implementation
decodeWorkerInput = CA.decode $ CA.object "WorkerInput" $ CA.record
  # CA.recordProp (Proxy :: _ "filePath") CA.string
  # CA.recordProp (Proxy :: _ "newModuleName") moduleNameCodec

formatInPlaceCommand :: Boolean -> WorkerInput -> Aff WorkerOutput
formatInPlaceCommand isCheckMode input = do
  workerInput_Implementation <- liftEffect $ either throwError pure $ decodeWorkerInput $ J.fromString input.newModuleName
  { filePath, result } <- formatInPlaceCommand_Implementation isCheckMode workerInput_Implementation
  pure { filePath, result: CA.encode updateModuleNameResult_codec result }

main :: Effect Unit
main = Worker.makeAsMain \{ receive, reply, workerData: { isCheckMode } } -> do
  receive \input -> do
    runAff_
      (either throwError reply)
      (formatInPlaceCommand isCheckMode input)
