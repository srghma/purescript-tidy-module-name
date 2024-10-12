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

type WorkerData =
  { isCheckMode :: Boolean
  }

type WorkerInput = FilePath
  -- { filePath :: FilePath
  -- }

type WorkerOutput =
  { filePath :: FilePath
  , result_UpdateModuleNameResult :: J.Json
  -- , timing :: Number
  }

formatInPlaceCommand :: Boolean -> WorkerInput -> Aff WorkerOutput
formatInPlaceCommand isCheckMode filePath = do
  contents <- FS.readTextFile UTF8 filePath

  let updateModuleNameResult = updateModuleName contents newModuleName

  -- case formatCommand formatOptions operators contents of
  --   Right formatted -> do
  --     timing <- map (unwrap <<< toMilliseconds) $ liftEffect $ hrtimeDiff start
  --     if isCheckMode then do
  --       let alreadyFormatted = formatted == contents
  --       pure { filePath, error: "", alreadyFormatted, timing }
  --     else do
  --       FS.writeTextFile UTF8 filePath formatted
  --       pure { filePath, error: "", alreadyFormatted: false, timing }
  --   Left error ->
  pure { filePath, result_UpdateModuleNameResult: CA.encode updateModuleNameResult_codec updateModuleNameResult }

main :: Effect Unit
main = Worker.makeAsMain \{ receive, reply, workerData: { isCheckMode } } -> do
  receive \input -> do
    runAff_
      (either throwError reply)
      (formatInPlaceCommand isCheckMode input)
