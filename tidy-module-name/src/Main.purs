module Main where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as Arg
import Bin.Version (version)
import Bin.Worker
import Control.Monad.State (evalStateT, lift)
import Control.Monad.State as State
import Control.Parallel (parTraverse)
import Control.Plus ((<|>))
import Data.Argonaut.Core as Json
import Data.Argonaut.Decode (parseJson, printJsonDecodeError)
import Data.Array as Array
import Data.Either (Either(..), isLeft)
import Data.Foldable (fold, foldMap, foldl, for_, oneOf)
import Data.Lazy (Lazy)
import Data.Lazy as Lazy
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Data.Number.Format as NF
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, throwError, try)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.FS.Stats as Stats
import Node.Glob.Basic (expandGlobsCwd, expandGlobsWithStatsCwd)
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process
import Node.Stream.Aff as Stream.Aff
import Node.WorkerBees as Worker
import Node.WorkerBees.Aff.Pool (poolTraverse)
import Debug

data FormatMode = Check | Write

derive instance Eq FormatMode

data Command
  = FormatInPlace FormatMode Int Boolean (Array String)

parser :: ArgParser Command
parser =
  Arg.choose "command"
    [ Arg.command [ "format-in-place" ]
        "Format source files in place."
        do
          FormatInPlace Write
            <$> workerOptions
            <*> timingOption
            <*> pursGlobs
            <* Arg.flagHelp
    , Arg.command [ "check" ]
        "Check source files are formatted."
        do
          FormatInPlace Check
            <$> workerOptions
            <*> timingOption
            <*> pursGlobs
            <* Arg.flagHelp
    ]
    <* Arg.flagInfo [ "--version", "-v" ] "Shows the current version." version
    <* Arg.flagHelp
  where
  pursGlobs =
    Arg.anyNotFlag "PURS_GLOB" "Globs for PureScript sources."
      # Arg.unfolded1

  workerOptions =
    Arg.argument [ "--threads", "-t" ]
      "Number of worker threads to use.\nDefaults to 4."
      # Arg.int
      # Arg.default 4

  timingOption =
    Arg.flag [ "--timing" ]
      "Print the time spent formatting each file."
      # Arg.boolean
      # Arg.default false

main :: Effect Unit
main = launchAff_ do
  args <- Array.drop 2 <$> liftEffect Process.argv
  let
    parsedCmd =
      Arg.parseArgs "purs-tidy-module-name" "Finds .purs files and updates their module name\n./src/Foo/Bar.purs -> module Foo.Bar\n./test/Foo/Bar.purs -> module Test.Foo.Bar" parser args

  case parsedCmd of
    Left err -> do
      Console.log $ Arg.printArgError err
      case err of
        Arg.ArgError _ Arg.ShowHelp ->
          liftEffect $ Process.exit
        Arg.ArgError _ (Arg.ShowInfo _) ->
          liftEffect $ Process.exit' 0
        _ ->
          liftEffect $ Process.exit' 1
    Right cmd ->
      case cmd of
        FormatInPlace mode numThreads printTiming globs -> do
          currentDir <- liftEffect Process.cwd
          let root = (Path.parse currentDir).root
          srcLocation <- fold <$> liftEffect (Process.lookupEnv "TIDY_INSTALL_LOC")
          files <- expandGlobs globs
          traceM files

          let
            workerData :: WorkerData
            workerData =
              { isCheckMode: mode == Check
              }
          results <-
            -- if Array.length files > numThreads * 2 then do
            --   -- Worker location for production bin
            --   let bundleLocation = Path.concat [ srcLocation, "bundle", "Bin.Worker", "index.js" ]
            --   -- Worker location for local dev
            --   let outputLocation = Path.concat [ srcLocation, "output", "Bin.Worker", "index.js" ]
            --   worker <-
            --     oneOf
            --       [ FS.stat bundleLocation $> Worker.unsafeWorkerFromPath bundleLocation
            --       , FS.stat outputLocation $> Worker.unsafeWorkerFromPath outputLocation
            --       ]
            --       <|> throwError (error "Worker not found")
            --   poolTraverse worker workerData numThreads files
            -- else
            parTraverse (formatInPlaceOne workerData) files

          -- let
          --   { errors, notFormatted } =
          --     results # foldMap \{ filePath, error, alreadyFormatted } ->
          --       { errors: guard (not String.null error) [ filePath /\ error ]
          --       , notFormatted: guard (not alreadyFormatted) [ filePath ]
          --       }
          --
          -- when printTiming do
          --   for_ (Array.sortBy (comparing _.timing) results) \{ filePath, timing } ->
          --     when (timing > 0.0) do
          --       Console.log $ fold
          --         [ Path.relative currentDir filePath
          --         , " "
          --         , NF.toStringWith (NF.fixed 2) timing
          --         , "ms"
          --         ]
          --
          -- case mode of
          --   Write ->
          --     for_ errors \(Tuple filePath error) ->
          --       Console.error $ filePath <> ":\n  " <> error <> "\n"
          --
          --   Check -> liftEffect do
          --     if Array.null errors && Array.null notFormatted then do
          --       Console.log "All files are formatted."
          --       Process.exit
          --     else do
          --       unless (Array.null errors) do
          --         Console.log "Some files have errors:\n"
          --         for_ errors \(Tuple filePath error) ->
          --           Console.error $ filePath <> ":\n  " <> error <> "\n"
          --       unless (Array.null notFormatted) do
          --         Console.log "Some files are not formatted:\n"
          --         for_ notFormatted Console.error
          --       Process.exit' 1
          pure unit


expandGlobs :: Array String -> Aff (Array String)
expandGlobs = map dirToGlob >>> expandGlobsWithStatsCwd >>> map onlyFiles
  where
  dirToGlob path =
    if Path.extname path == "" then
      if isJust (String.stripSuffix (Pattern "**") path) then
        Path.concat [ path, "*.purs" ]
      else
        Path.concat [ path, "**", "*.purs" ]
    else
      path

  onlyFiles =
    Map.filter Stats.isFile
      >>> Map.keys
      >>> Set.toUnfoldable

formatInPlaceOne :: WorkerData -> WorkerInput -> Aff WorkerOutput
formatInPlaceOne { isCheckMode } input = do
  formatInPlaceCommand isCheckMode input
