module Test.PursUtils.UpdateModuleNameSpec where

import Prelude

import ModuleName (ModuleName, unsafeModuleName)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)
import UpdateModuleName (UpdateModuleNameResult(..), updateModuleName)

-- Data Percent Example
dataPercent :: ModuleName
dataPercent = unsafePartial $ unsafeModuleName ["Data", "Percent"]

updatedDataPercent :: ModuleName
updatedDataPercent = unsafePartial $ unsafeModuleName ["UpdatedData", "Percent"]

-- The test suite in PureScript
spec :: Spec Unit
spec = do
  it "Data.Percent to Data.Percent -> UpdateModuleNameResult_AlreadyUpToDate" do
    let
      content :: String
      content = """module Data.Percent (Percent, calcPercent, unsafePercent, toNumber) where

newtype Percent = Percent Number
"""
    updateModuleName content dataPercent `shouldEqual` UpdateModuleNameResult_AlreadyUpToDate

  it "Data.Percent to UpdatedData.Percent -> UpdateModuleNameResult_Updated" do
    let
      content :: String
      content = """module Data.Percent (Percent, calcPercent, unsafePercent, toNumber) where

newtype Percent = Percent Number
"""
      newContent :: String
      newContent = """module UpdatedData.Percent (Percent, calcPercent, unsafePercent, toNumber) where

newtype Percent = Percent Number
"""
    updateModuleName content updatedDataPercent `shouldEqual` UpdateModuleNameResult_Updated { from: "Data.Percent", newFileContent: newContent }

  it "Data.Percent to UpdatedData.Percent (without explicit export and where) -> UpdateModuleNameResult_Updated" do
    let
      content :: String
      content = """module Data.Percent

newtype Percent = Percent Number
"""
      newContent :: String
      newContent = """module UpdatedData.Percent

newtype Percent = Percent Number
"""
    updateModuleName content updatedDataPercent `shouldEqual` UpdateModuleNameResult_Updated { from: "Data.Percent", newFileContent: newContent }

  it "UpdatedData.Percent.Smth to UpdatedData.Percent (without explicit export and where) -> UpdateModuleNameResult_Updated" do
    let
      content :: String
      content = """module UpdatedData.Percent.Smth

  newtype Percent = Percent Number
  """
      newContent :: String
      newContent = """module UpdatedData.Percent

  newtype Percent = Percent Number
  """
    updateModuleName content updatedDataPercent `shouldEqual` UpdateModuleNameResult_Updated { from: "UpdatedData.Percent.Smth", newFileContent: newContent }

  it "Data.Percent to UpdatedData.Percent (with explicit export and where on next line) -> UpdateModuleNameResult_Updated" do
    let
      content :: String
      content = """module Data.Percent
  ( Percent
  , calcPercent
  , unsafePercent
  , toNumber
  ) where

newtype Percent = Percent Number
"""
      newContent :: String
      newContent = """module UpdatedData.Percent
  ( Percent
  , calcPercent
  , unsafePercent
  , toNumber
  ) where

newtype Percent = Percent Number
"""
    -- unsafePartial $ NonEmptyString.unsafeFromString
    updateModuleName content updatedDataPercent `shouldEqual` UpdateModuleNameResult_Updated { from: "Data.Percent", newFileContent: newContent }

  it "NOTHING to UpdatedData.Percent (with explicit export and where on next line) -> UpdateModuleNameResult_Updated" do
    let
      content :: String
      content = """module
  ( Percent
  , calcPercent
  , unsafePercent
  , toNumber
  ) where

newtype Percent = Percent Number
"""
      newContent :: String
      newContent = """module UpdatedData.Percent( Percent
  , calcPercent
  , unsafePercent
  , toNumber
  ) where

newtype Percent = Percent Number
"""
    updateModuleName content updatedDataPercent `shouldEqual` UpdateModuleNameResult_Updated { from: "", newFileContent: newContent }

  it "NOTHING to UpdatedData.Percent (with explicit export and where on next line) -> UpdateModuleNameResult_Updated" do
    let
      content :: String
      content = """modul
  ( Percent
  , calcPercent
  , unsafePercent
  , toNumber
  ) where

newtype Percent = Percent Number
"""
      newContent :: String
      newContent = """module UpdatedData.Percent where

modul
  ( Percent
  , calcPercent
  , unsafePercent
  , toNumber
  ) where

newtype Percent = Percent Number
"""
    updateModuleName content updatedDataPercent `shouldEqual` UpdateModuleNameResult_Appended { newFileContent: newContent }
