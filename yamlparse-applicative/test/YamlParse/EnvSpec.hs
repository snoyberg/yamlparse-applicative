{-#LANGUAGE OverloadedStrings#-}

module YamlParse.EnvSpec where

import YamlParse.Applicative
import Data.Text
import Test.Hspec
import Path (parseAbsFile)
import Data.Aeson

data TestConfiguration = TestConfiguration {
      confHome :: Text
} deriving (Show, Eq)

instance YamlSchema TestConfiguration where
    yamlSchema = objectParser "TestConfiguration" (TestConfiguration <$> requiredField "home" "User HOME path")

instance FromJSON TestConfiguration where
  parseJSON = viaYamlSchema

spec :: Spec
spec = describe "environment variable" $ do
         it "reads environment variable" $ do
            file <- parseAbsFile "/home/sibi/fpco/github/fpco/yamlparse-applicative/yamlparse-applicative/test/data/config.yaml"                               
            val <- readConfigFileWithEnv file
            val `shouldBe` (Just $ TestConfiguration { confHome = "/home/sibi"})
