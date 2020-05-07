{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module YamlParse.ApplicativeSpec where

import qualified Data.Aeson.Types as Aeson
import Data.GenValidity.Aeson ()
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Typeable
import Test.Hspec
import Test.QuickCheck
import Test.Validity
import Test.Validity.Utils
import YamlParse.Applicative as Yaml

spec :: Spec
spec =
  describe "implementParser" $ do
    implementationsSpec @Bool
    implementationsSpec @Char
    implementationsSpec @String
    implementationsSpec @Text
    implementationsSpec @Scientific
    implementationsSpec @Aeson.Array
    implementationsSpec @Aeson.Object
    implementationsSpec @Aeson.Value
    implementationsSpec @[Text]

implementationsSpec ::
  forall a.
  ( Show a,
    Eq a,
    Typeable a,
    GenValid a,
    Aeson.FromJSON a,
    Aeson.ToJSON a,
    YamlSchema a
  ) =>
  Spec
implementationsSpec = specify ("The implementation of 'parseJSON' matches the implementation of 'implementParser yamlSchema' for " <> nameOf @a) $ implementationsMatch @a

implementationsMatch ::
  forall a.
  ( Show a,
    Eq a,
    GenValid a,
    Aeson.FromJSON a,
    Aeson.ToJSON a,
    YamlSchema a
  ) =>
  Property
implementationsMatch =
  forAllValid $
    \a -> do
      let v = Aeson.toJSON (a :: a)
      let aesonResult = Aeson.parseEither Aeson.parseJSON v :: Either String a
          yamlResult = Aeson.parseEither (implementParser yamlSchema) v
      yamlResult `shouldBe` aesonResult