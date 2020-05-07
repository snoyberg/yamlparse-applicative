{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module YamlParse.Applicative where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Scientific
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Validity
import Data.Validity.Text ()
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)

someFunc :: IO ()
someFunc = do
  forM_
    (explainParser (yamlSchema :: YamlParser MyConfig))
    $ T.putStrLn . prettySchema

data MyConfig
  = MyConfig
      { myConfigText :: Text,
        myConfigScientific :: Maybe Scientific,
        myConfigList :: [Bool],
        myConfigSub :: Maybe MySubConfig
      }
  deriving (Show, Eq)

instance YamlSchema MyConfig where
  yamlSchema =
    object "MyConfig" $
      MyConfig
        <$> requiredField "foo" "My foo"
        <*> optionalField "bar" "My bar"
        <*> optionalFieldWithDefault "quux" [] "My quux"
        <*> optionalField "sub" "My sub"

data MySubConfig
  = MySubConfig
      { mySubConfigBool :: Maybe Bool,
        mySubConfigText :: Text,
        mySubConfigAlt :: Either Text Bool
      }
  deriving (Show, Eq)

instance YamlSchema MySubConfig where
  yamlSchema =
    object "MySubConfig" $
      MySubConfig
        <$> optionalField "foofoo" "My foofoo"
        <*> optionalFieldWithDefault "barbar" "defaultTextHere" "My bar"
        <*> (Left <$> (requiredField "left" "The left case") <|> Right <$> (requiredField "right" "The right case"))

-- | A class of types for which a schema is defined.
--
-- Note that you do not have to use this class and can just use your own parser values.
-- Note also that the parsing of a type of this class should correspond to the parsing of the type in the FromJSON class.
class YamlSchema a where
  yamlSchema :: YamlParser a

instance YamlSchema Bool where
  yamlSchema = ParseBool Nothing ParseAny

instance YamlSchema Text where
  yamlSchema = ParseString Nothing ParseAny

instance YamlSchema Scientific where
  yamlSchema = ParseNumber Nothing ParseAny

instance YamlSchema Yaml.Object where
  yamlSchema = ParseObject Nothing ParseAny

instance YamlSchema Yaml.Value where
  yamlSchema = ParseAny

instance YamlSchema a => YamlSchema (Vector a) where
  yamlSchema = ParseArray Nothing (ParseList yamlSchema)

instance YamlSchema a => YamlSchema [a] where
  yamlSchema = V.toList <$> ParseArray Nothing (ParseList yamlSchema)

-- | A parser for a required field at a given key
requiredField :: YamlSchema a => Text -> Text -> ObjectParser a
requiredField k h = requiredFieldWith k h yamlSchema

-- | A parser for a required field at a given key without a help text
requiredField' :: YamlSchema a => Text -> ObjectParser a
requiredField' k = requiredFieldWith' k yamlSchema

-- | A parser for a required field at a given key with a parser for what is found at that key
requiredFieldWith :: Text -> Text -> YamlParser a -> ObjectParser a
requiredFieldWith k h func = ParseComment h $ ParseField k $ FieldParserRequired func

-- | A parser for a required field at a given key with a parser for what is found at that key without a help text
requiredFieldWith' :: Text -> YamlParser a -> ObjectParser a
requiredFieldWith' k func = ParseField k $ FieldParserRequired func

-- | A parser for an optional field at a given key
optionalField :: YamlSchema a => Text -> Text -> ObjectParser (Maybe a)
optionalField k h = optionalFieldWith k h yamlSchema

-- | A parser for an optional field at a given key without a help text
optionalField' :: YamlSchema a => Text -> ObjectParser (Maybe a)
optionalField' k = optionalFieldWith' k yamlSchema

-- | A parser for an optional field at a given key with a parser for what is found at that key
optionalFieldWith :: Text -> Text -> YamlParser a -> ObjectParser (Maybe a)
optionalFieldWith k h func = ParseComment h $ ParseField k $ FieldParserOptional func

-- | A parser for an optional field at a given key with a parser for what is found at that key without a help text
optionalFieldWith' :: Text -> YamlParser a -> ObjectParser (Maybe a)
optionalFieldWith' k func = ParseField k $ FieldParserOptional func

-- | A parser for an optional field at a given key with a default value
optionalFieldWithDefault :: (Show a, YamlSchema a) => Text -> a -> Text -> ObjectParser a
optionalFieldWithDefault k d h = optionalFieldWithDefaultWith k d h yamlSchema

-- | A parser for an optional field at a given key with a default value without a help text
optionalFieldWithDefault' :: (Show a, YamlSchema a) => Text -> a -> ObjectParser a
optionalFieldWithDefault' k d = optionalFieldWithDefaultWith' k d yamlSchema

-- | A parser for an optional field at a given key with a default value and a parser for what is found at that key
--
-- For the sake of documentation, the default value needs to be showable.
optionalFieldWithDefaultWith :: Show a => Text -> a -> Text -> YamlParser a -> ObjectParser a
optionalFieldWithDefaultWith k d h func = ParseComment h $ ParseField k $ FieldParserOptionalWithDefault func d

-- | A parser for an optional field at a given key with a default value and a parser for what is found at that key without a help text
--
-- For the sake of documentation, the default value needs to be showable.
optionalFieldWithDefaultWith' :: Show a => Text -> a -> YamlParser a -> ObjectParser a
optionalFieldWithDefaultWith' k d func = ParseField k $ FieldParserOptionalWithDefault func d

type YamlParser a = Parser Yaml.Value a

type ObjectParser a = Parser Yaml.Object a

data Parser i o where
  -- | Return the input
  ParseAny :: Parser i i
  -- | Parse a boolean value
  ParseBool :: Maybe Text -> Parser Bool o -> Parser Yaml.Value o
  -- | Parse a String value
  ParseString ::
    -- | Extra info about what the string represents
    -- This info will be used during parsing for error messages and in the schema for documentation.
    Maybe Text ->
    Parser Text o ->
    Parser Yaml.Value o
  -- | Parse a numeric value
  ParseNumber ::
    -- | Extra info about what the number represents
    -- This info will be used during parsing for error messages and in the schema for documentation.
    Maybe Text ->
    Parser Scientific o ->
    Parser Yaml.Value o
  -- | Parse an array
  ParseArray ::
    -- | Extra info about what the array represents
    -- This info will be used during parsing for error messages and in the schema for documentation.
    Maybe Text ->
    Parser Yaml.Array o ->
    Parser Yaml.Value o
  -- | Parse an object
  ParseObject ::
    -- | Extra info about what the object represents
    -- This info will be used during parsing for error messages and in the schema for documentation.
    Maybe Text ->
    Parser Yaml.Object a ->
    Parser Yaml.Value a
  -- | Parse a list of elements all in the same way
  ParseList ::
    Parser Yaml.Value o ->
    Parser Yaml.Array (Vector o)
  -- | Parse a field of an object
  ParseField ::
    Text ->
    -- | The key of the field
    FieldParser o ->
    Parser Yaml.Object o
  -- | A pure value
  ParsePure :: a -> Parser i a
  -- | To implement Functor
  ParseFmap :: (a -> b) -> Parser i a -> Parser i b
  -- | To implement Applicative
  ParseAp :: Parser i (a -> b) -> Parser i a -> Parser i b
  -- | To implement Alternative
  ParseAlt :: [Parser i o] -> Parser i o
  -- | Add comments to the parser.
  -- This info will be used in the schema for documentation.
  ParseComment :: Text -> Parser i o -> Parser i o

instance Functor (Parser i) where
  fmap = ParseFmap

-- Realy only makes sense for 'Parser Yaml.Object', but we need 'Parser i' here to get the 'Alternative' instance to work
instance Applicative (Parser i) where
  pure = ParsePure
  (<*>) = ParseAp

instance Alternative (Parser i) where
  empty = ParseAlt []
  l <|> r = ParseAlt [l, r]
  some = undefined -- TODO figure out what to do here
  many = undefined

data FieldParser o where
  FieldParserRequired :: YamlParser o -> FieldParser o
  FieldParserOptional :: YamlParser o -> FieldParser (Maybe o)
  FieldParserOptionalWithDefault :: Show o => YamlParser o -> o -> FieldParser o

-- | Declare a parser of a named object
object :: Text -> ObjectParser o -> YamlParser o
object name = ParseObject (Just name)

-- | Declare a parser of an unnamed object
--
-- Prefer 'object' if you can.
unnamedObject :: ObjectParser o -> YamlParser o
unnamedObject = ParseObject Nothing

-- | Add a comment to a parser
-- This info will be used in the schema for documentation.
(<?>) :: Parser i a -> Text -> Parser i a
(<?>) = flip ParseComment

-- | Use a 'Parser' to parse a value from Yaml.
--
-- A 'Parser i o' corresponds exactly to a 'i -> Yaml.Parser o' and this function servers as evidence for that.
implementParser :: Parser i o -> (i -> Yaml.Parser o)
implementParser = go
  where
    go :: Parser i o -> (i -> Yaml.Parser o)
    go = \case
      ParseAny -> pure
      ParseBool t p -> Yaml.withBool (maybe "Bool" T.unpack t) $ go p
      ParseString t p -> Yaml.withText (maybe "String" T.unpack t) $ go p
      ParseNumber t p -> Yaml.withScientific (maybe "Number" T.unpack t) $ go p
      ParseList p -> \l -> forM l $ \v -> go p v
      ParseArray t p -> Yaml.withArray (maybe "Array" T.unpack t) $ go p
      ParseField key fp -> \o -> case fp of
        FieldParserRequired p -> o Yaml..: key >>= go p
        FieldParserOptional p -> do
          mv <- o Yaml..:? key
          case mv of
            Nothing -> pure Nothing
            Just v -> Just <$> go p v
        FieldParserOptionalWithDefault p d -> do
          mv <- o Yaml..:? key
          case mv of
            Nothing -> pure d
            Just v -> go p v
      ParseObject t p -> Yaml.withObject (maybe "Object" T.unpack t) $ go p
      ParsePure v -> const $ pure v
      ParseAp pf p -> \v -> go pf v <*> go p v
      ParseAlt ps -> \v -> case ps of
        [] -> fail "No alternatives."
        (p : ps') -> go p v <|> go (ParseAlt ps') v
      ParseFmap f p -> fmap f . go p
      ParseComment _ p -> go p

-- | Use a parser to produce a schema that describes it for documentation.
--
-- Nothing means that nothing even needs to be parsed, you just get the 'a' without parsing anything.
-- This is for the 'pure' case.
explainParser :: Parser i o -> Maybe Schema
explainParser = go
  where
    go :: Parser i o -> Maybe Schema
    go = \case
      ParseAny -> Just AnySchema
      ParseBool t _ -> Just $ BoolSchema t
      ParseNumber t _ -> Just $ NumberSchema t
      ParseString t _ -> Just $ StringSchema t
      ParseArray t p -> ArraySchema t <$> go p
      ParseList p -> ListSchema <$> go p
      ParseField k fp -> case fp of
        FieldParserRequired p -> FieldSchema k True Nothing <$> go p
        FieldParserOptional p -> FieldSchema k False Nothing <$> go p
        FieldParserOptionalWithDefault p d -> FieldSchema k False (Just $ T.pack $ show d) <$> go p
      ParseObject t p -> ObjectSchema t <$> go p
      ParsePure _ -> Nothing
      ParseFmap _ p -> go p
      ParseAp pf p -> ApSchema <$> go pf <*> go p
      ParseAlt ps -> Just $ AltSchema $ catMaybes (map go ps)
      ParseComment t p -> CommentSchema t <$> go p

-- A schema for a parser.
--
-- This is used to produce documentation for what/how the parser parses.
data Schema
  = AnySchema
  | BoolSchema (Maybe Text)
  | NumberSchema (Maybe Text)
  | StringSchema (Maybe Text)
  | ArraySchema (Maybe Text) Schema
  | ObjectSchema (Maybe Text) Schema
  | ListSchema Schema
  | FieldSchema Text Bool (Maybe Text) Schema
  | ApSchema Schema Schema -- We'll take this to mean 'and'
  | AltSchema [Schema]
  | CommentSchema Text Schema
  deriving (Show, Eq, Generic)

instance Validity Schema

-- | Render a schema as pretty text.
--
-- This is meant for humans.
-- The output may look like YAML but it is not.
prettySchema :: Schema -> Text
prettySchema = renderStrict . layoutPretty defaultLayoutOptions . schemaDoc

-- | A list of comments
newtype Comments = Comments {commentsList :: [Doc ()]}
  deriving (Show)

instance Semigroup Comments where
  (Comments l1) <> (Comments l2) = Comments $ l1 <> l2

instance Monoid Comments where
  mempty = emptyComments
  mappend = (<>)

-- | No comments
emptyComments :: Comments
emptyComments = Comments []

-- | A raw text as comments
comment :: Text -> Comments
comment t = Comments [pretty t]

-- | Prettyprint a 'Schema'
schemaDoc :: Schema -> Doc ()
schemaDoc = go emptyComments
  where
    go :: Comments -> Schema -> Doc ()
    go cs =
      let g = go cs
          ge = go emptyComments
          mkComment :: Doc () -> Doc ()
          mkComment = ("# " <>)
          mkCommentsMDoc :: Comments -> Maybe (Doc ())
          mkCommentsMDoc = \case
            Comments [] -> Nothing
            Comments l -> Just $ align $ vsep $ map mkComment l
          addMComment :: Comments -> Maybe Text -> Comments
          addMComment c = \case
            Nothing -> c
            Just t -> c <> comment t
          e :: Doc () -> Comments -> Doc ()
          e s cs' =
            case mkCommentsMDoc cs' of
              Nothing -> s
              Just cd -> vsep [cd, s]
       in \case
            AnySchema -> e "<any>" cs
            BoolSchema t -> e "<bool>" $ addMComment cs t
            NumberSchema t -> e "<number>" $ addMComment cs t
            StringSchema t -> e "<string>" $ addMComment cs t
            ArraySchema t s -> "-" <+> align (go (addMComment cs t) s)
            -- The comments really only work on the object level
            -- so they are erased when going down
            ObjectSchema t s -> e (ge s) (addMComment cs t)
            ListSchema s -> g s
            FieldSchema k r md s ->
              let keyDoc :: Doc a
                  keyDoc = pretty k
                  requiredDoc :: Doc a
                  requiredDoc =
                    if r
                      then "required"
                      else case md of
                        Nothing -> "optional"
                        Just d -> "optional, default:" <+> pretty d
               in vsep
                    [ keyDoc <> ":" <+> mkComment requiredDoc,
                      indent 2 $ g s
                    ]
            ApSchema s1 s2 -> align $ vsep [g s1, g s2]
            AltSchema ss ->
              let listDoc :: [Doc a] -> Doc a
                  listDoc = \case
                    [] -> "[]"
                    (d : ds) -> vsep ["[" <+> nest 2 d, vsep $ map (("," <+>) . nest 2) ds, "]"]
               in e (listDoc $ map ge ss) (cs <> comment "Alternatives")
            CommentSchema t s -> go (cs <> comment t) s
