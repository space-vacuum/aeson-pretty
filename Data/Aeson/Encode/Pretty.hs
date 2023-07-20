{-# LANGUAGE OverloadedStrings, RecordWildCards, CPP, ViewPatterns #-}

-- |Aeson-compatible pretty-printing of JSON 'Value's.
module Data.Aeson.Encode.Pretty (
    -- * Simple Pretty-Printing
    encodePretty, encodePrettyToTextBuilder,

    -- * Pretty-Printing with Configuration Options
    encodePretty', encodePrettyToTextBuilder',
    Config (..), defConfig,
    Indent(..), NumberFormat(..),
    -- ** Sorting Keys in Objects
    -- |With the Aeson library, the order of keys in objects is undefined due to
    -- objects being implemented as HashMaps or KeyMaps. To allow user-specified
    -- key orders in the pretty-printed JSON, 'encodePretty'' can be configured
    -- with a modifying function. With sortValue both objects and arrays are
    -- sorted.
    sortValue
) where

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
#endif
import Data.Aeson (Value(..), ToJSON(..))
import Data.Aeson.Extra (ValueF(..))
import Data.Functor.Foldable (cata, embed)
import Data.Maybe (fromMaybe)
import Data.Sequences (sort)
import qualified Data.Aeson.Text as Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as H
  ( fromList, toList
#if !MIN_VERSION_aeson(2,0,0)
  , HashMap
#endif
  )
import Data.List (intersperse)
#if !MIN_VERSION_base(4,13,0)
import Data.Semigroup ((<>))
#endif
import qualified Data.Scientific as S (Scientific, FPFormat(..))
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Text.Lazy.Builder.Scientific (formatScientificBuilder)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Vector as V (fromList, toList)
import Prelude ()
import Prelude.Compat

-- | Sorts the keys of objects and the elements of arrays recursively.
sortValue :: Value -> Value
sortValue = cata (embed . f) where
  f (ObjectF a) = ObjectF (H.fromList . sort $ H.toList a)
  f (ArrayF xs) = ArrayF (V.fromList . sort $ V.toList xs)
  f x = x

data PState = PState { pLevel     :: Int
                     , pIndent    :: Builder
                     , pNewline   :: Builder
                     , pItemSep   :: Builder
                     , pKeyValSep :: Builder
                     , pNumFormat :: NumberFormat
                     , pModify    :: Maybe (Value -> Value)
                     }

-- | Indentation per level of nesting. @'Spaces' 0@ removes __all__ whitespace
--   from the output.
data Indent = Spaces Int | Tab

data NumberFormat
  -- | The standard behaviour of the 'Aeson.encode' function. Uses
  --   integer literals for integers (1, 2, 3...), simple decimals
  --   for fractional values between 0.1 and 9,999,999, and scientific
  --   notation otherwise.
  = Generic
  -- | Scientific notation (e.g. 2.3e123).
  | Scientific
  -- | Standard decimal notation
  | Decimal
  -- | Custom formatting function
  | Custom (S.Scientific -> Builder)

data Config = Config
    { confIndent  :: Indent
      -- ^ Indentation per level of nesting
    , confNumFormat :: NumberFormat
    , confTrailingNewline :: Bool
      -- ^ Whether to add a trailing newline to the output
    , confModify :: Maybe (Value -> Value)
      -- ^ Modify the value before encoding
    }

-- |The default configuration: indent by four spaces per level of nesting, sort 
-- values and do not add trailing newline.
--
--  > defConfig = Config { confIndent = Spaces 4, confNumFormat = Generic, confTrailingNewline = False, confModify = id }
defConfig :: Config
defConfig =
  Config {confIndent = Spaces 4, confNumFormat = Generic, confTrailingNewline = False, confModify = Just sortValue}

-- |A drop-in replacement for aeson's 'Aeson.encode' function, producing
--  JSON-ByteStrings for human readers.
--
--  Follows the default configuration in 'defConfig'.
encodePretty :: ToJSON a => a -> ByteString
encodePretty = encodePretty' defConfig

-- |A variant of 'encodePretty' that takes an additional configuration
--  parameter.
encodePretty' :: ToJSON a => Config -> a -> ByteString
encodePretty' conf = encodeUtf8 . toLazyText . encodePrettyToTextBuilder' conf

-- |A drop-in replacement for aeson's 'Aeson.encodeToTextBuilder' function,
--  producing JSON-ByteStrings for human readers.
--
--  Follows the default configuration in 'defConfig'.
encodePrettyToTextBuilder :: ToJSON a => a -> Builder
encodePrettyToTextBuilder = encodePrettyToTextBuilder' defConfig

-- |A variant of 'Aeson.encodeToTextBuilder' that takes an additional configuration
--  parameter.
encodePrettyToTextBuilder' :: ToJSON a => Config -> a -> Builder
encodePrettyToTextBuilder' Config{..} x =
  fromValue st (topModify $ toJSON x) <> trail
  where
    topModify = fromMaybe sortValue confModify

    st      = PState 0 indent newline itemSep kvSep confNumFormat confModify
    indent  = case confIndent of
                Spaces n -> mconcat (replicate n " ")
                Tab      -> "\t"
    newline = case confIndent of
                Spaces 0 -> ""
                _        -> "\n"
    itemSep = ","
    kvSep   = case confIndent of
                Spaces 0 -> ":"
                _        -> ": "
    trail   = if confTrailingNewline then "\n" else ""

fromValue :: PState -> Value -> Builder
fromValue st@PState{pModify = fromMaybe id -> modify} = go
  where
    go a@Array{} = let (Array v) = modify a in fromCompound st ("[","]") fromValue (V.toList v)
    go o@Object{} = let (Object m) = modify o in fromCompound st ("{","}") fromPair (toList' m)
    go (Number x) = fromNumber st x
    go v          = Aeson.encodeToTextBuilder v

#if MIN_VERSION_aeson(2,0,0)
toList' :: AKM.KeyMap b -> [(Text, b)]
toList' = fmap (\(k, v) -> (AK.toText k, v)) . AKM.toList
#else
toList' :: H.HashMap k v -> [(k, v)]
toList' = H.toList
#endif

fromCompound :: PState
             -> (Builder, Builder)
             -> (PState -> a -> Builder)
             -> [a]
             -> Builder
fromCompound st@PState{..} (delimL,delimR) fromItem items = mconcat
    [ delimL
    , if null items then mempty
        else pNewline <> items' <> pNewline <> fromIndent st
    , delimR
    ]
  where
    items' = mconcat . intersperse (pItemSep <> pNewline) $
                map (\item -> fromIndent st' <> fromItem st' item)
                    items
    st' = st { pLevel = pLevel + 1}

fromPair :: PState -> (Text, Value) -> Builder
fromPair st (k,v) =
  Aeson.encodeToTextBuilder (toJSON k) <> pKeyValSep st <> fromValue st v

fromIndent :: PState -> Builder
fromIndent PState{..} = mconcat (replicate pLevel pIndent)

fromNumber :: PState -> S.Scientific -> Builder
fromNumber st x = case pNumFormat st of
  Generic
    | (x > 1.0e19 || x < -1.0e19) -> formatScientificBuilder S.Exponent Nothing x
    | otherwise -> Aeson.encodeToTextBuilder $ Number x
  Scientific -> formatScientificBuilder S.Exponent Nothing x
  Decimal    -> formatScientificBuilder S.Fixed Nothing x
  Custom f   -> f x
