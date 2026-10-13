module Web.UAParser
  ( -- * Parsing browser (user agent)
    parseUA
  , parseUALenient
  , UAResult (..)
  , uarVersion

    -- * Parsing OS
  , parseOS
  , parseOSLenient
  , OSResult (..)
  , osrVersion

    -- * Parsing Dev
  , parseDev
  , parseDevLenient
  , DevResult (..)
  ) where

import Prelude

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Data.Aeson
import Data.ByteString.Char8 (ByteString)
import Data.Data
import Data.Default
import Data.FileEmbed
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Monoid as M
import Data.Serialize
import Data.Serialize.Text ()
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error (lenientDecode)
import Data.Yaml
import GHC.Generics
import Text.Regex.Pcre2 (Option (Caseless), capturesOpt)

-- UA Parser
uaConfig :: UAConfig
uaConfig =
  either (error . show) id
    $ decodeEither' $(embedFile "deps/uap-core/regexes.yaml")
{-# NOINLINE uaConfig #-}

-- | Parser that, upon failure to match a pattern returns a result of
-- family "Other" with all other fields blank. This is mainly for
-- compatibility with the uap-core test suite
parseUALenient :: ByteString -> UAResult
parseUALenient = fromMaybe def . parseUA

-- | Parse a given User-Agent string
parseUA :: ByteString -> Maybe UAResult
parseUA bs = msum $ map go uaParsers
 where
  UAConfig {uaParsers} = uaConfig
  subject = decodeSubject bs

  go UAParser {uaMatch, uaFamRep, uaV1Rep, uaV2Rep, uaV3Rep} =
    normalize . mkRes . NE.toList <$> uaMatch subject
   where
    normalize (UAResult f v1 v2 v3) =
      UAResult
        f
        (normalizeMaybeText v1)
        (normalizeMaybeText v2)
        (normalizeMaybeText v3)
    normalizeMaybeText (Just "") = Nothing
    normalizeMaybeText x = x

    mkRes caps =
      UAResult
        (repF caps (familyCap caps))
        (repV1 caps)
        (repV2 caps)
        (repV3 caps)

    repV1 caps = maybe (capture caps 2) Just (makeReplacements caps <$> uaV1Rep)
    repV2 caps = maybe (capture caps 3) Just (makeReplacements caps <$> uaV2Rep)
    repV3 caps = maybe (capture caps 4) Just (makeReplacements caps <$> uaV3Rep)

    repF caps x = maybe x (makeReplacements caps) uaFamRep

-- | Results datatype for the parsed User-Agent
data UAResult = UAResult
  { uarFamily :: Text
  , uarV1 :: Maybe Text
  , uarV2 :: Maybe Text
  , uarV3 :: Maybe Text
  }
  deriving (Data, Eq, Generic, NFData, Read, Serialize, Show, Typeable)

-- | Construct a browser version-string from 'UAResult'
uarVersion :: UAResult -> Text
uarVersion UAResult {uarV1, uarV2, uarV3} =
  T.intercalate "." . catMaybes . takeWhile isJust $ [uarV1, uarV2, uarV3]

instance Default UAResult where
  def = UAResult "Other" Nothing Nothing Nothing

-- OS Parser

-- | Parser that, upon failure to match a pattern returns a result of
-- family "Other" with all other fields blank. This is mainly for
-- compatibility with the uap-core test suite
parseOSLenient :: ByteString -> OSResult
parseOSLenient = fromMaybe def . parseOS

-- | Parse OS from given User-Agent string
parseOS :: ByteString -> Maybe OSResult
parseOS bs = msum $ map go osParsers
 where
  UAConfig {osParsers} = uaConfig
  subject = decodeSubject bs

  go OSParser {osMatch, osFamRep, osRep1, osRep2, osRep3, osRep4} =
    mkRes . NE.toList <$> osMatch subject
   where
    mkRes caps =
      OSResult
        (repF caps (familyCap caps))
        (repV1 caps)
        (repV2 caps)
        (repV3 caps)
        (repV4 caps)

    repF caps x = maybe x (makeReplacements caps) osFamRep

    repV1 caps = maybe (capture caps 2) Just (makeReplacements caps <$> osRep1)
    repV2 caps = maybe (capture caps 3) Just (makeReplacements caps <$> osRep2)
    repV3 caps = maybe (capture caps 4) Just (makeReplacements caps <$> osRep3)
    repV4 caps = maybe (capture caps 5) Just (makeReplacements caps <$> osRep4)

-- | Result type for 'parseOS'
data OSResult = OSResult
  { osrFamily :: Text
  , osrV1 :: Maybe Text
  , osrV2 :: Maybe Text
  , osrV3 :: Maybe Text
  , osrV4 :: Maybe Text
  }
  deriving (Data, Eq, Generic, NFData, Read, Serialize, Show, Typeable)

instance Default OSResult where
  def = OSResult "Other" Nothing Nothing Nothing Nothing

-- | Construct a version string from 'OSResult'
osrVersion :: OSResult -> Text
osrVersion OSResult {osrV1, osrV2, osrV3, osrV4} =
  T.intercalate "." . catMaybes . takeWhile isJust $ [osrV1, osrV2, osrV3, osrV4]

-- Dev Parser

-- | Parser that, upon failure to match a pattern returns a result of
-- family "Other" with all other fields blank. This is mainly for
-- compatibility with the uap-core test suite
parseDevLenient :: ByteString -> DevResult
parseDevLenient = fromMaybe def . parseDev

parseDev :: ByteString -> Maybe DevResult
parseDev bs = msum $ map go devParsers
 where
  UAConfig {devParsers} = uaConfig
  subject = decodeSubject bs

  go DevParser {devMatch, devFamRep, devBrandRep, devModelRep} =
    mkRes . NE.toList <$> devMatch subject
   where
    mkRes caps =
      mkDR (repF caps (familyCap caps)) (repBrand caps) (repModel caps)

    mkDR a b c = DevResult (T.strip a) (strip' =<< b) (strip' =<< c)

    strip' t = case T.strip t of
      "" -> Nothing
      t' -> Just t'

    repBrand caps =
      maybe (capture caps 2) Just (makeReplacements caps <$> devBrandRep)
    -- This technique is used in the python ua-parser. It isn't
    -- clear if there's a precedent in the spec but it clears up
    -- some remote edge cases (which may be test suite bugs TBH).
    repModel caps =
      maybe
        (capture caps 3 <|> capture caps 1)
        Just
        (makeReplacements caps <$> devModelRep)

    repF caps x = maybe x (makeReplacements caps) devFamRep

-- | Replace replacement placeholders with captures and remove any
-- that are unused. Goes up to $4 as per the spec
makeReplacements
  :: [Text]
  -- ^ Captures
  -> Text
  -- ^ Replacement text with 1-indexed replace points ($1, $2, $3 or $4)
  -> Text
makeReplacements (_ : cs) t = makeReplacements' (zip ([1 .. 4] :: [Int]) (cs ++ repeat "")) t
 where
  makeReplacements' [] acc = acc
  makeReplacements' ((idx, cap) : caps) acc =
    let acc' = T.replace ("$" M.<> showT idx) cap acc
    in  makeReplacements' caps acc'
makeReplacements _ t = t

showT :: Show a => a -> Text
showT = T.pack . show

-- | Result type for 'parseDev'
data DevResult = DevResult
  { drFamily :: Text
  , drBrand :: Maybe Text
  , drModel :: Maybe Text
  }
  deriving (Data, Eq, Generic, NFData, Read, Serialize, Show, Typeable)

instance Default DevResult where
  def = DevResult "Other" Nothing Nothing

-- Parser Config

-- | User-Agent string parser data
data UAConfig = UAConfig
  { uaParsers :: [UAParser]
  , osParsers :: [OSParser]
  , devParsers :: [DevParser]
  }

data UAParser = UAParser
  { uaMatch :: Text -> Maybe (NonEmpty Text)
  , uaFamRep :: Maybe Text
  , uaV1Rep :: Maybe Text
  , uaV2Rep :: Maybe Text
  , uaV3Rep :: Maybe Text
  }

data OSParser = OSParser
  { osMatch :: Text -> Maybe (NonEmpty Text)
  , osFamRep :: Maybe Text
  , osRep1 :: Maybe Text
  , osRep2 :: Maybe Text
  , osRep3 :: Maybe Text
  , osRep4 :: Maybe Text
  }

data DevParser = DevParser
  { devMatch :: Text -> Maybe (NonEmpty Text)
  , devFamRep :: Maybe Text
  , devBrandRep :: Maybe Text
  , devModelRep :: Maybe Text
  }

parseRegex :: Object -> Parser (Text -> Maybe (NonEmpty Text))
parseRegex v = do
  pat <- v .: "regex"
  flag <- v .:? "regex_flag" :: Parser (Maybe Text)
  let opts = case flag of
        Just "i" -> Caseless
        _ -> mempty
  return (capturesOpt opts pat)

instance FromJSON UAConfig where
  parseJSON (Object v) =
    UAConfig
      <$> v .: "user_agent_parsers"
      <*> v .: "os_parsers"
      <*> v .: "device_parsers"
  parseJSON _ = error "Object expected when parsing JSON"

instance FromJSON UAParser where
  parseJSON (Object v) =
    UAParser
      <$> parseRegex v
      <*> v .:? "family_replacement"
      <*> v .:? "v1_replacement"
      <*> v .:? "v2_replacement"
      <*> v .:? "v3_replacement"
  parseJSON _ = error "Object expected when parsing JSON"

instance FromJSON OSParser where
  parseJSON (Object v) =
    OSParser
      <$> parseRegex v
      <*> v .:? "os_replacement"
      <*> v .:? "os_v1_replacement"
      <*> v .:? "os_v2_replacement"
      <*> v .:? "os_v3_replacement"
      <*> v .:? "os_v4_replacement"
  parseJSON _ = error "Object expected when parsing JSON"

instance FromJSON DevParser where
  parseJSON (Object v) = do
    r <- parseRegex v
    fam <- v .:? "device_replacement"
    brandRep <- v .:? "brand_replacement"
    modRep <- v .:? "model_replacement"
    return
      ( DevParser
          { devMatch = r
          , devFamRep = fam
          , devBrandRep = brandRep
          , devModelRep = modRep
          }
      )
  parseJSON _ = error "Object expected when parsing JSON"

decodeSubject :: ByteString -> Text
decodeSubject = T.decodeUtf8With lenientDecode

-- | A capture group by index (0 is the whole match). Groups that are absent,
-- unset or empty are all 'Nothing'.
capture :: [Text] -> Int -> Maybe Text
capture caps n = case drop n caps of
  t : _ | not (T.null t) -> Just t
  _ -> Nothing

-- | The first capture group, or the whole match when the pattern has none
familyCap :: [Text] -> Text
familyCap caps = case caps of
  _ : f : _ -> f
  whole : _ -> whole
  [] -> ""
