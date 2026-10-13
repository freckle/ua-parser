module Main where

import Prelude

import Control.Applicative as A
import Data.ByteString.Char8 qualified as B
import Data.Monoid as M
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit
import Web.UAParser
import Web.UAParser.SuiteUtils

main :: IO ()
main = do
  uaCases <-
    M.mconcat
      A.<$> mapM
        loadTests
        [ "test_resources/firefox_user_agent_strings.yaml"
        , "test_resources/pgts_browser_list.yaml"
        , "tests/test_ua.yaml"
        , "test_resources/opera_mini_user_agent_strings.yaml"
        , "test_resources/podcasting_user_agent_strings.yaml"
        ]
  osCases <-
    mconcat
      <$> mapM
        loadTests
        [ "test_resources/additional_os_tests.yaml"
        , "tests/test_os.yaml"
        ]
  devCases <- loadTests "tests/test_device.yaml"
  defaultMain
    $ testGroup
      "ua-parser"
      [ uaTests uaCases
      , osTests osCases
      , devTests devCases
      , encodingTests
      ]

uaTests :: [UserAgentTestCase] -> TestTree
uaTests = testGroup "UA Parsing Tests" . map testUAParser

testUAParser :: UserAgentTestCase -> TestTree
testUAParser UATC {uatcString, uatcFamily, uatcV1, uatcV2, uatcV3} = testCase tn $ do
  assertEqual "family is same" uatcFamily uarFamily
  assertEqual "v1 is the same" uatcV1 uarV1
  assertEqual "v2 is the same" uatcV2 uarV2
  assertEqual "v3 is the same" uatcV3 uarV3
 where
  UAResult {uarFamily, uarV1, uarV2, uarV3} = parseUALenient uatcString
  tn =
    T.unpack
      $ T.intercalate "/" ["UA Test: ", uatcFamily, m uatcV1, m uatcV2, m uatcV3]
  m x = maybe "-" id x

-- OS Testing
osTests :: [OSTestCase] -> TestTree
osTests = testGroup "OS Parsing Tests" . map testOSParser

testOSParser :: OSTestCase -> TestTree
testOSParser OSTC {ostcString, ostcFamily, ostcV1, ostcV2, ostcV3, ostcV4} = testCase tn $ do
  assertEqual "family is same" ostcFamily osrFamily
  assertEqual "major is the same" ostcV1 osrV1
  assertEqual "minor is the same" ostcV2 osrV2
  assertEqual "patch is the same" ostcV3 osrV3
  assertEqual "patch_minor is the same" ostcV4 osrV4
 where
  OSResult {osrFamily, osrV1, osrV2, osrV3, osrV4} = parseOSLenient ostcString
  tn = B.unpack ostcString <> " - " <> T.unpack summary
  summary =
    T.intercalate
      "/"
      [ "OS Test: "
      , ostcFamily
      , m ostcV1
      , m ostcV2
      , m ostcV3
      , m ostcV4
      ]
  m x = maybe "-" id x

-- Dev Testing
devTests :: [DevTestCase] -> TestTree
devTests = testGroup "Dev Parsing Tests" . map testDevParser

testDevParser :: DevTestCase -> TestTree
testDevParser DTC {dtcString, dtcFamily, dtcBrand, dtcModel} = testCase tn $ do
  assertEqual "family is same" dtcFamily drFamily
  assertEqual "brand is the same" dtcBrand drBrand
  assertEqual "model is the same" dtcModel drModel
 where
  DevResult {drFamily, drBrand, drModel} = parseDevLenient dtcString
  tn = B.unpack dtcString <> " - " <> T.unpack summary
  summary =
    T.intercalate
      "/"
      [ "Dev Test: "
      , dtcFamily
      , m dtcBrand
      , m dtcModel
      ]
  m x = maybe "-" id x

-- Input that is not ASCII, or not UTF-8 at all, is outside what the shared
-- uap-core test cases (deps/uap-core) cover. The subject is decoded as UTF-8
-- before matching, leniently, so invalid bytes become U+FFFD.
encodingTests :: TestTree
encodingTests =
  testGroup
    "Encoding"
    [ testCase "invalid UTF-8 in a captured family is replaced and still matches"
        $ parseUA "77 WABC\xff/11 CFNetwork/758.3.15 Darwin/15.4.0"
          @?= Just
            UAResult
              { uarFamily = "77 WABC\xfffd"
              , uarV1 = Just "11"
              , uarV2 = Nothing
              , uarV3 = Nothing
              }
    , testCase "invalid UTF-8 in a captured device is replaced and still matches"
        $ parseDev "Amazon.com/2.8.0 (Android/2.3.5/HTC\xe9 Desire HD A9191)"
          @?= Just
            DevResult
              { drFamily = "HTC \xfffd Desire HD A9191"
              , drBrand = Just "HTC"
              , drModel = Just "\xfffd Desire HD A9191"
              }
    , testCase "invalid UTF-8 does not prevent an OS match"
        $ parseOS "App/1.0.0 CFNetwork/897.\xc3\&5 Darwin/17.5.0"
          @?= Just
            OSResult
              { osrFamily = "iOS"
              , osrV1 = Just "11"
              , osrV2 = Just "3"
              , osrV3 = Nothing
              , osrV4 = Nothing
              }
    , testCase "invalid UTF-8 does not change which OS version is captured"
        $ parseOS
          "Box \xc3Sync/4.0.7848;Windows/10;Intel64 Family 6 Model 158 Stepping 9, GenuineIntel/64bit"
          @?= Just
            OSResult
              { osrFamily = "Windows"
              , osrV1 = Just "10"
              , osrV2 = Nothing
              , osrV3 = Nothing
              , osrV4 = Nothing
              }
    , testCase "a multibyte character ending a family name is part of the match"
        $ parseUA "FollowSit\xc3\xa9 Bot ( http://www.followsite.com/bot.html )"
          @?= Just
            UAResult
              { uarFamily = "FollowSit\xe9 Bot"
              , uarV1 = Nothing
              , uarV2 = Nothing
              , uarV3 = Nothing
              }
    , testCase "a multibyte character is matched as one character"
        $ parseUA "8bo Crawle\xe6\x97\xa5 Bot"
          @?= Just
            UAResult
              { uarFamily = "bo Crawle\x65e5 Bot"
              , uarV1 = Nothing
              , uarV2 = Nothing
              , uarV3 = Nothing
              }
    , testCase "a multibyte character inside a device name is part of the match"
        $ parseDev
          "Android 4.0.4;AppleWebKit/534.30;Build/IMM76D;T-\xf0\x9f\x98\x80\&70 Build/IMM76D"
          @?= Just
            DevResult
              { drFamily = "T-\x1f600\&70"
              , drBrand = Just "Audiosonic"
              , drModel = Just "T-\x1f600\&70"
              }
    , testCase "trailing invalid UTF-8 after a complete match is ignored"
        $ parseUA
          "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36 \xff"
          @?= Just
            UAResult
              { uarFamily = "Chrome"
              , uarV1 = Just "91"
              , uarV2 = Just "0"
              , uarV3 = Just "4472"
              }
    ]
