module Test.SplaApi.Client (test) where

import qualified SplaApi
import qualified SplaApi.Client as Client
import Test.Hspec
import qualified TestUtil as TU
import Prelude (Bool (False, True), Either (..), IO, Maybe (Just), ($))

test :: IO ()
test = hspec $ do
  describe "SplaApi.Client" $ do
    describe "MockSplaApiClient" $ do
      it "should return predefined success data" $ do
        let testApiData = createTestApiData
        let mockClient = Client.newMockClient (Right testApiData)

        result <- Client.fetchScheduleData mockClient

        result `shouldBe` Right testApiData

      it "should return predefined error message" $ do
        let errorMessage = "Test API error"
        let mockClient = Client.newMockClient (Left errorMessage)

        result <- Client.fetchScheduleData mockClient

        result `shouldBe` Left errorMessage

    describe "HttpSplaApiClient" $ do
      it "should initialize successfully" $ do
        client <- Client.newHttpClient
        -- Simple test to ensure initialization doesn't fail
        -- HttpSplaApiClient has no Show instance, so we just check it exists
        let _ = client
        True `shouldBe` True

-- Helper function to create test data
createTestApiData :: SplaApi.Root
createTestApiData =
  SplaApi.Root
    { SplaApi.result =
        SplaApi.Result
          { SplaApi.regular =
              [ SplaApi.DefaultSchedule
                  { SplaApi.startTime = TU.createUTCTime 2025 1 5 10 0,
                    SplaApi.endTime = TU.createUTCTime 2025 1 5 12 0,
                    SplaApi.rule = Just (SplaApi.Rule {SplaApi.key = SplaApi.TurfWar, SplaApi.name = "Turf War"}),
                    SplaApi.stages =
                      Just
                        [ SplaApi.Stage
                            { SplaApi.id = 1,
                              SplaApi.name = "Test Stage 1",
                              SplaApi.image = "http://example.com/stage1.png"
                            }
                        ],
                    SplaApi.isFest = False
                  }
              ],
            SplaApi.bankaraOpen = [],
            SplaApi.bankaraChallenge = [],
            SplaApi.x = [],
            SplaApi.event = []
          }
    }
