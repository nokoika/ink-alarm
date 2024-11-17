module SplaApiTest (test) where

import qualified Data.Aeson as A (eitherDecode)
import qualified Data.ByteString.Lazy as BL (intercalate)
import qualified SplaApi
  ( DefaultSchedule (..),
    EventMatch (..),
    EventSummary (..),
    Result (..),
    Root (..),
    Rule (..),
    Stage (..),
  )
import Test.Hspec (describe, hspec, it, shouldBe)
import Prelude (Bool (..), Either (..), IO, Maybe (..), String, ($))

test :: IO ()
test = hspec $ do
  describe "JSON Parser" $ do
    it "should parse JSON" $ do
      let jsonString =
            BL.intercalate
              "\n"
              [ "{",
                "  \"result\": {",
                "    \"regular\": [",
                "      {",
                "        \"start_time\": \"2024-11-17T21:00:00+09:00\",",
                "        \"end_time\": \"2024-11-17T23:00:00+09:00\",",
                "        \"rule\": {",
                "          \"key\": \"TURF_WAR\",",
                "          \"name\": \"ナワバリバトル\"",
                "        },",
                "        \"stages\": [",
                "          {",
                "            \"id\": 22,",
                "            \"name\": \"ネギトロ炭鉱\",",
                "            \"image\": \"https://example.com\"",
                "          }",
                "        ],",
                "        \"is_fest\": false",
                "      }",
                "    ],",
                "    \"bankara_open\": [",
                "      {",
                "        \"start_time\": \"2024-11-17T21:00:00+09:00\",",
                "        \"end_time\": \"2024-11-17T23:00:00+09:00\",",
                "        \"rule\": {",
                "          \"key\": \"AREA\",",
                "          \"name\": \"ガチエリア\"",
                "        },",
                "        \"stages\": [",
                "          {",
                "            \"id\": 9,",
                "            \"name\": \"ヒラメが丘団地\",",
                "            \"image\": \"https://example.com\"",
                "          }",
                "        ],",
                "        \"is_fest\": false",
                "      }",
                "    ],",
                "    \"bankara_challenge\": [",
                "      {",
                "        \"start_time\": \"2024-11-17T21:00:00+09:00\",",
                "        \"end_time\": \"2024-11-17T23:00:00+09:00\",",
                "        \"rule\": {",
                "          \"key\": \"GOAL\",",
                "          \"name\": \"ガチホコバトル\"",
                "        },",
                "        \"stages\": [",
                "          {",
                "            \"id\": 10,",
                "            \"name\": \"マサバ海峡大橋\",",
                "            \"image\": \"https://example.com\"",
                "          }",
                "        ],",
                "        \"is_fest\": false",
                "      }",
                "    ],",
                "    \"event\": [",
                "      {",
                "        \"start_time\": \"2024-11-20T11:00:00+09:00\",",
                "        \"end_time\": \"2024-11-20T13:00:00+09:00\",",
                "        \"rule\": {",
                "          \"key\": \"AREA\",",
                "          \"name\": \"ガチエリア\"",
                "        },",
                "        \"stages\": [",
                "          {",
                "            \"id\": 10,",
                "            \"name\": \"マサバ海峡大橋\",",
                "            \"image\": \"https://example.com\"",
                "          }",
                "        ],",
                "        \"event\": {",
                "          \"id\": \"FastMove\",",
                "          \"name\": \"イカダッシュバトル\",",
                "          \"desc\": \"イカダッシュ速度アップ！ イカやタコでステージを泳ぎ回れ！\"",
                "        },",
                "        \"is_fest\": false",
                "      }",
                "    ],",
                "    \"fest\": [",
                "      {",
                "        \"start_time\": \"2024-11-17T21:00:00+09:00\",",
                "        \"end_time\": \"2024-11-17T23:00:00+09:00\",",
                "        \"rule\": null,",
                "        \"stages\": null,",
                "        \"is_fest\": false,",
                "        \"is_tricolor\": false,",
                "        \"tricolor_stages\": null",
                "      }",
                "    ],",
                "    \"fest_challenge\": [",
                "      {",
                "        \"start_time\": \"2024-11-17T21:00:00+09:00\",",
                "        \"end_time\": \"2024-11-17T23:00:00+09:00\",",
                "        \"rule\": null,",
                "        \"stages\": null,",
                "        \"is_fest\": false,",
                "        \"is_tricolor\": false,",
                "        \"tricolor_stages\": null",
                "      }",
                "    ],",
                "    \"x\": [",
                "      {",
                "        \"start_time\": \"2024-11-17T21:00:00+09:00\",",
                "        \"end_time\": \"2024-11-17T23:00:00+09:00\",",
                "        \"rule\": {",
                "          \"key\": \"CLAM\",",
                "          \"name\": \"ガチアサリ\"",
                "        },",
                "        \"stages\": [",
                "          {",
                "            \"id\": 23,",
                "            \"name\": \"カジキ空港\",",
                "            \"image\": \"https://example.com\"",
                "          }",
                "        ],",
                "        \"is_fest\": false",
                "      }",
                "    ]",
                "  }",
                "}"
              ]
      let actual = A.eitherDecode $ jsonString :: Either String SplaApi.Root
      let expect =
            Right
              ( SplaApi.Root
                  { result =
                      SplaApi.Result
                        { regular =
                            [ SplaApi.DefaultSchedule
                                { startTime = "2024-11-17T21:00:00+09:00",
                                  endTime = "2024-11-17T23:00:00+09:00",
                                  rule = Just (SplaApi.Rule {key = "TURF_WAR", name = "ナワバリバトル"}),
                                  stages =
                                    Just
                                      [ SplaApi.Stage
                                          { id = 22,
                                            name = "ネギトロ炭鉱",
                                            image = "https://example.com"
                                          }
                                      ],
                                  isFest = False
                                }
                            ],
                          bankaraOpen =
                            [ SplaApi.DefaultSchedule
                                { startTime = "2024-11-17T21:00:00+09:00",
                                  endTime = "2024-11-17T23:00:00+09:00",
                                  rule = Just (SplaApi.Rule {key = "AREA", name = "ガチエリア"}),
                                  stages =
                                    Just
                                      [ SplaApi.Stage
                                          { id = 9,
                                            name = "ヒラメが丘団地",
                                            image = "https://example.com"
                                          }
                                      ],
                                  isFest = False
                                }
                            ],
                          bankaraChallenge =
                            [ SplaApi.DefaultSchedule
                                { startTime = "2024-11-17T21:00:00+09:00",
                                  endTime = "2024-11-17T23:00:00+09:00",
                                  rule = Just (SplaApi.Rule {key = "GOAL", name = "ガチホコバトル"}),
                                  stages =
                                    Just
                                      [ SplaApi.Stage
                                          { id = 10,
                                            name = "マサバ海峡大橋",
                                            image = "https://example.com"
                                          }
                                      ],
                                  isFest = False
                                }
                            ],
                          event =
                            [ SplaApi.EventMatch
                                { startTime = "2024-11-20T11:00:00+09:00",
                                  endTime = "2024-11-20T13:00:00+09:00",
                                  rule = (SplaApi.Rule {key = "AREA", name = "ガチエリア"}),
                                  stages =
                                    [ SplaApi.Stage
                                        { id = 10,
                                          name = "マサバ海峡大橋",
                                          image = "https://example.com"
                                        }
                                    ],
                                  event =
                                    SplaApi.EventSummary
                                      { id = "FastMove",
                                        name = "イカダッシュバトル",
                                        desc = "イカダッシュ速度アップ！ イカやタコでステージを泳ぎ回れ！"
                                      },
                                  isFest = False
                                }
                            ],
                          x =
                            [ SplaApi.DefaultSchedule
                                { startTime = "2024-11-17T21:00:00+09:00",
                                  endTime = "2024-11-17T23:00:00+09:00",
                                  rule = Just (SplaApi.Rule {key = "CLAM", name = "ガチアサリ"}),
                                  stages =
                                    Just
                                      [ SplaApi.Stage
                                          { id = 23,
                                            name = "カジキ空港",
                                            image = "https://example.com"
                                          }
                                      ],
                                  isFest = False
                                }
                            ]
                        }
                  }
              )

      actual `shouldBe` expect
