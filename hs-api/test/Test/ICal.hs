module Test.ICal (test) where

import Data.List (intercalate)
import qualified ICal (ICalEvent (..), ICalInput (..), buildICalText)
import qualified Query as Q
import Test.Hspec (describe, hspec, it, shouldBe)
import qualified TestUtil as TU
import Prelude (IO, ($))

test :: IO ()
test = hspec $ do
  describe "buildICalText" $ do
    it "should create ics text" $ do
      let arg =
            ICal.ICalInput
              { ICal.language = Q.Japanese,
                ICal.events =
                  [ ICal.ICalEvent
                      { id = "ID1",
                        summary = "バンカラマッチ(オープン) - ヤガラ市場 / チョウザメ造船 - ガチヤグラ",
                        description = "ルール: ガチヤグラ\\nステージ: ヤガラ市場, チョウザメ造船",
                        start = TU.createUTCTime 2022 9 14 4 0,
                        end = TU.createUTCTime 2022 9 14 6 0
                      },
                    ICal.ICalEvent
                      { id = "ID2",
                        summary = "イベントマッチ - タラポートショッピングパーク",
                        description = "イベント名: 新シーズン開幕記念カップ\\nステージ: タラポートショッピングパーク",
                        start = TU.createUTCTime 2022 9 16 23 0,
                        end = TU.createUTCTime 2022 9 17 1 0
                      }
                  ]
              }
      let actual = ICal.buildICalText arg

      let expect =
            intercalate
              "\n"
              [ "BEGIN:VCALENDAR",
                "VERSION:2.0",
                "PRODID:-//nokoika//ガチアラーム github.com/nokoika/ink-alarm//JA",
                "METHOD:PUBLISH",
                "CALSCALE:GREGORIAN",
                "X-WR-CALNAME:ガチアラーム",
                "BEGIN:VEVENT",
                "UID:ID1",
                "SUMMARY:バンカラマッチ(オープン) - ヤガラ市場 / チョウザメ造船 - ガチヤグラ",
                "DESCRIPTION:ルール: ガチヤグラ\\nステージ: ヤガラ市場, チョウザメ造船",
                "DTSTART:20220914T040000Z",
                "DTEND:20220914T060000Z",
                "END:VEVENT",
                "BEGIN:VEVENT",
                "UID:ID2",
                "SUMMARY:イベントマッチ - タラポートショッピングパーク",
                "DESCRIPTION:イベント名: 新シーズン開幕記念カップ\\nステージ: タラポートショッピングパーク",
                "DTSTART:20220916T230000Z",
                "DTEND:20220917T010000Z",
                "END:VEVENT",
                "END:VCALENDAR"
              ]
      actual `shouldBe` expect
