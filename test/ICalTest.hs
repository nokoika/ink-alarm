module ICalTest (test) where

import Data.List (intercalate)
import qualified ICal (ICalEvent (..), Reminder (..), ReminderAction (..), ReminderTrigger (..), buildICalText)
import Test.Hspec (describe, hspec, it, shouldBe)
import Prelude (IO, Maybe (Just), ($))

test :: IO ()
test = hspec $ do
  describe "buildICalText" $ do
    it "should create ics text" $ do
      let arg =
            [ ICal.ICalEvent
                { summary = "バンカラマッチ(オープン) - ヤガラ市場 / チョウザメ造船 - ガチヤグラ",
                  description = "ルール: ガチヤグラ\nステージ: ヤガラ市場, チョウザメ造船",
                  start = "20220914T040000Z", -- ここ、普通にISO8601にするか、あるいはRFC5545にするか悩む
                  end = "20220914T060000Z",
                  url = Just "https://example.com",
                  reminders =
                    [ ICal.Reminder
                        { trigger = ICal.ReminderTrigger {time = 10},
                          action = ICal.Display
                        },
                      ICal.Reminder
                        { trigger = ICal.ReminderTrigger {time = 30},
                          action = ICal.Display
                        }
                    ]
                },
              ICal.ICalEvent
                { summary = "イベントマッチ - タラポートショッピングパーク",
                  description = "イベント名: 新シーズン開幕記念カップ\nステージ: タラポートショッピングパーク",
                  start = "20220916T230000Z",
                  end = "20220917T010000Z",
                  url = Just "https://example.com",
                  reminders =
                    [ ICal.Reminder
                        { trigger = ICal.ReminderTrigger {time = 5},
                          action = ICal.Display
                        },
                      ICal.Reminder
                        { trigger = ICal.ReminderTrigger {time = 60},
                          action = ICal.Email
                        }
                    ]
                }
            ]
      let actual = ICal.buildICalText arg "JA"

      let expect =
            intercalate
              "\n"
              [ "BEGIN:VCALENDAR",
                "VERSION:2.0",
                "PRODID:-//Splatoon 3//JA",
                "BEGIN:VEVENT",
                "SUMMARY:バンカラマッチ(オープン) - ヤガラ市場 / チョウザメ造船 - ガチヤグラ",
                "DESCRIPTION:ルール: ガチヤグラ\nステージ: ヤガラ市場, チョウザメ造船",
                "DTSTART:20220914T040000Z",
                "DTEND:20220914T060000Z",
                "URL:https://example.com",
                "BEGIN:VALARM",
                "TRIGGER:-PT10M",
                "ACTION:DISPLAY",
                "END:VALARM",
                "BEGIN:VALARM",
                "TRIGGER:-PT30M",
                "ACTION:DISPLAY",
                "END:VALARM",
                "END:VEVENT",
                "BEGIN:VEVENT",
                "SUMMARY:イベントマッチ - タラポートショッピングパーク",
                "DESCRIPTION:イベント名: 新シーズン開幕記念カップ\nステージ: タラポートショッピングパーク",
                "DTSTART:20220916T230000Z",
                "DTEND:20220917T010000Z",
                "URL:https://example.com",
                "BEGIN:VALARM",
                "TRIGGER:-PT5M",
                "ACTION:DISPLAY",
                "END:VALARM",
                "BEGIN:VALARM",
                "TRIGGER:-PT60M",
                "ACTION:EMAIL",
                "END:VALARM",
                "END:VEVENT",
                "END:VCALENDAR"
              ]
      actual `shouldBe` expect
