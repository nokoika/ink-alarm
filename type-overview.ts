// JSONのスキーマ定義（base64URIのデコード結果として解釈する内容）
type SplatoonStageScheduleQuery = {
  language: "ja" | "en"; // 出力言語指定（日本語・英語）
  filters: FilterCondition[]; // 出力する試合のフィルタ条件リスト（ANDで結合）
};

type FilterCondition =
  | {
      matchType: "bankara_open" | "bankara_challenge" | "x";
      stages?: {
        matchBothStages: boolean; // 指定ステージが両方含まれている場合かどちらか含まれている場合かの条件
        stageIds: number[]; // ステージIDのリスト。1~20の範囲で指定
      };
      rules?: ("TURF_WAR" | "AREA" | "LOFT" | "GOAL" | "CLAM")[]; // 試合のルール指定（複数可）
      timeSlots?: TimeSlot[]; // 出力する時間帯指定（複数可）
      notifications?: NotificationSetting[]; // 通知設定（複数可）
    }
  | {
      matchType: "regular" | "event";
      timeSlots?: TimeSlot[]; // 出力する時間帯指定（複数可）
      notifications?: NotificationSetting[]; // 通知設定（複数可）
    };

type TimeSlot = {
  start: string;
  end: string;
};

type NotificationSetting = {
  minutesBefore: number; // 通知時間（試合開始前の何分前に通知するか）
};

// iCalのスキーマ（JSONから生成する予定のフォーマット）
type ICalEvent = {
  summary: string; // イベント名
  description: string; // 説明（ステージ名やルール名を含む）
  start: string; // 開始日時（ISO 8601形式）
  end: string; // 終了日時（ISO 8601形式）
  url?: string; // ステージ画像URL
  reminders: Reminder[]; // 通知設定（複数可）
};

type Reminder = {
  trigger: string; // 通知を行うタイミング（例: '-PT15M' => 15分前）
};

// --- サンプル ---

// 1. 入力JSONサンプル
const sampleQuery: SplatoonStageScheduleQuery = {
  language: "ja",
  filters: [
    {
      matchType: "bankara_open",
      stages: {
        matchBothStages: false,
        stageIds: [3, 14],
      },
      rules: ["LOFT"],
      timeSlots: [
        {
          start: "13:00",
          end: "15:00",
        },
        {
          start: "21:00",
          end: "01:00",
        },
      ],
      notifications: [{ minutesBefore: 10 }, { minutesBefore: 30 }],
    },
    {
      matchType: "x",
      rules: ["AREA", "CLAM"],
      timeSlots: [
        {
          start: "10:00",
          end: "12:00",
        },
        {
          start: "18:00",
          end: "20:00",
        },
      ],
      notifications: [{ minutesBefore: 15 }],
    },
    {
      matchType: "event",
      timeSlots: [
        {
          start: "08:00",
          end: "10:00",
        },
      ],
      notifications: [{ minutesBefore: 5 }, { minutesBefore: 20 }],
    },
  ],
};

// 2. iCalイベント出力サンプル
const sampleICalEvents: ICalEvent[] = [
  {
    summary:
      "バンカラマッチ(オープン) - ヤガラ市場 / チョウザメ造船 - ガチヤグラ",
    description: "ルール: ガチヤグラ\nステージ: ヤガラ市場, チョウザメ造船",
    start: "2022-09-14T13:00:00+09:00",
    end: "2022-09-14T15:00:00+09:00",
    url: "https://xxxxxxxx",
    reminders: [{ trigger: "-PT10M" }, { trigger: "-PT30M" }],
  },
  {
    summary: "イベントマッチ - タラポートショッピングパーク",
    description:
      "イベント名: 新シーズン開幕記念カップ\nステージ: タラポートショッピングパーク",
    start: "2022-09-16T08:00:00+09:00",
    end: "2022-09-16T10:00:00+09:00",
    url: "https://xxxxxxxx",
    reminders: [{ trigger: "-PT10M" }, { trigger: "-PT30M" }],
  },
];

// 3. iCalファイル出力例 (2つの例)
const sampleICS: string[] = [
  `BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Splatoon 3//JA
BEGIN:VEVENT
SUMMARY:バンカラマッチ(オープン) - ヤガラ市場 / チョウザメ造船 - ガチヤグラ
DESCRIPTION:ルール: ガチヤグラ\nステージ: ヤガラ市場, チョウザメ造船
DTSTART:20220914T040000Z
DTEND:20220914T060000Z
URL:https://xxxxxxxx
BEGIN:VALARM
TRIGGER:-PT10M
ACTION:DISPLAY
DESCRIPTION:Reminder
END:VALARM
BEGIN:VALARM
TRIGGER:-PT30M
ACTION:DISPLAY
DESCRIPTION:Reminder
END:VALARM
END:VEVENT
BEGIN:VEVENT
SUMMARY:イベントマッチ - タラポートショッピングパーク
DESCRIPTION:イベント名: 新シーズン開幕記念カップ\nステージ: タラポートショッピングパーク
DTSTART:20220916T230000Z
DTEND:20220917T010000Z
URL:https://xxxxxxxx
BEGIN:VALARM
TRIGGER:-PT5M
ACTION:DISPLAY
DESCRIPTION:Reminder
END:VALARM
BEGIN:VALARM
TRIGGER:-PT20M
ACTION:DISPLAY
DESCRIPTION:Reminder
END:VALARM
END:VEVENT
END:VCALENDAR`,
];
