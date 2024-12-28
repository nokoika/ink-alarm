import { FC } from 'react';

const RulesFilter: FC = () => {
  return (
    <div className="flex gap-2">
      {['ガチエリア', 'ガチヤグラ', 'ガチホコバトル', 'ガチアサリ'].map(
        (option, index) => (
          <label key={index} className="relative cursor-pointer">
            <input type="checkbox" className="sr-only peer" />
            <span className="block px-4 py-2 text-gray-700 bg-gray-200 rounded peer-checked:bg-blue-500 peer-checked:text-white transition-colors duration-200 hover:bg-gray-300">
              {option}
            </span>
          </label>
        ),
      )}
    </div>
  );
};

const ModesFilter: FC = () => {
  return (
    <div className="flex gap-2">
      {[
        'Xマッチ',
        'イベントマッチ',
        'バンカラマッチ (オープン)',
        'バンカラマッチ (チャレンジ)',
        'レギュラーマッチ',
      ].map((option, index) => (
        <label key={index} className="relative cursor-pointer">
          <input type="checkbox" className="sr-only peer" />
          <span className="block px-4 py-2 text-gray-700 bg-gray-200 rounded peer-checked:bg-blue-500 peer-checked:text-white transition-colors duration-200 hover:bg-gray-300">
            {option}
          </span>
        </label>
      ))}
    </div>
  );
};

const TimeSlotFilter: FC = () => {
  const daysOfWeek = ['月', '火', '水', '木', '金', '土', '日'];
  return (
    <div className="flex gap-4">
      <div className="flex items-center gap-2">
        <input
          type="time"
          className="px-3 py-2 border border-gray-300 rounded focus:outline-none focus:border-blue-500 focus:ring-1 focus:ring-blue-500"
          defaultValue="19:00"
        />
        <span className="text-gray-700">~</span>
        <input
          type="time"
          className="px-3 py-2 border border-gray-300 rounded focus:outline-none focus:border-blue-500 focus:ring-1 focus:ring-blue-500"
          defaultValue="00:00"
        />
      </div>
      <div className="flex flex-wrap gap-2">
        {daysOfWeek.map((day, index) => (
          <label key={index} className="cursor-pointer">
            {/* 実際のチェックボックスは視覚的に隠して、状態によって見た目を変える */}
            <input
              type="checkbox"
              className="sr-only peer"
              // onChangeなどのイベントはあえて入れずに状態管理を排除
            />
            <span className="block px-3 py-1 text-gray-700 bg-gray-200 rounded peer-checked:bg-blue-500 peer-checked:text-white hover:bg-gray-100 transition-colors">
              {day}
            </span>
          </label>
        ))}
      </div>
    </div>
  );
};

const StageFilter: FC = () => {
  // ステージID -> ステージ名 のマッピングを配列で定義
  const stages = [
    { id: 1, name: 'ユノハナ大渓谷' },
    { id: 2, name: 'ゴンズイ地区' },
    { id: 3, name: 'ヤガラ市場' },
    { id: 4, name: 'マテガイ放水路' },
    { id: 5, name: 'ナンプラー遺跡' },
    { id: 6, name: 'ナメロウ金属' },
    { id: 7, name: 'クサヤ温泉' },
    { id: 8, name: 'タラポートショッピングパーク' },
    { id: 9, name: 'ヒラメが丘団地' },
    { id: 10, name: 'マサバ海峡大橋' },
    { id: 11, name: 'キンメダイ美術館' },
    { id: 12, name: 'マヒマヒリゾート＆スパ' },
    { id: 13, name: '海女美術大学' },
    { id: 14, name: 'チョウザメ造船' },
    { id: 15, name: 'ザトウマーケット' },
    { id: 16, name: 'スメーシーワールド' },
    { id: 17, name: 'コンブトラック' },
    { id: 18, name: 'マンタマリア号' },
    { id: 19, name: 'タカアシ経済特区' },
    { id: 20, name: 'オヒョウ海運' },
    { id: 21, name: 'バイガイ亭' },
    { id: 22, name: 'ネギトロ炭鉱' },
    { id: 23, name: 'カジキ空港' },
    { id: 24, name: 'リュウグウターミナル' },
  ];

  return (
    <div className="grid grid-cols-4 gap-2">
      {stages.map((stage) => (
        <label key={stage.id} className="relative cursor-pointer">
          <input type="checkbox" className="sr-only peer" />
          <span className="block px-3 py-2 text-sm text-center text-gray-700 bg-gray-200 rounded peer-checked:bg-blue-500 peer-checked:text-white hover:bg-gray-300 transition-colors">
            {stage.name}
          </span>
        </label>
      ))}
    </div>
  );
};

const App: FC = () => {
  return (
    <div className="space-y-2">
      <RulesFilter />
      <ModesFilter />
      <TimeSlotFilter />
      <StageFilter />
    </div>
  );
};

export default App;
