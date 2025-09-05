# ガチアラーム (Ink Alarm)

Splatoon 3のゲームスケジュールをiCalendar形式で配信するWebサービス\
[ink-alarm.pages.dev](https://ink-alarm.pages.dev/)

## 概要

ガチアラームは、Splatoon 3のスケジュール（レギュラーマッチ、バンカラマッチ、Xマッチ、イベントマッチなど）を取得し、フィルタリングしてiCalendar形式で提供するツールです。\
Google CalendarやiCalendarなどのカレンダーアプリに登録することで、お気に入りのステージやルールの通知を受け取ることができます。

## 技術スタック

### Frontend
- React
- TypeScript
- Vite
- TailwindCSS
- Bun

### Backend
- Haskell
- Stack
- Scotty (HTTP)

### Linter
- Front: Biome
- Backend: ormolu + hlint
- precommit: lint-staged + simple-git-hooks

## セットアップ

### 必要な環境
- Frontend
    - Bun 1.1.42以上
- Backend
    - GHCup 推奨
    - Stack 3.1.1以上
    - GHC 9.6.6 (LTS 22.40)
    - HLS 2.9.0.1以上

### インストール手順

1. リポジトリをクローン
```bash
git clone https://github.com/nokoika/ink-alarm.git
cd ink-alarm
```

2. 依存関係のインストール
```bash
# ルートディレクトリで
bun install

# フロントエンド
cd front
bun install

# バックエンド
cd ../hs-api
stack build
```

## 開発

### Frontend

```bash
cd front
bun dev              # 開発サーバー起動（http://localhost:40000）
```

### Backend

```bash
cd hs-api
stack run                      # ビルド&実行（http://localhost:8080）
stack test                     # テスト実行
```

## API仕様

フロントと、Google Calendar や iCalendar などのカレンダーアプリが参照します。

### Endpoint

本番: `GET https://hs-api-709184628742.asia-northeast1.run.app/api/v2/ical?query={query}`
ローカル: `GET http://localhost:8080/api/v2/ical?query={query}`

### query

クエリはJSONをgzipしたものをBase64URLエンコードしたものです。\
JSONは[query-schema.json](https://github.com/nokoika/ink-alarm/blob/main/query-schema.json)のJSON Schemaに準拠する必要があります。

エンコード例:
```bash
# 1. JSONを作成
JSON='{"language":"ja","utcOffset":"+09:00","filters":[{"modes":["x"],"rules":["asari"],"timeSlots":[{"start":"20:00","end":"02:30"}]}]}'

# 2. gzipで圧縮してBase64URLエンコード
echo "$JSON" | gzip | base64 -w 0 | sed 's/+/-/g; s/\//_/g; s/=//g'

# 出力例: H4sICJDZumgAA3F1ZXJ5Lmpzb24AJYwxCsMwEAR7P2NbqzicKvqEC5fBxRGdjIIsg3SCgMnfIyG22R2WuRE5HZUPgcWHYVD1vXpfRBuY6WmJGvQhquQC-7pxXk56wxe7Qa5xLC6cQycaTtnipeNdlHNXLTRMklxbtNgH4be3TH_rsx8JgwAAAA

# 3. API呼び出し
curl "https://hs-api-709184628742.asia-northeast1.run.app/api/v2/ical?query=H4sICJDZumgAA3F1ZXJ5Lmpzb24AJYwxCsMwEAR7P2NbqzicKvqEC5fBxRGdjIIsg3SCgMnfIyG22R2WuRE5HZUPgcWHYVD1vXpfRBuY6WmJGvQhquQC-7pxXk56wxe7Qa5xLC6cQycaTtnipeNdlHNXLTRMklxbtNgH4be3TH_rsx8JgwAAAA"
```

### Response

- Content-Type: `text/calendar`
- Body: iCalendar形式のテキスト

## デプロイ先

- Frontend: [Cloudflare Pages](https://ink-alarm.pages.dev/)
  - URL: https://ink-alarm.pages.dev/

- Backend: Google Cloud Run
  - リージョン: asia-northeast1
  - URL: https://hs-api-709184628742.asia-northeast1.run.app/

詳しくは [.github/workflows/deploy-production.yml](https://github.com/nokoika/ink-alarm/blob/main/.github/workflows/deploy-production.yml) に記載

## License

AGPL-3.0-or-later

## Author

* GitHub: [@nokoika](https://github.com/nokoika)
* Twitter: [@ringomotintin](https://twitter.com/ringomotintin)
