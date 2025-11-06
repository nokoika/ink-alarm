# カレンダー設定復元リンク対応 設計書

## ゴール
- iCal の説明文から設定画面 (`https://ink-alarm.pages.dev`) を開いたときに、カレンダーに登録された予定と同じフィルター設定が初期状態として反映されるようにする。
- バックエンドは受け取ったクエリ文字列（Base64URL + gzip）をそのままフロントエンドへ引き渡し、内容の解析は行わない。
- フロントエンドはクエリを復号して JSON（`query-schema.json` 準拠）を復元し、言語・UTC オフセット・各種フィルター UI の初期状態を組み立てる。

## 現状整理
- ical の説明文は `hs-api/src/Translation.hs::showCalendarDescription` で組み立てており、言語ごとに静的な設定ページ URL を埋め込んでいる。
- API エントリポイント (`hs-api/src/Lib.hs`) は `query` パラメータを Base64URL 文字列として受け取り、V1 はそのまま JSON、V2 は gzip 展開して Query を構築している。
- フロントエンドは `front/src/utils/generateIcalUrl.ts` でクエリ JSON を gzip + Base64URL し、`?query=` として各種リンクに付与している。省略可能な項目（全選択など）は JSON から除外する最適化が入っている。
- フロント側の初期状態は `generateDefaultFilter` / `generateDefaultTimeSlot`（`front/src/utils/generateInitialState.ts`）で生成し、`useFilterCondition` フックで管理している。URL クエリからの復元機構はない。

## 要件・仕様
1. ical の説明文に設定画面へのリンクを埋め込み、クリックすると同一設定を持つ UI が開くこと。
2. リンク形式は `https://ink-alarm.pages.dev/<lang>?query=<base64url>` とし、`<lang>` は Query の言語に合わせる。既存 URL との互換性のためベースドメインは固定で OK。
3. フロント到達時に `query` が存在すれば、Base64URL → gzip 展開 → JSON デシリアライズ → UI 状態反映を行う。失敗時はログを残しつつ従来通りのデフォルト初期化を行う。
4. JSON に項目が存在しない場合は「全選択」「未指定」を意味するため、UI 用の完全な状態へ正規化する。
5. クエリ適用後も通常の操作で状態変更・URL 生成ができること。

## 対応方針

### バックエンド（`hs-api`）
- `Scotty.get "/api/v2/ical"`（必要に応じて `/api/ical` も）で受け取った `query` 文字列を `processQuery` に渡す際に保持するようインターフェースを拡張する。
  - 例: `processQuery :: ... -> T.Text -> Query.QueryRoot -> ActionM ()` のようにパラメータ追加。
- `Filter.createICalInput` / `ICal.ICalInput` に「説明文に差し込む設定 URL」を渡せるフィールドを追加する。
- `Translation.showCalendarDescription` のシグネチャを `showCalendarDescription language mode rule stages timeRange settingsUrl` のように変更し、末尾のリンクを `settingsUrl` へ差し替える。
- `settingsUrl` の生成は Query の `language` と保持している Base64URL を利用して `https://ink-alarm.pages.dev/<lang>?query=<raw>` を組み立てる。Base64URL はそのまま連結する（既に URL セーフ文字のみ）。
- 既存のテスト（`hs-api/test/e2e.sh` 等）が説明文の静的リンクに依存している場合は更新する。
- V1 エンドポイントでの挙動は、クエリが gzip ではない点を除いて同じリンク形式とする（統一性確保）。

### フロントエンド（`front`）
1. **クエリ復号ユーティリティの追加**
   - `utils/decodeCalendarQuery.ts`（仮）を新設。
   - 処理手順: `URLSearchParams` で `query` を取得 → Base64URL を Base64 に変換（`-`/`_` の置換 + padding） → `atob` でバイナリ文字列 → `Uint8Array` → `pako.ungzip`（`{ to: 'string' }`）→ `JSON.parse`。
   - 復元したオブジェクトの型は `Query` (`types/querySchema.ts`)。型ガードを挟み、整合しない場合は `null` を返す。
   - 例外発生時は `console.error` で通知し `null`。

2. **Query → UI 状態への正規化ロジック**
   - `utils/normalizeQueryState.ts`（仮）を追加し、`Query` から `FilterConditionWithKey[]` / UTC オフセット / 言語を生成。
   - モード・ルールが未定義の場合は「全選択」とみなし、`Mode`/`Rule` の全列挙をセット。
   - `stages` が未定義の場合は `generateDefaultFilter().stages`（全ステージ）を利用。
   - `timeSlots` が未定義または `[]` の場合は `generateDefaultTimeSlot()` を 1 件持つ配列を返す。指定がある場合は `uuid` で key を採番しつつコピー。
   - `filters` が空配列の場合は `generateDefaultFilter()` を 1 件返す（UI 要件: 最低 1 行必要）。

3. **各種状態の初期化**
   - `Input.tsx` でフック呼び出し前に `const restoredQuery = useMemo(() => decodeCalendarQuery(location.search), [])` を作成。
   - `useState` 初期化を `useState(restoredQuery?.utcOffset ?? generateInitialUtcOffset())` に変更。
   - `useFilterCondition` を `useFilterCondition(initialFilters?: FilterConditionWithKey[])` に拡張し、`useState` 初期値として利用する。
   - `TranslationLanguageProvider` 由来の `language` がパスと一致しない場合に限り `setLanguage(restoredQuery.language)` を `useEffect` で実行。`setBrowserLanguage` 内の `history.replaceState` は `history.replaceState(null, '', \\`${lang}${location.search}\\`)` のようにクエリ文字列を保持する形へ調整する。

4. **URL 維持と UI 反映**
   - 復元後も `generateIcalUrl` によるリンク生成は従来通り。ユーザーが設定を変更すると新たなクエリが生成される。
   - 初期表示で `useCalendar` に渡す URL も復元状態に合わせて生成されるため、プレビュー内容と実データの一貫性を保証。

5. **例外処理**
   - 復元失敗時はデフォルト状態で初期化し、「設定を復元できませんでした」といったトーストを出すか（任意）。設計上はまず `console.error` ログに留める。

### 共有事項
- Query の gzip 形式は v2 API デフォルト。v1 の Raw JSON でも Base64URL 形式は同じのためフロント復号ロジックで自動判別（gunzip 失敗時に JSON 直読みする二段構え）としても良いが、今回は v2 をリンク源と想定して gzip のみサポートする。必要であれば 2 パス（gunzip → 失敗で JSON 直接 parse）を実装。
- 既存の `generateIcalUrl` の最適化条件をメンテしつつ、逆変換側では「省略 = フル選択」を明示的に実装する。

## テスト観点
- **バックエンド**
  - `/api/v2/ical?query=...` で取得した ical の DESCRIPTION に `?query=` が含まれることを確認（言語別に 2 ケース）。
  - Base64URL をそのまま差し込んでも iCal クライアントでリンクが崩れないことを確認。
- **フロントエンド**
  - `https://ink-alarm.pages.dev/ja?query=...` で各 UI のチェック状態・時間帯・ステージが復元されること。
  - `language=en` のクエリで英語 UI が表示され、`history.replaceState` がクエリを保持すること。
  - 破損クエリ（ランダム文字列）でアクセスした場合にエラーが発生せずデフォルト状態になること。
  - 復元直後にカレンダー URL/Google カレンダーリンクが押せること（リンク先の query が同じであること）。

## リスク・留意事項
- `setBrowserLanguage` の `history.replaceState` を変更する際、既存の言語切り替えボタン操作との挙動差異を確認する。
- 省略されたフィールドを UI に戻す際、`generateDefaultFilter` の初期値（例: モードは X のみ）と「全選択」の意味が異なるため、定数を切り出して共通化する。

## 今後のタスク
- 実装着手時はバックエンド→フロントの順で対応し、最終的に e2e テストスクリプトを更新する。
- 復元失敗時のユーザー通知（トースト等）は別タスクで検討。
