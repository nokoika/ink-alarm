// @ts-check

import { fixupConfigRules } from '@eslint/compat';
import reactRefresh from 'eslint-plugin-react-refresh';
import pluginJs from '@eslint/js';
import pluginTseslint from 'typescript-eslint';
import pluginReact from 'eslint-plugin-react';
import { FlatCompat } from '@eslint/eslintrc';
import pluginPrettier from 'eslint-plugin-prettier/recommended';

const compat = new FlatCompat({
  baseDirectory: import.meta.dirname,
});

/**
 * 型があわないけどドキュメントを見る限りこれでいいらしい
 * @type {any}
 */
const pluginReactConfig = pluginReact.configs.flat?.recommended;

export default pluginTseslint.config(
  {
    ignores: ['**/dist'],
  },
  pluginJs.configs.recommended,
  ...pluginTseslint.configs.recommendedTypeChecked,
  {
    languageOptions: {
      parserOptions: {
        projectService: {
          allowDefaultProject: ['*.js', '*.mjs'],
        },
      },
    },
  },
  // eslint-disable-next-line @typescript-eslint/no-unsafe-argument
  pluginReactConfig,
  ...fixupConfigRules(compat.extends('plugin:react-hooks/recommended')),
  reactRefresh.configs.recommended,
  pluginPrettier,
  {
    rules: {
      // 昔のReactのバージョンで必要だったものなので、最新のReactでは不要
      'react/react-in-jsx-scope': 'off',

      // await 漏れをエラーにする
      '@typescript-eslint/no-floating-promises': 'error',

      // export する関数は型を明示する
      '@typescript-eslint/explicit-module-boundary-types': 'error',
    },
    settings: {
      react: {
        version: 'detect',
      },
    },
  },
);
