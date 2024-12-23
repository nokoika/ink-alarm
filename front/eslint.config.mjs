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

// 型があってないけどドキュメントを見る限りこれであってるらしい
/** @type {any} */
const pluginReactConfig = pluginReact.configs.flat?.recommended;

export default pluginTseslint.config(
  {
    ignores: ['**/dist'],
  },
  pluginJs.configs.recommended,
  ...pluginTseslint.configs.recommended,
  pluginReactConfig,
  ...fixupConfigRules(compat.extends('plugin:react-hooks/recommended')),
  reactRefresh.configs.recommended,
  pluginPrettier,
  {
    rules: {
      // 昔のReactのバージョンで必要だったものなので、最新のReactでは不要
      'react/react-in-jsx-scope': 'off',
    },
    settings: {
      react: {
        version: 'detect',
      },
    },
  },
);
