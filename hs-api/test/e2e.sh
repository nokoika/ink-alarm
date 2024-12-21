#!/bin/bash

set -euo pipefail

e2e () {
  local -r url="http://localhost:8080/api?query=ewogICIvLyDoqqzmmI4iOiAi44GT44GuanNvbuOCkmJhc2U2NFVSTOOCqOODs-OCs-ODvOODieOBl-OBn-OCguOBruOCknF1ZXJ544Gr44K744OD44OI44GX44G-44GZ44CCcGJwYXN0ZSB8IGJhc2VuYyAtLWJhc2U2NHVybCAtdyAwIHwgcGJjb3B5IiwKICAibGFuZ3VhZ2UiOiAiamEiLAogICJ1dGNPZmZzZXQiOiAiKzA5OjAwIiwKICAiZmlsdGVycyI6IFsKICAgIHsKICAgICAgIm1hdGNoVHlwZSI6ICJiYW5rYXJhX29wZW4iLAogICAgICAicnVsZXMiOiBbCiAgICAgICAgImFzYXJpIgogICAgICBdLAogICAgICAidGltZVNsb3RzIjogWwogICAgICAgIHsKICAgICAgICAgICJzdGFydCI6ICIwMDowMCIsCiAgICAgICAgICAiZW5kIjogIjA4OjAwIgogICAgICAgIH0KICAgICAgXSwKICAgICAgIm5vdGlmaWNhdGlvbnMiOiBbCiAgICAgICAgewogICAgICAgICAgIm1pbnV0ZXNCZWZvcmUiOiAyMAogICAgICAgIH0KICAgICAgXQogICAgfQogIF0KfQo="

  response=$(curl -s "$url")

  echo "$response" | grep -q "BEGIN"
}

main () {
  if e2e; then
    echo "E2E test passed"
  else
    echo "E2E test failed"
    exit 1
  fi
}

main "$@"
