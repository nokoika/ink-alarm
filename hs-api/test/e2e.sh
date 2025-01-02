#!/bin/bash

set -euo pipefail

e2e () {
  local -r url="http://localhost:8080/api/ical?query=ewogICIvLyDms6jmhI_kuovpoIUiOiAi44GT44Gu44OV44Kh44Kk44Or44KSYmFzZTY0VVJM44Ko44Oz44Kz44O844OJ44GX44Gf44KC44Gu44GMcXVlcnktYmFzZTY0dXJsLnR4dOOBq-OBquOCiuOBvuOBmeOAgmNhdCBxdWVyeS5qc29uIHwgYmFzZW5jIC0tYmFzZTY0dXJsIC13IDAgPiBxdWVyeS1iYXNlNjR1cmwudHh0IiwKICAiJHNjaGVtYSI6ICIuLi8uLi9xdWVyeS1zY2hlbWEuanNvbiIsCiAgImxhbmd1YWdlIjogImphIiwKICAidXRjT2Zmc2V0IjogIiswOTowMCIsCiAgImZpbHRlcnMiOiBbCiAgICB7CiAgICAgICJtb2RlIjogImJhbmthcmFfb3BlbiIsCiAgICAgICJzdGFnZXMiOiB7CiAgICAgICAgInN0YWdlSWRzIjogWwogICAgICAgICAgMjIKICAgICAgICBdLAogICAgICAgICJtYXRjaEJvdGhTdGFnZXMiOiB0cnVlCiAgICAgIH0sCiAgICAgICJydWxlcyI6IFsKICAgICAgICAibmF3YWJhcmkiCiAgICAgIF0sCiAgICAgICJ0aW1lU2xvdHMiOiBbCiAgICAgICAgewogICAgICAgICAgInN0YXJ0IjogIjAwOjAwIiwKICAgICAgICAgICJlbmQiOiAiMDY6MDAiLAogICAgICAgICAgImRheU9mV2VlayI6ICJzdW4iCiAgICAgICAgfQogICAgICBdLAogICAgICAibm90aWZpY2F0aW9ucyI6IFsKICAgICAgICB7CiAgICAgICAgICAibWludXRlc0JlZm9yZSI6IDkwCiAgICAgICAgfQogICAgICBdCiAgICB9CiAgXQp9Cg=="

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
