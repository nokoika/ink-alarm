#!/bin/bash

set -euo pipefail

e2eV1 () {
  local -r url="http://localhost:8080/api/ical?query=ewogICIvLyDms6jmhI_kuovpoIUiOiAi44GT44Gu44OV44Kh44Kk44Or44KSYmFzZTY0VVJM44Ko44Oz44Kz44O844OJ44GX44Gf44KC44Gu44GMcXVlcnktYmFzZTY0dXJsLnR4dOOBq-OBquOCiuOBvuOBmeOAgmNhdCBxdWVyeS5qc29uIHwgYmFzZW5jIC0tYmFzZTY0dXJsIC13IDAgPiBxdWVyeS1iYXNlNjR1cmwudHh0IiwKICAiJHNjaGVtYSI6ICIuLi8uLi9xdWVyeS1zY2hlbWEuanNvbiIsCiAgImxhbmd1YWdlIjogImphIiwKICAidXRjT2Zmc2V0IjogIiswOTowMCIsCiAgImZpbHRlcnMiOiBbCiAgICB7CiAgICAgICJtb2RlIjogImJhbmthcmFfb3BlbiIsCiAgICAgICJzdGFnZXMiOiB7CiAgICAgICAgInN0YWdlSWRzIjogWwogICAgICAgICAgMjIKICAgICAgICBdLAogICAgICAgICJtYXRjaEJvdGhTdGFnZXMiOiB0cnVlCiAgICAgIH0sCiAgICAgICJydWxlcyI6IFsKICAgICAgICAibmF3YWJhcmkiCiAgICAgIF0sCiAgICAgICJ0aW1lU2xvdHMiOiBbCiAgICAgICAgewogICAgICAgICAgInN0YXJ0IjogIjAwOjAwIiwKICAgICAgICAgICJlbmQiOiAiMDY6MDAiLAogICAgICAgICAgImRheU9mV2VlayI6ICJzdW4iCiAgICAgICAgfQogICAgICBdLAogICAgICAibm90aWZpY2F0aW9ucyI6IFsKICAgICAgICB7CiAgICAgICAgICAibWludXRlc0JlZm9yZSI6IDkwCiAgICAgICAgfQogICAgICBdCiAgICB9CiAgXQp9Cg=="

  response=$(curl -s "$url")

  echo "$response" | grep -q "BEGIN"
}

e2eV2 () {
  local -r url="http://localhost:8080/api/v2/ical?query=H4sIAAAAAAAAA61Py0rDQBTd9yuG4M6miUUKduHCnSAUFHEhItN02qamSZ2ZUF8FZ6ILHyAoIq5UECkKbroT8WeuFPwLM5mmii9QhBCS87jnnM0MQoZloX6v2985en44eLncHTOKyABxAuIeolOQVyCvIboDeVzGjBTG52dnQHYh6oHsQfQI0R6IMxAXIKWyiMPVkNB1k-K2qQ0h9XJ8jYO4A3ELch_EE4hz2JYO5qjOTNxyLU4YtyhhQUgdwqzkRK7BAh9tIXXFd5D5dg6ZbWSjyZ_Mn_ON7Bdj89-OrW24raRyvO3mV8trG_80XFX48_6PLfT8EebUSROr1bmcpR-t10QSraUe9mshrhGlbWCNhdwpVauMcAWO2hNF29ZE1fU4oSyGF-NfhDaTd0w0gwpJYKOM_RVM8XLQIr6xlE0FjMcZSpFaUmy6kvjy-aFWncPcqU8FvD6X2jgNyYDvDI_S0Buk-riNy5i67xK52yRzXsDf2r5vnBagyUbbTicOOeJXEqbwiang9VJ1gZAVHc3CeOeQ7wy-NKL-ljKdzCu5C2mmgQMAAA=="

  response=$(curl -s "$url")

  echo "$response" | grep -q "BEGIN"
}


main () {
  if e2eV1 && e2eV2; then
    echo "E2E test passed"
  else
    echo "E2E test failed"
    exit 1
  fi
}

main "$@"
