default:
  just --list

check:
  moon check --deny-warn

test:
  moon test --target js

test-update:
  moon test --target js -u

run:
  moon run src/main --target js

fmt:
  moon fmt

web:
  moon build --target js src/web
  cp _build/js/debug/build/web/web.js web.js

serve: web
  python3 -m http.server 8080

js-build: web
  cp _build/js/debug/build/web/web.js js/terrain.js
  cp _build/js/debug/build/web/web.d.ts js/terrain.d.ts
  cp _build/js/debug/build/web/moonbit.d.ts js/moonbit.d.ts
  cd js && npx tsc

js-bench: js-build
  node js/dist/bench.js
