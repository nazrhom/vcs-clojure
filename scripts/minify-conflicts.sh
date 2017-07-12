#!/bin/bash
cd ../test/conflicts/mined
for D in ./*; do
  if [ -d "$D" ]; then
    cd "$D"
    diff3 A.clj O.clj B.clj -m > M.clj
    cd ..
  fi
done
