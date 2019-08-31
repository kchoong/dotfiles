#!bin/bash

CURRENT="$(light -G)"
if [ "$CURRENT" != "0.00" ]; then
  light -S 0
else
  light -S 50
fi
