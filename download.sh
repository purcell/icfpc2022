#!/bin/sh

set -e

[ -n "$1" ] || { echo "usage: $0 N ..." >&2; exit 1; } 

for f in $*; do
#   curl https://cdn.robovinci.xyz/imageframes/$f.png > problems/$f.png
   curl https://cdn.robovinci.xyz/imageframes/$f.initial.png > problems/$f.initial.png
#   curl https://cdn.robovinci.xyz/imageframes/$f.initial.json | jq . > problems/$f.json
done
