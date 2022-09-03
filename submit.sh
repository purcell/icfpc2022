#!/bin/sh

set -e

KEY=$(cat team.key)

authcurl() {
    curl --header "Authorization: Bearer $KEY" "$@"
}

for f in solutions/*.isl; do
    n=$(basename "$f" | sed 's/\.isl//')
    authcurl -F file=@$f https://robovinci.xyz/api/submissions/$n/create
    echo
done
