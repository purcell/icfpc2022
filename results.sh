#!/bin/sh

set -e

KEY=$(cat team.key)

authcurl() {
    curl --header "Authorization: Bearer $KEY" "$@"
}

authcurl https://robovinci.xyz/api/results/user | jq .

