#!/bin/sh

set -eu

cd "$(dirname "$0")"

export SLACK_API_TOKEN='<ENTER_YOUR_LEGACY_API_TOKEN>'
stack build --exec slack-log

git add docs
git commit -m"Slack log update at $(date)"
git push
