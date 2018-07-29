cd /d %~dp0
set SLACK_API_TOKEN=<ENTER_YOUR_LEGACY_API_TOKEN>
stack build --exec slack-log
