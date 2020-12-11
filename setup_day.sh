#!/usr/bin/env sh

SESSION="$(cat .session_key)"
DAY="day$1"

mkdir $DAY
curl "https://adventofcode.com/2020/day/$1/input" --cookie "session=$SESSION" -o "$DAY/input.txt"
