#!/bin/bash

SESSION=$(<.token)

YEAR="$1"
DAY="$2"

PADDED_DAY=$(printf '%2s' "$DAY" | tr ' ' '0')
FILENAME="$YEAR.$PADDED_DAY.in"

https -vvv "adventofcode.com/$YEAR/day/$DAY/input" \
	User-Agent:https://github.com/rnallandigal/advent-of-code/blob/master/get_input.sh\ by\ rnallandigal@gmail.com \
	Cookie:session="$SESSION" \
	--download --output "c++/in/$FILENAME"

cp "c++/in/$FILENAME" "haskell/in/"
