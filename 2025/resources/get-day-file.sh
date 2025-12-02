#!/bin/sh
echo "getting file $1"
session=`cat ~/.aoc-session`
curl 'https://adventofcode.com/2025/day/'$1'/input' \
    -H 'authority: adventofcode.com' \
    -H 'cookie: session='$session \
    -o "day-$1.txt"
