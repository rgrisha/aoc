#!/bin/sh
echo "getting file $1"
session=`cat ~/Documents/aoc-session.txt`
curl 'https://adventofcode.com/2021/day/'$1'/input' \
    -H 'authority: adventofcode.com' \
    -H 'cookie: session='$session \
    -o "day-$1.txt"
