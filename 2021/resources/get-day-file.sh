#!/bin/sh
echo "getting file $1"
curl 'https://adventofcode.com/2021/day/'$1'/input' \
    -H 'authority: adventofcode.com' \
    -H 'sec-ch-ua: "Chromium";v="94", "Google Chrome";v="94", ";Not A Brand";v="99"' \
    -H 'user-agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/94.0.4606.61 Safari/537.36' \
    -H 'accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9' \
    -H 'cookie: _ga=GA1.2.519253074.1636918247; _gid=GA1.2.2110957216.1636918247; session=53616c7465645f5f5492edf15bd649a20ab575226479e7f7276fa948082c6b6f074d4b201400bac9194dd11eea4bf38f' \
    -o "day-$1.txt"
