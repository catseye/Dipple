#!/bin/sh

# ./test-args.sh one two three
# ./test-args.sh one three
# ./test-args.sh one '' three

echo "ONE=[$1]"
echo "TWO=[$2]"
echo "THREE=[$3]"

if [ "$2" != "" ]; then
   echo "You supplied arg #2"
fi

