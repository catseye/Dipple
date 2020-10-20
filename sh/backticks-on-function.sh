#!/bin/sh

translate() {
    if [ "X$1" = "Xfoo" ]; then
        echo "bar"
    elif [ "X$1" = "Xwhee" ]; then
        echo "WHEE"
    else
        echo $1
    fi
}

FILE=`translate $1`
echo $FILE
