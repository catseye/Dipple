#!/bin/sh

funky()
{
	for J in 1 2 3 4 5; do
		echo "--> $J"
		sleep 1
	done
}

funky &

for K in 1 2 3 4 5; do
	echo "$K <<<"
	sleep 1
done
