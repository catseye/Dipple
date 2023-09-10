#!/bin/sh -e

SHELFA=$HOME/canonical/shelves/catseye
SHELFB=$HOME/catseye

survey_branches() {
    for D in *; do
        if [ -d $D ]; then
            (echo $D && cd $D && git branch --all)
        fi
    done
}

(cd $SHELFA && survey_branches | grep -v HEAD > /tmp/shelf_a.txt)
(cd $SHELFB && survey_branches | grep -v HEAD > /tmp/shelf_b.txt)

(cd $HOME && diff -ru /tmp/shelf_a.txt /tmp/shelf_b.txt)
