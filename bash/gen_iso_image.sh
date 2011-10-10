#!/bin/sh

# command-line options for genisoimage that let you create a decent ISO
genisoimage -R -J -D -joliet-long -o $1.iso $1