#!/bin/bash
if [ -f crctbl.inc ]
then
echo "File crctbl.inc already exists"
exit 1
fi
crctbl >crctbl.inc
