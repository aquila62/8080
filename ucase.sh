#!/bin/bash
if [ -z $1 ]
then
echo "Usage: $0 basename"
echo "Example: $0 ascii"
exit 1
fi
cat $1.asm | tr 'a-z' 'A-Z' >$1.80
mv -i $1.80 $1.asm
