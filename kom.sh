#!/bin/bash
if [ -z $1 ]
then
echo "Usage: $0 basename"
echo "Example: $0 ascii"
exit 1
fi
cat $1.hex | hx2c >$1.com
