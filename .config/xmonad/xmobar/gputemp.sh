#!/bin/bash

temp1=50
temp2=60

temp=$(sensors | grep 'edge: ' | awk '{print $2}' | sed 's/+//' | sed 's/.0°C//')
temp=${temp%???}

if [ "$temp" -ge "$temp2" ] ; then
    echo "$temp°C"
elif [ "$temp" -ge "$temp1" ] ; then
    echo "$temp°C"
else
    echo "$temp°C"
fi
