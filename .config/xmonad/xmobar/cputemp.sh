#!/bin/bash

temp1=40
temp2=50

temp=$(sensors | grep 'Tctl: ' | awk '{print $2}' | sed 's/+//' | sed 's/C//')
temp=${temp%???}

if [ "$temp" -ge "$temp2" ] ; then
    echo "CPU: <fc=#C1514E>$temp</fc>°C"
elif [ "$temp" -ge "$temp1" ] ; then
    echo "CPU: <fc=#C1A24E>$temp</fc>°C"
else
    echo "CPU: <fc=#AAC0F0>$temp</fc>°C"
fi
