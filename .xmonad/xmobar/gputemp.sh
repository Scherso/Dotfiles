#!/bin/bash

temp1=50
temp2=60

temp=$(sensors | grep 'edge: ' | awk '{print $2}' | sed 's/+//' | sed 's/.0°C//')
temp=${temp%???}

if [ "$temp" -ge "$temp2" ] ; then
    echo "GPU: <fc=#C1514E>$temp</fc>°C"
elif [ "$temp" -ge "$temp1" ] ; then
    echo "GPU: <fc=#C1A24E>$temp</fc>°C"
else
    echo "GPU: <fc=#AAC0F0>$temp</fc>°C"
fi
