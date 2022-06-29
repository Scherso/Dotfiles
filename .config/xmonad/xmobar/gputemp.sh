#!/bin/bash

temp1=50
temp2=60

temp=$(sensors | grep 'edge: ' | awk '{print $2}' | sed 's/+//' | sed 's/.0°C//')
temp=${temp%???}

if [ "$temp" -ge "$temp2" ] ; then
    echo "﨎 <fc=#C1514E>$temp°C</fc>"
elif [ "$temp" -ge "$temp1" ] ; then
    echo "﨎 <fc=#C1A24E>$temp°C</fc>"
else
    echo "﨎 <fc=#AAC0F0>$temp°C</fc>"
fi
