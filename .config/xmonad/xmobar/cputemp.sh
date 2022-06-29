#!/bin/bash

temp1=40
temp2=50

temp=$(sensors | grep 'Tctl: ' | awk '{print $2}' | sed 's/+//' | sed 's/C//')
temp=${temp%???}

if [ "$temp" -ge "$temp2" ] ; then
    echo "﬙ <fc=#C1514E>$temp°C</fc>"
elif [ "$temp" -ge "$temp1" ] ; then
    echo "﬙ <fc=#C1A24E>$temp°C</fc>"
else
    echo "﬙ <fc=#AAC0F0>$temp°C</fc>"
fi
