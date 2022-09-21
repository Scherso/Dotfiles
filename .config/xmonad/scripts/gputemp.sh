#!/bin/bash

temp=$(sensors | grep 'edge: ' | awk '{print $2}' | sed 's/+//' | sed 's/.0°C//')
temp=${temp%???}

echo "$temp°C"
