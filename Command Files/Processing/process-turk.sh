#!/bin/bash

FILE1="$1.csv"

FILENEW="$1-new.csv"

FILENEW2="$1-rawdata.csv"

cut -d, -f-40,42- $FILE1 > $FILENEW

sed 's/,$//' $FILENEW > $FILENEW2


