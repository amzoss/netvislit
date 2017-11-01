#!/bin/bash

FILE1="$1.csv"

FILENEW2="$1-rawdata.csv"

sed 's/,$//' $FILE1 > $FILENEW2


