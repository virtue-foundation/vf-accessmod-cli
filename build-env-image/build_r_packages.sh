#!/bin/bash

export RPACKAGEINSTALLLIST=$1

# test if there is at least one argument: if not, return an error
if [-z "$RPACKAGEINSTALLLIST" ]; then
  printf '%s\n' "Must provide a package list file path as argument" >&2
  exit 1
fi

while IFS=" " read -r package; 
  do 
    Rscript -e 'install("'"$package"'")';
done < $RPACKAGEINSTALLLIST
