#!/bin/bash

JPORT=`shuf -i 8400-9400 -n 1` 

source activate jlab

echo "ssh -N -L $JPORT:`hostname`:$JPORT $USER@yeti.cr.usgs.gov"

jupyter lab --ip '*' --no-browser --port $JPORT --notebook-dir=. &

wait
