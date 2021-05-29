#!/bin/bash

echo "ripping!"
#bw has 711, bw2 has 187
cd ..
for i in {0..187}; do echo "$i front"; ./ripscript ./Scripts/rip-bw-trainer-animated-batch.scm $i ""; done
for i in {0..16}; do echo "$i back"; ./ripscript ./Scripts/rip-bw-trainerB-animated-batch.scm $i ""; done