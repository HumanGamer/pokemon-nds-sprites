#!/bin/bash

echo "ripping!"
#bw has ..?, bw2 has 187

NUM=94
NUM2=5

if [ "$1" = "2" ]; then
    NUM=187;
    NUM2=16;
else
    echo "BW 1 has no animated front trainer sprite. This will look extremely messed up. Use ./rip 3 in the root folder instead."
fi

cd ..
echo "0 front"
for i in $(seq 0 $NUM); do echo -e "\e[1A\e[K$i/$NUM front"; ./ripscript ./Scripts/rip-bw-trainer-animated-batch.scm $i ""; done
echo -e "\e[1A\e[Kfront done"
echo "0 back"
for i in $(seq 0 $NUM2); do echo -e "\e[1A\e[K$i/$NUM back"; ./ripscript ./Scripts/rip-bw-trainerB-animated-batch.scm $i ""; done
echo -e "\e[1A\e[Kback done"