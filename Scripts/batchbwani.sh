#!/bin/bash
#todo: tables that we know do have female forms so we can avoid error spam in the console..
echo "ripping!"
#bw has 711, bw2 has 750

NUM=711

if [ "$1" = "2" ]; then
    NUM=750;
fi

cd ..
echo "0 front"
for i in $(seq 0 $NUM); do echo -e "\e[1A\e[K$i/$NUM front"; ./ripscript ./Scripts/rip-bw-animated-batch.scm $i 18 2 0 ""; done
echo -e "\e[1A\e[Kfront done"
echo "0 front female"
for i in $(seq 0 $NUM); do echo -e "\e[1A\e[K$i/$NUM front female"; ./ripscript ./Scripts/rip-bw-animated-batch.scm $i 18 3 0 "/female"; done
echo -e "\e[1A\e[Kfront female done"
echo "0 front shiny"
for i in $(seq 0 $NUM); do echo -e "\e[1A\e[K$i/$NUM front shiny"; ./ripscript ./Scripts/rip-bw-animated-batch.scm $i 19 2 0 "/shiny"; done
echo -e "\e[1A\e[Kfront shiny done"
echo "0 front shiny female"
for i in $(seq 0 $NUM); do echo -e "\e[1A\e[K$i/$NUM front shiny female"; ./ripscript ./Scripts/rip-bw-animated-batch.scm $i 19 3 0 "/shiny/female"; done
echo -e "\e[1A\e[Kfront shiny done"
#back
echo "0 back"
for i in $(seq 0 $NUM); do echo -e "\e[1A\e[K$i/$NUM back"; ./ripscript ./Scripts/rip-bw-animated-batch.scm $i 18 2 9 "/back"; done
echo -e "\e[1A\e[Kback done"
echo "0 back female"
for i in $(seq 0 $NUM); do echo -e "\e[1A\e[K$i/$NUM back female"; ./ripscript ./Scripts/rip-bw-animated-batch.scm $i 18 3 9 "/back/female"; done
echo -e "\e[1A\e[Kback female done"
echo "0 back shiny"
for i in $(seq 0 $NUM); do echo -e "\e[1A\e[K$i/$NUM back shiny"; ./ripscript ./Scripts/rip-bw-animated-batch.scm $i 19 2 9 "/back/shiny"; done
echo -e "\e[1A\e[Kback shiny done"
echo "0 back shiny female"
for i in $(seq 0 $NUM); do echo -e "\e[1A\e[K$i/$NUM back shiny female"; ./ripscript ./Scripts/rip-bw-animated-batch.scm $i 19 3 9 "/back/shiny/female"; done
echo -e "\e[1A\e[Kback shiny female done"