#!/bin/bash
echo "ripping!"
#bw has 711, bw2 has 750

NUM=711

if [ "$1" = "2" ]; then
    NUM=750;
fi

female=(3  12  19  20  25  26  41  42  44  45  64  65  84  85  97  111  112  118  119  123  129  130  154  165  166  178  185  186  190  194  195  198  202  203  207  208  212  214  215  217  221  224  229  232  255  256  257  267  269  272  274  275  307  308  315  316  317  322  323  332  350  369  396  397  398  399  400  401  402  403  404  405  407  415  417  418  419  424  443  444  445  449  450  453  454  456  457  459  460  461  464  465  473  521  592  593)

cd ..
echo "0 front"
for i in $(seq 0 $NUM); do echo -e "\e[1A\e[K$i/$NUM front"; ./ripscript ./Scripts/rip-bw-animated-batch.scm $i 18 2 0 ""; done
echo -e "\e[1A\e[Kfront done"
echo "0 front female"
for i in ${female[@]}; do echo -e "\e[1A\e[K$i/$NUM front female"; ./ripscript ./Scripts/rip-bw-animated-batch.scm $i 18 3 0 "/female"; done
echo -e "\e[1A\e[Kfront female done"
echo "0 front shiny"
for i in $(seq 0 $NUM); do echo -e "\e[1A\e[K$i/$NUM front shiny"; ./ripscript ./Scripts/rip-bw-animated-batch.scm $i 19 2 0 "/shiny"; done
echo -e "\e[1A\e[Kfront shiny done"
echo "0 front shiny female"
for i in ${female[@]}; do echo -e "\e[1A\e[K$i/$NUM front shiny female"; ./ripscript ./Scripts/rip-bw-animated-batch.scm $i 19 3 0 "/shiny/female"; done
echo -e "\e[1A\e[Kfront shiny done"
#back
echo "0 back"
for i in $(seq 0 $NUM); do echo -e "\e[1A\e[K$i/$NUM back"; ./ripscript ./Scripts/rip-bw-animated-batch.scm $i 18 2 9 "/back"; done
echo -e "\e[1A\e[Kback done"
echo "0 back female"
for i in ${female[@]}; do echo -e "\e[1A\e[K$i/$NUM back female"; ./ripscript ./Scripts/rip-bw-animated-batch.scm $i 18 3 9 "/back/female"; done
echo -e "\e[1A\e[Kback female done"
echo "0 back shiny"
for i in $(seq 0 $NUM); do echo -e "\e[1A\e[K$i/$NUM back shiny"; ./ripscript ./Scripts/rip-bw-animated-batch.scm $i 19 2 9 "/back/shiny"; done
echo -e "\e[1A\e[Kback shiny done"
echo "0 back shiny female"
for i in ${female[@]}; do echo -e "\e[1A\e[K$i/$NUM back shiny female"; ./ripscript ./Scripts/rip-bw-animated-batch.scm $i 19 3 9 "/back/shiny/female"; done
echo -e "\e[1A\e[Kback shiny female done"