#!/bin/bash

echo "ripping!"
#bw has 711, bw2 has 750
cd ..
for i in {0..750}; do echo "$i front"; ./ripscript ./Scripts/rip-bw-animated-batch.scm $i 18 2 0 ""; done
for i in {0..750}; do echo "$i front female"; ./ripscript ./Scripts/rip-bw-animated-batch.scm $i 18 3 0 "/female"; done
for i in {0..750}; do echo "$i front shiny"; ./ripscript ./Scripts/rip-bw-animated-batch.scm $i 19 2 0 "/shiny"; done
for i in {0..750}; do echo "$i front shiny female"; ./ripscript ./Scripts/rip-bw-animated-batch.scm $i 19 3 0 "/shiny/female"; done
#back
for i in {0..750}; do echo "$i back"; ./ripscript ./Scripts/rip-bw-animated-batch.scm $i 18 2 9 "/back"; done
for i in {0..750}; do echo "$i back female"; ./ripscript ./Scripts/rip-bw-animated-batch.scm $i 18 3 9 "/back/female"; done
for i in {0..750}; do echo "$i back shiny"; ./ripscript ./Scripts/rip-bw-animated-batch.scm $i 19 2 9 "/back/shiny"; done
for i in {0..750}; do echo "$i back shiny female"; ./ripscript ./Scripts/rip-bw-animated-batch.scm $i 19 3 9 "/back/shiny/female"; done