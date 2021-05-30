# pokemon-nds-sprites
Sprite rip tool collection for the fourth- and fifth-generation Pokémon games (D/P/Pt/HG/SS/B/W/B2/W2)
  
This is a collection of scripts originally created by magical, and edited by Ayukito. New scripts have also been added by Ayukito   
  
These are tested on Ubuntu 20.0.4 and Macos 11.4  
  
Uses:  
[Apicula](https://github.com/scurest/apicula) for .BTX0->.Png conversion  
[Narctowl](https://github.com/turtleisaac/Narctowl) for .narc unpacking, first step before .BTX0->.Png conversion  
*Regarding the above, I could totally try to figure out how the included narc library written in C works, but I hate C and prefer Python & bash*  
*Also, the NarcTools reimps in Python don't run on Linux, and the original(?) is written for Windows*


Requirements for all total:  

Bash?  
Java 12+  
Python3  
gcc  
Guile2.2 (or 3.0 for mac, change the makefile)  
Libpng  
Libgif  
More?  
  
See [wiki](https://github.com/Ayukito/pokemon-nds-sprites/wiki) page for more information, more documentation on usage later
