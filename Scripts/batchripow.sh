#!/bin/bash

echo "WIP!"

cd ..

mkdir "TMP"
mkdir "TMP/OverworldSprites"
cd Resources

java -jar NARCtowl.jar unpack Narcs/mmodel

mv extracted ../TMP 
rm -f extracted

cd ../TMP/extracted/Narcs/mmodel

dorip () {
    mkdir "out"
    if [ $2 = "true" ]
    then
        echo "$1 Shiny"
        ../../../../Resources/apicula convert "$a.bin" -o out --overwrite --more-textures --shiny
    else
        echo "$1"
        ../../../../Resources/apicula convert "$a.bin" -o out --overwrite --more-textures
    fi
    cd out

    arrSprites=()
    for entry in ./*
    do
        #echo $entry
        arrSprites+=($entry)
    done;

    #echo "${arrSprites[2]}"
    wid=$(identify -format '%w' ${arrSprites[0]})
    hei=$(identify -format '%h' ${arrSprites[0]})
    len=$((${#arrSprites[@]} * $hei))
    convert -size "${wid}x${len}" xc:cyan test.bmp
    for i in  $(seq 0 $((${#arrSprites[@]} - 1))); do
        convert -extract "${wid}x${hei}+0+0" ${arrSprites[$i]} tmp.bmp;
        composite -compose atop -geometry "+0+$(($i * $hei))" tmp.bmp test.bmp test.bmp;
    done;
    convert -transparent cyan test.bmp test.bmp

    if [ $2 = "true" ]
    then
        convert test.bmp "../../../../OverworldSprites/$a-Shiny.png"
    else
        convert test.bmp ../../../../OverworldSprites/$a.png
    fi

    
    rm tmp.bmp
    rm test.bmp

    cd ../
    rm -rf out
}

#dp = 375
#plat = 420
#hgss = 0-265, 297-862
#bw = 6-763
#bw2 = 7-974
for a in {0..974}; do
    [ "$1" = "1" ] && [ "$a" -gt 375 ] &&  continue;
    [ "$1" = "2" ] && [ "$a" -eq 393 ] &&  continue;
    [ "$1" = "2" ] && [ "$a" -gt 420 ] &&  continue;
    [ "$1" = "3" ] && [ "$a" -gt 862 ] &&  continue;
    [ "$1" = "3" ] && [ "$a" -gt 265 ] && [ "$a" -lt 297 ] &&  continue;
    [ "$1" = "4" ] && [ "$a" -lt 6 ] &&  continue;
    [ "$1" = "4" ] && [ "$a" -gt 763 ] &&  continue;
    [ "$1" = "5" ] && [ "$a" -lt 7 ] &&  continue;

    
    if [[ "$1" = "3" && "$a" -gt 297 ]] || [[ "$1" = "3" && "$a" -gt 200 && "$a" -lt 207 ]]
    then
        dorip $a true
    fi
    dorip $a false
done;

cd ../../../../
rm -rf Out/OverworldSprites
mv TMP/OverworldSprites Out
rm -rf TMP