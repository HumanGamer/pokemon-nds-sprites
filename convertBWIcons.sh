#!/bin/bash
./rip 9;
python3 BWIconPalette.py;
cd pokeIcons
for i in {0..711}; do
echo "converting $i to the right format!"
convert -size 32x64 xc:black $i.bmp
convert -extract 32x8+0+0 $i.png tmp.bmp
composite -compose atop -geometry +0+0 tmp.bmp $i.bmp $i.bmp
convert -extract 32x8+32+0 $i.png tmp.bmp
composite -compose atop -geometry +0+8 tmp.bmp $i.bmp $i.bmp
convert -extract 32x8+0+8 $i.png tmp.bmp
composite -compose atop -geometry +0+16 tmp.bmp $i.bmp $i.bmp
convert -extract 32x8+32+8 $i.png tmp.bmp
composite -compose atop -geometry +0+24 tmp.bmp $i.bmp $i.bmp
convert -extract 32x8+0+16 $i.png tmp.bmp
composite -compose atop -geometry +0+32 tmp.bmp $i.bmp $i.bmp
convert -extract 32x8+32+16 $i.png tmp.bmp
composite -compose atop -geometry +0+40 tmp.bmp $i.bmp $i.bmp
convert -extract 32x8+0+24 $i.png tmp.bmp
composite -compose atop -geometry +0+48 tmp.bmp $i.bmp $i.bmp
convert -extract 32x8+32+24 $i.png tmp.bmp
composite -compose atop -geometry +0+56 tmp.bmp $i.bmp $i.bmp

convert -transparent black $i.bmp $i.bmp

convert $i.bmp $i.png

rm -f tmp.bmp
rm -f $i.bmp
done;