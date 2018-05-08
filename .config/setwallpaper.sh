#!/bin/bash

### GET RANDOM WALLPAPER FROM $HOME/Pictures/wallpaper/*, CHANGES RES AND COMBINES THEM ###

export XAUTHORITY=/home/genzix/.Xauthority
HOME=/home/genzix

### WORKING DIR ###
WDIR="/tmp/renpaper"
Wallpaper=${WDIR}/wallpaper.png
if [[ -d ${WDIR} ]]; then
	echo "Already Running. If it isnt running delete ${WDIR}"
	exit 1
fi
mkdir -p ${WDIR}

### Xrandr Stuff ###
#Sum monitor lengths
WallpaperL=$(xrandr | grep \* | awk 'BEGIN {FPAT="[0-9]+"}{s+=$1} END {print s}')
#Max monitor height
WallpaperH=$(xrandr | grep \* | awk -v max=0 'BEGIN {FPAT="[0-9]+"}{if($2>max){max=$2}}END{print max}')
#Array monitor lengths/heights
WallpaperLArr=($(xrandr | grep \* | awk 'BEGIN{FPAT="[0-9]+"} {print $1}' | tr ' ' '\n' | paste -sd' ' - ))
WallpaperHArr=($(xrandr | grep \* | awk 'BEGIN{FPAT="[0-9]+"} {print $2}' | tr ' ' '\n' | paste -sd' ' - ))
numMon=$(xrandr | grep -w connected | wc -l)

### RANDOMLY SET WALLPAPER FROM PICTURES ###
monWall=()
for ((m=0;numMon>m;m++)); do 
	monWall[m]=$(find $HOME/Pictures/wallpaper -type f | shuf -n 1); 
	if [[ m==0 ]]; then
		continue
	fi
	if [[ monWall[m]!=monWall[m-1] ]]; then
		((m-=1));
	fi
done

### Scaling wallpaper ###
for ((i=0;${numMon}>i;i++)); do
	convert ${monWall[$i]} -resize ${WallpaperLArr[${i}]}x${WallpaperHArr[${i}]}! ${WDIR}/D${i}.png
	sleep 1
done

### Creating Background ###
convert -size ${WallpaperL}x${WallpaperH} xc:black $Wallpaper
ofset=0
i=0
for mon in ${WallpaperLArr[@]}; do	
	composite -geometry +${ofset}+0 ${WDIR}/D${i}.png $Wallpaper $Wallpaper 
	((ofset+=${mon}))
	((i+=1))
	sleep 1
done 
feh --bg-max $Wallpaper
rm -r ${WDIR} &
