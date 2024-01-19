#!/bin/sh

# Obtaining the name of every slide
HASHES=$(grep -o -E 'href="\#[a-z_]+"' $1 | grep -o -E '"#[a-z_]+"')

# Variable to store the menu
MENU=""

# Variable to create the input array for the trick of avoding jump
# to the top of the screen
ARRAY=""

for slide in $HASHES
do
     # Open li and href
     MENUITEM='<li><a href='${slide}'>'
     # Remove dash, quotes and underline. The last commands capitalize the first letter
     #MENUTITLE=$(echo ${slide} | sed 's/#//;s/\"//g;s/\_/\ /g;s/.*/\u&/')
     MENUTITLE=$(grep -o -E "<h2>([0-9a-zA-Z\ ]+)<a class=\"anchorlink\" href=${slide}>" $1 | sed 's/<h2>//;s/<a.*//')
     # Closing href and li
     MENUITEM=${MENUITEM}${MENUTITLE}'</a></li>'
     MENU=${MENU}'\n'${MENUITEM}

     # Array
     ARRAY=${ARRAY}${slide}','
done

ARRAY=$( echo ${ARRAY} | sed 's/.$//' )
ARRAY='['${ARRAY}']'

sed "s:{{menu}}:${MENU}:" $1 | sed "s:{{hash_items}}:${ARRAY}:" > tmp.html

mv tmp.html $1
