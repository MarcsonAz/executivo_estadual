#!/bin/bash
path="\\srjn4\atlas\executivo_estadual" # Replace this with the path to your git repository
while true
do
	inotifywait --recursive -qq -e attrib,create,delete,modify,delete_self,move,move_self,close_write $path
	cd $path                              &> /dev/null
	git pull                              &> /dev/null
	git add --all                         &> /dev/null
	now=$(date)                           &> /dev/null
	git commit -m "Auto-Commit at : $now" &> /dev/null	
	git push -u origin main               &> /dev/null
done