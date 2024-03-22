#!/bin/bash
path="./" # Replace this with the path to your git repository
while true
do
	cd $path                              &> /dev/null
	git pull                              &> /dev/null
	git add --all                         &> /dev/null
	now=$(date)                           &> /dev/null
	git commit -m "Auto-Commit at : $now" &> /dev/null	
	git push -u origin main               &> /dev/null
	sleep 1 && exit	
done