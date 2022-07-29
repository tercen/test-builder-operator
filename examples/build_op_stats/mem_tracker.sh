#!/bin/bash

COUNT=0
> /home/thiago/Tercen/mem_track/mem_usage.txt
while true
do
	docker stats --format "{{.Name}},{{.MemUsage}}" --no-stream >> /home/thiago/Tercen/mem_track/mem_usage.txt
	
	COUNT=$((COUNT+1))
	
	
	if [[ "$COUNT" -eq "5" ]]; then
		COUNT=0
		> /home/thiago/Tercen/mem_track/mem_usage.txt
	fi
#	> filename
	sleep 0.5
done
