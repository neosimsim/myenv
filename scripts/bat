#!/bin/sh

bat=$(acpi)

if $(echo $bat | grep Charging > /dev/null); then
	echo $bat | awk -F"," '{print "Charging " $2}'
else
	if $(echo $bat | grep Full > /dev/null); then
		echo Full
	else
		echo $bat| awk -F":| |," '{print $6 " " $8 ":" $9}'
	fi
fi
