# Fire-brigade-alert-times
Analysis of the fire brigade reaction times in several big Swiss cities.

The goal was to find out, how many buildings can be reached within 10 minutes from the closest fire brigade base. (Which is the legal requirement).

This was done by calculating travel times from fire brigade bases to randomly chosen addresses within each city. Travel times were calculated using the Google maps API. The analysis was conducted for six of the biggest swiss cities, as well as several smaller ones. The analysis can be repeated for any swiss city, by importing the corresponding address file and inserting the coordinates for the fire brigade bases.

Address-files (.csv) can be downloaded here: https://map.geo.admin.ch/?lang=de&topic=ech&bgLayer=ch.swisstopo.swissimage&layers=ch.swisstopo.amtliches-strassenverzeichnis,ch.bfs.gebaeude_wohnungs_register&E=2561877.00&N=1205249.25&zoom=9&layers_opacity=0.85,1

Shape-file for boundaries of municipalities (for plotting the map only): https://shop.swisstopo.admin.ch/de/products/landscape/boundaries3D

Google maps API: https://cloud.google.com/maps-platform/routes/?hl=de
