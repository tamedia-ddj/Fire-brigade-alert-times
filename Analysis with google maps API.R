library(mapsapi) ## google maps api package
library(leaflet) ## Display map
library(ggplot2) ## fortify() --> df erstellen
library(rgdal) ## readOGR() --> read shapefiles
library(maptools)
library(plyr)
library(rgeos)
library(sp)
library(ggmap)
library(stringr) # --> für str_c

## Transform coordinates CH -> interantional (the ones google uses) ####
CHtoWGS <- function(y, x){
  ## Converts militar to civil and  to unit = 1000km
  ## Auxiliary values (% Bern)
  y_aux = (y - 2600000)/1000000
  x_aux = (x - 1200000)/1000000
  lng = 2.6779094 + 4.728982 * y_aux + 0.791484 * y_aux * x_aux + 0.1306 * y_aux * x_aux**2 - 0.0436   * y_aux**3
  lat = 16.9023892 + 3.238272 * x_aux -0.270978 * y_aux**2 -  0.002528 * x_aux**2 - 0.0447 * y_aux**2 * x_aux - 0.0140 * x_aux**3
  return(c(lng * 100/36, lat * 100/36))
}

api_key <- "" # Insert google maps API key here. It is needed if traffic should be included. Otherwise it might work without (though not always). 
city <- "Lausanne" # choose city (.csv file with addresses needs to be provided)
set.seed(1)

## Read / enter coordinates of origins/destinations and create reduced sample ####
# Koordinaten der Feuerwehrstationen
if (city == "Lausanne") {
  Origins <- rbind(c(8.524455, 47.369159), c(8.549590, 47.444050), c(8.536943, 47.417641))
  #Origins <- rbind(c(8.524455, 47.369159))
  Destinations <- read.csv("D:/Tamedia/Projekte/Feuerwehr/Daten/Geb_Stadt_Zurich.csv", header = TRUE, sep=';')
  gemeinde <- "ZÃ¼rich"
  }
if (city == "Bern") {
  Origins <- rbind(c(7.416707, 46.952940))
  Destinations <- read.csv("D:/Tamedia/Projekte/Feuerwehr/Daten/Geb_Stadt_Bern.csv", header = TRUE, sep=';')
  gemeinde <- city
}
if (city == "Basel") {
  Origins <- rbind(c(7.582329, 47.556960))
  Destinations <- read.csv("D:/Tamedia/Projekte/Feuerwehr/Daten/Geb_Stadt_Basel.csv", header = TRUE, sep=';')
  gemeinde <- city
}
if (city == "Luzern") {
  Origins <- rbind(c(8.306345, 47.041278), c(8.259128, 47.052346))
  Destinations <- read.csv("D:/Tamedia/Projekte/Feuerwehr/Daten/Geb_Stadt_Luzern.csv", header = TRUE, sep=';')
  gemeinde <- city
}
if (city == "Genf") {
  Origins <- rbind(c(6.137367, 46.200293), c(6.130794, 46.214144))
  Destinations <- read.csv("D:/Tamedia/Projekte/Feuerwehr/Daten/Geb_Stadt_Genf.csv", header = TRUE, sep=';')
  gemeinde <- "GenÃ¨ve"
}
if (city == "Meilen") {
  Origins <- rbind(c(8.642575, 47.271726))
  Destinations <- read.csv("D:/Tamedia/Projekte/Feuerwehr/Daten/Geb_Stadt_Meilen.csv", header = TRUE, sep=';')
  gemeinde <- city
}
if (city == "Winterthur") {
  Origins <- rbind(c(8.733131, 47.495629))
  Destinations <- read.csv("D:/Tamedia/Projekte/Feuerwehr/Daten/Geb_Stadt_Winterthur.csv", header = TRUE, sep=';')
  gemeinde <- city
}
if (city == "Burgdorf") {
  Origins <- rbind(c(7.629129, 47.053603))
  Destinations <- read.csv("D:/Tamedia/Projekte/Feuerwehr/Daten/Geb_Stadt_Burgdorf.csv", header = TRUE, sep=';')
  gemeinde <- city
}
if (city == "Chur") {
  Origins <- rbind(c(9.510138, 46.847895))
  Destinations <- read.csv("D:/Tamedia/Projekte/Feuerwehr/Daten/Geb_Stadt_Chur.csv", header = TRUE, sep=';')
  gemeinde <- city
}
if (city == "Gossau (SG)") {
  Origins <- rbind(c(9.241895, 47.422299))
  Destinations <- read.csv("D:/Tamedia/Projekte/Feuerwehr/Daten/Geb_Stadt_Gossau SG.csv", header = TRUE, sep=';')
  gemeinde <- city
}
if (city == "Lausanne") {
  Origins <- rbind(c(6.624504, 46.522221))
  Destinations <- read.csv("D:/Tamedia/Projekte/Feuerwehr/Daten/Geb_Stadt_Lausanne.csv", header = TRUE, sep=';')
  gemeinde <- city
}

n_buildings <- length(Destinations[, 1])
sample_size <- 150
## Transform Coordinates ####

Destinations["coordLong"] <- 0; Destinations["coordLat"] <- 0
i=1
for (i in 1:n_buildings){
  Destinations$coordLong[i] <- CHtoWGS(Destinations$GKODE[i], Destinations$GKODN[i])[1]
  Destinations$coordLat[i] <- CHtoWGS(Destinations$GKODE[i], Destinations$GKODN[i])[2]
}

## Take sample of N from Buildings ####
Destinations_sample_index <- box_index_final # Use for box sample
Destinations_sample_index <- sample(length(Destinations[,1]), sample_size)
Destinations_sample <- Destinations[Destinations_sample_index, ]
sample_size <- length(Destinations_sample[, 1])

## Get travel times from Google API: ACHTUNG: 1$ per 100 runs! (1 run: #Origins * #Destinations) ####

# time for traffic delay
time <- as.POSIXct("180920 17:01", format = "%y%m%d %H:%M") ## Donnerstag im September, 18:01

# split sample in smaller packages to comply with google API
max_size <- 40 ## Maximale Anzahl Abfragen für die google API pro Durchgang (<100)
o <-  length(Origins[ ,1 ])
n <- sample_size*o
iterations <- n %/% max_size+1 # Wie viele Durchgänge sind nötig?
size <- floor(sample_size/iterations) # Wie viele Abfragen pro Durchgang?
index_split <- split(1:sample_size, ceiling(seq_along(1:sample_size)/size)) ## Listen mit den Indices pro Durchgang

rm(index_temp)
m_dist <- array(0, dim=c(sample_size, o))
m_time_traffic <- array(0, dim=c(sample_size, o))
m_time_notraffic <- array(0, dim=c(sample_size, o))

i <- 1
for (i in 1:length(index_split)){
  index_temp <- as.numeric(unlist(index_split[i]))
  doc = mp_matrix(origins = Origins, destinations = cbind(Destinations_sample[index_temp, ]$coordLong, Destinations_sample[index_temp, ]$coordLat), mode = "driving", key = api_key) # , 
  # doc = mp_matrix(origins = Origins, destinations = Destinations_newcoord[index_temp, ], mode = "driving", key = api_key)
  m_dist[index_temp, ] = t(mp_get_matrix(doc, value = "distance_m"))
  #m_time_traffic[index_temp, ]  = t(mp_get_matrix(doc, value = "duration_in_traffic_s"))
  m_time_notraffic[index_temp, ]  = t(mp_get_matrix(doc, value = "duration_s"))
  Sys.sleep(0.1) # Google API nicht überlasten (vielleicht nicht notwendig)
}

# Reiesezeiten in ursprünglichen DF abfüllen
Destinations_sample$dist <- apply(m_dist, 1, min)
Destinations_sample$time_notraffic <- apply(m_time_notraffic, 1, min)
#Destinations_sample$time_traffic <- apply(m_time_traffic, 1, min)

## Analyze travel times ####
# draw markers with green/orange/red to indicate problem
Destinations_sample$marker_color <- "red" # red as a default value
Destinations_sample[Destinations_sample$time_notraffic < 900, ]$marker_color <- "orange" # orange if reached <15min
Destinations_sample[Destinations_sample$time_notraffic < 600, ]$marker_color <- "green" # green if reached <10 min

## Store Data ####

## Store as .csv (Destination sample with received time and distance)
# write.table(Destinations_sample, "D:/Tamedia/Projekte/Feuerwehr/Daten/Destinations_sample_LU 150x1.csv", sep = ";", col.names = NA, row.names = TRUE)
# Destinations_sample <- read.csv("D:/Tamedia/Projekte/Feuerwehr/Daten/Destinations_sample_LS 150x1.csv", header = TRUE, sep=';')

## Prepare community borders ####
gemeinde_shp <- readOGR(dsn = "D:/Tamedia/Projekte/Feuerwehr/Daten/Gemeindeumriss/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp")
borders_df <- fortify(gemeinde_shp[gemeinde_shp$NAME == gemeinde, ])
nn <- length(borders_df$long) ## Umrechnen der Koordinaten der Gemeindegrenzen
border_coord <- array(0, dim=c(nn, 2))
i=1
for (i in 1:nn){ ## Umrechnen Koordinaten
  border_coord[i, ] <- rbind(CHtoWGS(borders_df$long[i], borders_df$lat[i]))
}
marker_size <- 80
## plot map ####
rm(m)
m <- leaflet()
m <- addTiles(m) # import map
#m <- addProviderTiles(m, 'Stamen.TonerBackground')
m <- addMarkers(m, lng=Origins[, 1], lat=Origins[, 2], popup="Feuerwache Süd") # make markers for the to Feuerwehr
#box_analyse <- Destinations_sample[Destinations_sample$time_notraffic > 1000, ]
m <- addCircles(m, Destinations_sample$coordLong, Destinations_sample$coordLat, color = Destinations_sample$marker_color, radius = marker_size)
m <- addPolylines(m, lng = border_coord[, 1], lat = border_coord[, 2], # Gemeinde borders
             stroke = TRUE, color = "black", weight = 5, opacity = 1, fill = FALSE, fillColor = "red",
             fillOpacity = 0, smoothFactor = 1, noClip = FALSE, options = pathOptions(), data = getMapData(m))
#m <- addLabelOnlyMarkers(m, Destinations_sample$coordLong[Destinations_sample$time_notraffic>1420], Destinations_sample$coordLat[Destinations_sample$time_notraffic>1420],
#              label = as.character(Destinations_sample$time_notraffic[Destinations_sample$time_notraffic>1420]), labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T),
#              options = markerOptions(), data = getMapData(m))
m # Karte ausgeben 
max(Destinations_sample$time_notraffic)

## Print Textanalysis ####
counts_green <- sum(Destinations_sample$marker_color == "green")
counts_orange <- sum(Destinations_sample$marker_color == "orange")
counts_red <- sum(Destinations_sample$marker_color == "red")

print(str_c(round(counts_green / sample_size*100, 0), "% der zufällig ausgewählten Adressen können innerhalb von 10 Minuten erreicht werden"))
print(str_c(round(counts_red / sample_size*100, 0), "% der zufällig ausgewählten Adressen können nicht innerhalb von 15 Minuten erreicht werden"))

## markers export as SVG ####

svg(filename = str_c("D:/Tamedia/Projekte/Feuerwehr/plots/plot_", str_c(gemeinde),"_Umriss.svg"))
plot1 <- plot(border_coord[, 1], border_coord[, 2], asp = 1, type = "n")
plot1 <- lines(border_coord[, 1], border_coord[, 2], asp = 1)
plot1 <- lines(Destinations_sample$coordLong, Destinations_sample$coordLat, col = Destinations_sample$marker_color, asp = 1, type = "p")
dev.off()

svg(filename = str_c("D:/Tamedia/Projekte/Feuerwehr/plots/plot_", str_c(gemeinde),".svg"))
plot2 <- plot(Destinations_sample$coordLong, Destinations_sample$coordLat, col = Destinations_sample$marker_color, asp = 1)
dev.off()




