############################################################################
cat("Load basemap for Middle East and North Africa with Egypt as center\n")
############################################################################
## this code is in an extra script because basemaps are sometimes not loaded
## loading must be repeated manually then (sometimes 2-3 times)


basemap <- get_map(location = "Egypt", zoom=3, maptype="terrain")

basemap.mena <- get_map(location = "Jerusalem", zoom=5, maptype="terrain")

basemap.yem <- get_map(location = "Yemen", zoom=6, maptype="terrain")

basemap.egy <- get_map(location = "Asyut", zoom=6, maptype="terrain")

basemap.lbn <- get_map(location = "Amman", zoom=7, maptype="terrain")

basemap.irq <- get_map(location = "Iraq", zoom=6, maptype="terrain")

basemap.syr <- get_map(location = "Syria", zoom=7, maptype="terrain")

