
## Get Geodata #############################################################################
cat("GET GEODATA: Here all geodata needed for this project is load")
## make sure that all basemaps in the end of this script are load correctly
## sometimes a manual run is additionally needed
## admin level 2 is deacticated -> activate if needed

#############################################################################################
cat("Necessary Data: Activate data load if variables are not existent\n")

if (!exists("data.orig") == F | exists("country.asylum") == F ) {
    data.orig <- read.csv("data/data_processed/data_orig.csv")
    country.asylum <- read.csv("data/data_processed/country_asylum.csv")
    map.titles <- read.csv("data/reference_data/map_titles.csv", sep = ";")
}



#############################################################################################
cat("1) Get data\n")
############################################################################
cat("Load geojson: adm0 \n")

for (i in 1:nrow(country.asylum)) {
  iso3 <- country.asylum[i,"iso3"]
  filename <- paste0(iso3,"_ADM0.geojson")
  destfilepath <- paste0("data/mapping/geojson/adm0/", filename )
  geojsonurl <- paste0("http://raw.githubusercontent.com/unhcr-mena/p-codes/gh-pages/geojson/",iso3,"/ADM0.geojson")
  
  ## only download geojson if not existing for better performance
  if(!file.exists(destfilepath)){
    res <- tryCatch(download.file(geojsonurl, destfile=destfilepath ),
                    error=function(e) 1)
  }
}


############################################################################
cat("Load geojson: adm1 \n")

for (i in 1:nrow(country.asylum)) {
  iso3 <- country.asylum[i,"iso3"]
  filename <- paste0(iso3,"_ADM1.geojson")
  destfilepath <- paste0("data/mapping/geojson/adm1/", filename )
  geojsonurl <- paste0("http://raw.githubusercontent.com/unhcr-mena/p-codes/gh-pages/geojson/",iso3,"/ADM1.geojson")
  
  ## only download geojson if not existing for better performance
  if(!file.exists(destfilepath)){
    res <- tryCatch(download.file(geojsonurl, destfile=destfilepath ),
                    error=function(e) 1)
  }
}


# ############################################################################
# cat("Load geojson: adm2 \n")
# 
# for (i in 1:nrow(country.asylum)) {
#   iso3 <- country.asylum[i,"iso3"]
#   filename <- paste0(iso3,"_ADM2.geojson")
#   destfilepath <- paste0("data/mapping/geojson/adm2/", filename )
#   geojsonurl <- paste0("http://raw.githubusercontent.com/unhcr-mena/p-codes/gh-pages/geojson/",iso3,"/ADM2.geojson")
#   
#   ## only download geojson if not existing for better performance
#   if(!file.exists(destfilepath)){
#     res <- tryCatch(download.file(geojsonurl, destfile=destfilepath ),
#                     error=function(e) 1)
#   }
# }
# 
# 
# rm(geojsonurl)


#############################################################################################
#############################################################################################
cat("2) Merge country geojsons to one complete MENA geojson for admin level 0-2 \n")

mapdata.adm0 <- list()

for (i in 1:nrow(country.asylum)) {
  
      pcode <- country.asylum[i,"p_code"]
      iso3 <- country.asylum[i,"iso3"]
  
  ############################################################################
  cat(paste("Transforming geojsons country level adm 0", iso3,"\n"))
  
    filename <- paste0(iso3,"_ADM0.geojson")
    destfilepath <- paste0("data/mapping/geojson/adm0/", filename )
  
    if (file.info(destfilepath)$size > 0) {  ## check if json has data
        json.adm0 <- geojson_read(destfilepath, method="local", what="sp" )
        
        drops <- c("iso3")
        json.adm0 <- json.adm0[,(names(json.adm0) %in% drops)] ## select only columns of 'drops'
        
        # define crs WGS 84 / UTM zone 36N which is common for northern hemisphere and middle east
        json.adm0 <- spTransform(json.adm0, CRS("+proj=longlat +datum=WGS84")) 
        json.adm0 <- gBuffer(json.adm0, byid=TRUE, width=0)
        mapdata.adm0[[i]] <- (json.adm0)
    }
}

############################################################################
cat(paste(iso3,": Dissolving geojsons adm 0 \n"))

mapdata <- do.call("rbind", mapdata.adm0) ## bind all jsons to one large spatialpolygondataframe
if(!file.exists("data/mapping/mapdata.adm0.spatial")){
  writeOGR(mapdata, "data/mapping/mapdata.adm0.spatial", "CountryAsylum", driver="ESRI Shapefile")
}


############################################################################
cat(paste(iso3,": Fortifying geojson adm 0 -> makes it ggplot compatible \n"))

mapdata@data$id = rownames(mapdata@data)
mapdata.fortified <- fortify(mapdata, region = "id")
mapdata <- plyr::join(mapdata.fortified, mapdata@data, by="id") # joining dataframe with geojson
mapdata.adm0 <- mapdata[, c("long", "lat", "iso3", "group")] # only relevant columns to accelerate performance

############################################################################
cat(paste(iso3,": Save mena geojson adm 0 \n"))

write.csv(mapdata.adm0, file = "data/mapping/mena_adm0.csv") 


#############################################################################################
#############################################################################################
cat("Create json for whole middle east and north africa for adm1\n")

mapdata.adm1 <- list()

for (i in 1:nrow(country.asylum)) {
    pcode <- country.asylum[i,"p_code"]
    iso3 <- country.asylum[i,"iso3"]
  
  ############################################################################
  cat(paste("Transforming geojsons country level adm 1", iso3,"\n"))
  
    filename <- paste0(iso3,"_ADM1.geojson")
    destfilepath <- paste0("data/mapping/geojson/adm1/", filename )
    
    if (file.info(destfilepath)$size > 0) {  
      json.adm1 <- geojson_read(destfilepath, method="local", what="sp" )
      
      drops <- c("idprogres", "iso3") 
      json.adm1 <- json.adm1[,(names(json.adm1) %in% drops)]
      
      json.adm1 <- spTransform(json.adm1, CRS("+proj=longlat +datum=WGS84"))
      json.adm1 <- gBuffer(json.adm1, byid=TRUE, width=0)
      mapdata.adm1[[i]] <- (json.adm1)
    }
  }

############################################################################
cat(paste(iso3,": Dissolving geojsons adm 1 \n"))

mapdata <- do.call("rbind", mapdata.adm1) 

if(!file.exists("data/mapping/mapdata.adm1.spatial")){
   writeOGR(mapdata, "data/mapping/mapdata.adm1.spatial", "coal1id", driver="ESRI Shapefile")
  }

############################################################################
cat(paste(iso3,": Fortifying geojson adm 1 -> makes it ggplot compatible \n"))

mapdata@data$id = rownames(mapdata@data)
mapdata.fortified <- fortify(mapdata, region = "id")
mapdata <- plyr::join(mapdata.fortified, mapdata@data, by="id") 
mapdata.adm1 <- mapdata[, c("long", "lat", "idprogres", "iso3", "group")] 

############################################################################
cat(paste(iso3,": Save mena geojson adm 1 \n"))

write.csv(mapdata.adm1, file = "data/mapping/mena_adm1.csv") 



# #############################################################################################
# #############################################################################################
# cat("Create json for whole middle east and north africa for adm2\n")
# 
# mapdata.adm2 <- list()
# ## in adm2 json are missing: GCC, ISR, LBY, MAR, TUN -> List element with NULL produced
# for (i in 1:nrow(country.asylum)) {
#   
#     pcode <- country.asylum[i,"p_code"]
#     iso3 <- country.asylum[i,"iso3"]
#   
#   
#     ############################################################################
#     cat(paste("Loading geojsons country level adm 2", iso3,"\n"))
#     
#     
#     filename <- paste0(iso3,"_adm2.geojson")
#     destfilepath <- paste0("data/mapping/geojson/adm2/", filename )
#     
#     if (file.info(destfilepath)$size > 0) {  ## check if json has data
#       json.adm2 <- geojson_read(destfilepath, method="local", what="sp" )
#       
#       drops <- c("idprogres", "iso3") 
#       json.adm2 <- json.adm2[,(names(json.adm2) %in% drops)] 
#       
#       json.adm2 <- spTransform(json.adm2, CRS("+proj=longlat +datum=WGS84"))
#       json.adm2 <- gBuffer(json.adm2, byid=TRUE, width=0)
#       mapdata.adm2[[i]] <- (json.adm2)
#     } 
#   }
# 
# ############################################################################
# cat(paste(iso3,": Combining geojsons adm 2 to one file if adm 2 codes are existent \n"))
# 
# 
# mapdata.adm2 <- mapdata.adm2[lengths(mapdata.adm2) > 0] 
# mapdata <- do.call("rbind", mapdata.adm2) 
# 
# if(!file.exists("data/mapping/mapdata.adm2.spatial")){
#    writeOGR(mapdata, "data/mapping/mapdata.adm2.spatial", "coal2id", driver="ESRI Shapefile")
# }
# 
# ############################################################################
# cat(paste(iso3,": Fortifying geojson adm 2 -> makes it ggplot compatible \n"))
# 
# mapdata@data$id = rownames(mapdata@data)
# mapdata.fortified <- fortify(mapdata, region = "id")
# mapdata <- plyr::join(mapdata.fortified, mapdata@data, by="id") 
# mapdata.adm2 <- mapdata[, c("long", "lat", "idprogres", "iso3", "group")] 
# 
# ############################################################################
# cat(paste(iso3,": Save mena geojson adm 2 \n"))
# 
# write.csv(mapdata.adm2, file = "data/mapping/mena_adm2.csv") 



rm(pcode, iso3, filename, destfilepath, json.adm0, json.adm1, json.adm2, mapdata, mapdata.fortified)


