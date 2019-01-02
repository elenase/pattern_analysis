#############################################################################################
cat("Spatial aggregation of numerical data and join with geodata by adm level 1 od asylum\n")
#############################################################################################
rm(list = ls())
#############################################################################################
cat("Loading of neccessary data\n")

mapdata.adm1.shp <- readOGR("data/mapping/mapdata.adm1.spatial", "coal1id")
mapdata.adm1.shp <- mapdata.adm1.shp <- mapdata.adm1.shp[!(mapdata.adm1.shp$iso3 == "MAR" | mapdata.adm1.shp$iso3 == "TUN" |
                                                             mapdata.adm1.shp$iso3 == "DZA" | mapdata.adm1.shp$iso3 == "LBY"),]
mapdata.adm1.shp@data$iso3 <- NULL
class.data <- read.csv("data/lca/lca_class_data_16.csv", sep = ",")
data.lca.complete <- read.csv("data/lca/data_lca_complete.csv", sep = ",")
data.class.dummy <- cbind(data.lca.complete[2:7], dummy(class.data$lc.predclass, sep = ".")) 

country.codes <- read.csv("data/data_processed/country_asylum.csv")
country.codes$X <- NULL


#############################################################################################
cat("Data aggregation by Country of Asylum and calculation of percentage \n")
########################################################################################

for (i in 1:16) {
  
  data <- summaryBy(data.class.dummy[i+6] ~ CountryAsylum + coal1id, data=data.class.dummy, FUN=c(length,mean))
  data[4] <- round(data[4]*100)
  colnames(data) <- c("P_Code", "idprogres", "N", "pct")
  data[,c("P_Code","N")] <- NULL
  colnames(data)[2] <- paste0("Class.",i)
  
  ## merge data with country codes to get iso3 for merge with json
  mapdata.adm1.shp <- sp::merge(mapdata.adm1.shp, data, by='idprogres', duplicateGeoms=TRUE) ## merge data with shp

}

#rm(mapdata.adm1.shp)
#col <- colorspace::heat_hcl(12, c=c(100,80),l=c(30,90),power=c(1/12,3))
#col <- colorspace::sequential_hcl(12, c=c(100,80),l=c(30,90),power=c(1/5,3))
col <- c("#F4F6F6", "#B0D2CF", "#93C1C2", "#79ACB3", "#6195A3", "#4C7D93", "#396584", "#294E74", "#1C3964", "#102555", "#081545", "#020835")
spplot(mapdata.adm1.shp[,c(2)], main = "Relative frequency of cases by class and country of asylum", col="gray", 
       col.regions=col,
       at=at,
       colorkey = list(labels = list(labels = c(0,2,4,6,8,10,15,20,25,30,40,60,80),   
                                     at=at,
                                     cex = 0.8)))

at <- c(0,2,4,6,8,10,15,20,25,30,40,60,80)  
spplot(classes.shp[,c(2)], main = "Relative frequency of cases by class and country of asylum", col="gray", 
       col.regions=col,
       at=at,
       colorkey = list(labels = list( labels = c(0,2,4,6,8,10,15,20,25,30,40,60,80),   
                                      at=at,
                                      cex = 0.8)))

