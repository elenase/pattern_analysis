#############################################################################################
cat("Spatial aggregation of numerical data and join with geodata by country\n")
#############################################################################################
cat("Loading of neccessary data\n")

mapdata.adm0.shp <- readOGR("data/mapping/mapdata.adm0.spatial", "CountryAsylum")

class.data <- read.csv("data/lca/lca_class_data_16.csv", sep = ",")
data.lca.complete <- read.csv("data/lca/data_lca_complete.csv", sep = ",")
data.class.dummy <- cbind(data.lca.complete[2:7], dummy(class.data$lc.predclass, sep = ".")) 

country.codes <- read.csv("data/data_processed/country_asylum.csv")
country.codes$X <- NULL

#############################################################################################
cat("Date aggregation by Country of Asylum and calculation if percentage \n")

for (i in 1:16) {
    data <- summaryBy(data.class.dummy[i+6] ~ CountryAsylum, data=data.class.dummy, FUN=c(length,mean))
    data[3] <- round(data[3]*100)
    colnames(data) <- c("P_Code", "N", "pct")
    data <- merge(data, country.codes,by="P_Code")
    data[,c(1,2,5)] <- NULL
    colnames(data)[1] <- paste0("Class.",i)
     
    
    ## merge data with country codes to get iso3 for merge with json
    mapdata.adm0.shp <- sp::merge(mapdata.adm0.shp, data, by='iso3') ## merge data with shp

}

#rm(mapdata.adm0.shp)
#col <- colorspace::heat_hcl(12, c=c(95,70),l=c(20,90),power=c(1/12,3))
#col <- colorspace::sequential_hcl(12, c=c(100,80),l=c(30,90),power=c(1/5,3))
col <- c("#F4F6F6", "#B0D2CF", "#93C1C2", "#79ACB3", "#6195A3", "#4C7D93", "#396584", "#294E74", "#1C3964", "#102555", "#081545", "#020835")

at <- c(0,2,4,6,8,10,15,20,25,30,40,60,80)  
png(filename="out/lca/class_dist_adm0.png", res=300, 3000,3000)
spplot(mapdata.adm0.shp[,c(2:17)], main = "Relative frequency of cases by class and country of asylum in percent", col="gray", 
       col.regions=col,
       at=at,
       colorkey = list(labels = list( labels = c(0,2,4,6,8,10,15,20,25,30,40,60,80),   
                                      at=at,
                                      cex = 0.8)))
dev.off()
