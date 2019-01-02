#############################################################################################
cat("Spatial aggregation of numerical data and join with geodata by country\n")
#############################################################################################
keep(data.regression, data.maps, map.titles, mapdata.adm0, mapdata.adm1, mapdata.adm2,
     country.codes, 
     theme.base, theme.choropleth, theme.confidence, theme.detail, theme.zscore, check,
     basemap, basemap.detail.mena, basemap.detail.yem, basemap.detail.egy, basemap.detail.lbn,
     basemap.detail.irq, basemap.detail.syr,
     sure = TRUE)
#############################################################################################
cat("Necessary Data: Activate data load if variables are not existent\n")

if (exists("map.titles") == F | exists("data.regression") == F | exists("data.maps")  == F |
    exists("country.codes")  == F | exists("mapdata.adm2")  == F) {
  # source("code/00_preprocessing/00_packages.R")
  # source("code/00_preprocessing/04_data_processing_maps.R")
  # source("code/00_preprocessing/06_data_processing_regression.R")
  source("code/01_univariate_maps/00_get_geodata.R")
  
  country.codes <- read.csv("data/data_processed/country.codes.csv", sep = ",")
  map.titles <- read.csv("data/map_titles.csv", sep = ";")
  data.regression <- read.csv("data/data_processed/data_regression.csv")
  data.maps <- read.csv("data/data_processed/data_maps.csv", sep = ",")
  
}
data.maps$X <- NULL
data.maps$X.1 <- NULL

#############################################################################################
#############################################################################################
cat("Data aggregation by adm2 and join with geojson\n")

adm2.quanti <- splitmix(data.maps)$X.quanti  ## (splitmix is from package 'PCAmixdata')
adm2.quanti <- cbind(adm2.quanti, data.regression[16:26]) ## add summarized risk columns
map.adm2.list.choro <- list()
map.adm2.list.choro.detail <- list()
map.adm2.list.conf <- list()
map.adm2.list.zscore <- list() 
map.adm2.list.numbers.detail <- list()
keytable.adm2 <- data.frame() ## to see at which position in list which variables is mapped

varmean <-  c("Num_Inds", "dem_age", "AVG_Age", "STDEV_Age", "status.span", 
              "Child_0_14_prop", "Youth_15_17_prop", "Work_18_64_prop", "Eldern_65_prop")

#for (l in 1:length(adm2.quanti)) {     ##loop through all variables
  for (l in 1:5) {
  ############################################################################
  cat(paste(colnames(adm2.quanti[l]),": Data aggregation by adm2\n"))
  
  ## functions for aggregating data: main basis for maps
  data <- summaryBy(adm2.quanti[l] ~ coal2id + CountryAsylum, data=data.maps, FUN=c(length,mean,sd,sum)) 
  num.inds <- summaryBy(Num_Inds ~  coal2id + CountryAsylum, data=data.maps, FUN=c(sum)) 
  data <- cbind(data, as.numeric(num.inds$Num_Inds.sum))
  colnames(data) <- c("idprogres","P_Code", "N", "mean", "sd", "sum","numinds")
  data$se <- data$sd / sqrt(data$N)   ## standard error of the mean
  data$pct <- data$sum/data$numinds*100 ## percent
  data$zscore <- data$mean - mean(adm2.quanti[,l]) / sd(adm2.quanti[,l]) / sqrt(nrow(data))
  ciMult <- qt(.95/2 + .5, data$N-1)  ## conf.interval=.95
  data$ci <- data$se * ciMult
  data <- merge(data, country.codes,by="P_Code") ## merge data with country codes to get iso3 for merge with json
  
  ## breaks z-score
  data$zscore.breaks <- cut(data$zscore, 
                            breaks=c(-Inf, -2, -1, -0.001, 0.001, 1, 2, Inf), 
                            labels=c("<= -2", "(-2,-1]", "(-1,-0.001]", "~ 0", "(0.001,1]", "(1,2]", "> 2"),
                            include.lowest = FALSE, right = TRUE)
  
  ## breaks margin of error
  data$error.breaks <- cut(data$ci, 
                           breaks=c(-Inf, 0.3, 0.5, 0.8, Inf), 
                           labels=c("<= 0.3", "(0.3,0.5]", "(0.5,0.8]", "> 0.8"))
  

  if (colnames(adm2.quanti[l]) %in% varmean) {
    f <- "mean"
    calculation <- "Average "
  } else {
    
    f <- "pct"
    calculation <- "Percentage of "
  }
  
  
  ## check if we have data for this country and if we have at least one class in aggregated variable
  if (nrow(data) >= 1 && unique(data[,4]) > 0){  
    ############################################################################
    cat(paste(colnames(adm2.quanti[l]),": Join data with geojson\n"))
    keytable.adm2[l,1] <- l
    keytable.adm2[l,2] <- colnames(adm2.quanti[l])
    keytable.adm2[l,3] <- map.titles[l,2]
    
    mapdata <- plyr::join(x=mapdata.adm2, y=data, by="idprogres", type = "left")
    mapdata$brks <- 0
    mapdata <- mapdata[,c(which(colnames(mapdata)=="brks"),which(colnames(mapdata)!="brks"))] #reorder data so that column 'brks' is at 1, first comma means keep all the rows
    
    ############################################################################
    cat(paste(colnames(adm2.quanti[l]),": Creation of polygon centers for symbol map\n"))
    
    distcenters <- aggregate(cbind(long, lat) ~ idprogres, data=mapdata, 
                             FUN=function(x)mean(range(x)))
    distcenters <- plyr::join(x=distcenters, y=data, by="idprogres")
    
    
    ############################################################################
    cat(paste(colnames(adm2.quanti[l]),": Creation of fisher class breaks and rounded labels\n"))
    
    
    natural.breaks <- (classIntervals(data[,f], n = 5, style = "fisher", intervalClosure='right')$brks) 
    
    
    # fill column breaks with data and use rounded breaks as labels
    labels <- c()
    brks <- natural.breaks
    labels <- round(brks, 2)
    labels <- labels[2:length(labels)] ## remove starting value
    
    mapdata$brks <- cut(mapdata[[f]], 
                        breaks = brks, 
                        include.lowest = TRUE,
                        label = labels)
    brks.scale <- levels(mapdata$brks)
    labels.scale <- rev(brks.scale)
    
    
    
    ###################################################################################
    cat(paste(colnames(adm2.quanti[l]),": Data subsets for detail maps \n"))
    
    ## Detail Map EGY, JOR, LBN, SYR, IRQ, TUR
    mapdata.detail <- mapdata[(mapdata$iso3 == "EGY" | mapdata$iso3 == "JOR" | mapdata$iso3 == "IRQ" |
                               mapdata$iso3 == "LBN" | mapdata$iso3 == "SYR" | mapdata$iso3 == "TUR"), ]
    mapdata.detail.yem <- mapdata[(mapdata$iso3 == "YEM"), ]
    mapdata.detail.egy <- mapdata[(mapdata$iso3 == "EGY"), ]
    mapdata.detail.lbn <- mapdata[(mapdata$iso3 == "LBN"), ]
    mapdata.detail.irq<- mapdata[(mapdata$iso3 == "IRQ"), ]
    mapdata.detail.syr <- mapdata[(mapdata$iso3 == "SYR"), ]
    mapdata.detail.jord <- mapdata[(mapdata$iso3 == "JOR"), ]
    mapdata.detail.jor <- mapdata[(mapdata$iso3 == "JOR"| mapdata$iso3 == "LBN"), ]
    mapdata.detail.conf <- mapdata[(mapdata$iso3 == "JOR" | mapdata$iso3 == "LBN" | mapdata$iso3 == "EGY"), ]
    
    ###################################################################################
    ###################################################################################
    cat(paste(colnames(adm2.quanti[l]),": Mapping Part\n"))
    
    
    if (check(p.number.adm2) == 0) {     ## absolute number by country is for all variables equal, map needed only once
      ###################################################################################
      cat(paste(colnames(adm2.quanti[l]),": Map creation: Symbol\n"))
      
      p.number.adm2 <- ggplot(mapdata, aes(x = long, y = lat, group = group)) +
        #geom_polygon(data=mapdata, aes(x = long, y = lat, group = group, alpha = 0.6)) + # administrative polygons
        geom_polygon(colour = "#aaaaaa", aes(fill = N/sum(data$N))) +
        scale_fill_gradient(low="#deebf7", high="#08306b", na.value = "lightgray", 
                            name="% of cases by adm level 2",
                            labels = percent, 
                            guide = "colourbar")+
        coord_equal() +
        theme.base() + #map text and styling attributes
        theme.confidence() +
        theme(legend.key.width = unit(6, units = "mm"), legend.direction = "vertical",
              legend.title = element_text(size = 8)) +
        labs(x = NULL, y = NULL,
             title = paste0("Percentage number of registered cases by Admin Level 2 of Asylum"),
             subtitle = paste0("Total registered cases: ", sum(data$N)))
       p.number.adm2
      #ggsave("out/maps/maplist/adm2_map_number.png", width=8, height=6, dpi=300)

      ################################################################################## 
      ## DETAIL MAPS 
       
      p.number.detail.adm2 <- ggplot(mapdata.detail, aes(x = long, y = lat, group = group)) +
        geom_polygon(colour = "#aaaaaa", aes(fill = N/sum(data$N))) +
        scale_fill_gradient(low="#deebf7", high="#08306b", na.value = "gray", 
                            name="% of cases by adm level 2",
                            labels = percent, 
                            guide = "colourbar")+
        coord_equal() +
        theme(legend.key.width = unit(6, units = "mm"), legend.direction = "vertical",
              legend.title = element_text(size = 8)) +
        theme.base() + #map text and styling attributes
        theme(plot.margin = unit(c(2,0,1,0), "cm"))+
        labs(x = NULL, y = NULL,
             title = "MENA")
      
      #p.number.detail.adm2
      #ggsave("out/maps/maplist/adm2/adm2_map_numberdetail.png", width=6, height=6, dpi=300)
      
      
      p.number.detail.jor.adm2 <- ggplot(mapdata.detail.jord, aes(x = long, y = lat, group = group)) +
        geom_polygon(colour = "#aaaaaa", aes(fill = N/sum(data$N))) +
        scale_fill_gradient(low="#deebf7", high="#08306b", na.value = "gray", 
                            name="% of cases by adm level 2",
                            labels = percent, 
                            guide = "colourbar")+
        coord_equal() +
        theme(legend.key.width = unit(6, units = "mm"), legend.direction = "vertical",
              legend.title = element_text(size = 8)) +
        theme.base() + #map text and styling attributes
        theme(plot.margin = unit(c(2,0,1,0), "cm"))+
        labs(x = NULL, y = NULL,
             title = "JORDAN")
      #p.number.detail.jor.adm2
      #ggsave("out/maps/maplist/adm2/adm2_map_numberdetail_jor.png", width=6, height=6, dpi=300)
      
      p.number.detail.lbn.adm2 <- ggplot(mapdata.detail.lbn, aes(x = long, y = lat, group = group)) +
        geom_polygon(colour = "#aaaaaa", aes(fill = N/sum(data$N))) +
        scale_fill_gradient(low="#deebf7", high="#08306b", na.value = "gray", 
                            name="% of cases by adm level 2",
                            labels = percent, 
                            guide = "colourbar")+
        coord_equal() +
        theme(legend.key.width = unit(6, units = "mm"), legend.direction = "vertical",
              legend.title = element_text(size = 8)) +
        theme.base() + #map text and styling attributes
        theme(plot.margin = unit(c(2,0,1,0), "cm"))+
        labs(x = NULL, y = NULL,
             title = "LEBANON")
      #p.number.detail.lbn.adm2
      #ggsave("out/maps/maplist/adm2/adm2_map_numberdetail_lbn.png", width=6, height=6, dpi=300)
      
      p.number.detail.egy.adm2 <- ggplot(mapdata.detail.egy, aes(x = long, y = lat, group = group)) +
        geom_polygon(colour = "#aaaaaa", aes(fill = N/sum(data$N))) +
        scale_fill_gradient(low="#deebf7", high="#08306b", na.value = "gray", 
                            name="% of cases by adm level 2",
                            labels = percent, 
                            guide = "colourbar")+
        coord_equal() +
        theme(legend.key.width = unit(6, units = "mm"), legend.direction = "vertical",
              legend.title = element_text(size = 8)) +
        theme.base() + #map text and styling attributes
        theme(plot.margin = unit(c(2,0,1,0), "cm"))+
        labs(x = NULL, y = NULL,
             title = "EGYPT")
      #p.number.detail.egy.adm2
      #ggsave("out/maps/maplist/adm2/adm2_map_numberdetail_egy.png", width=6, height=6, dpi=300)


      detailplot.numbers <- ggarrange(p.number.detail.adm2, p.number.detail.egy.adm2, p.number.detail.lbn.adm2, p.number.detail.jor.adm2, 
                                      ncol=2, nrow=2,
                                      common.legend = TRUE, legend = "right")
      
      #detailplot.numbers
      #ggsave("out/maps/maplist/adm2/adm2_map_numberdetail_collage.png", width=6, height=6, dpi=300)
    }
    
    
    
    ############################################################################
    cat(paste(colnames(adm2.quanti[l]),": Map creation: Choropleth\n"))
    
    p.map <- ggmap(basemap) +
      scale_alpha(name = "", range = c(0.3, 0), guide = F)  +
      geom_polygon(data=mapdata, aes(x = long, y = lat, group = group, alpha = 1)) + # administrative polygons
      geom_polygon(data=mapdata, colour = "#939393", aes(x = long, y = lat, group = group, fill = brks)) +
      theme.base() +
      theme.choropleth() +     # map text and styling
      scale_x_continuous(limits = c(-22.324219, 70.839844), expand = c(0, 0)) +
      scale_y_continuous(limits = c(6.004452, 45.371659), expand = c(0, 0)) +
      labs(x = NULL, y = NULL,
           title = paste0(calculation, map.titles[l,2],"\nby Admin Level 2 of Asylum"), subtitle =  paste0(""),
           caption = paste0("Source:\nThematic Data: UNHCR proGres\nShapefiles: UNHCR Github repository 'p-codes'\nBasemap: Google\nCreated: ",Sys.Date())) +
      scale_fill_manual("",
                        values = rev(magma(8, alpha = 0.8)[2:7]), breaks = rev(brks.scale),
                        drop = FALSE,
                        labels = labels.scale,
                        guide = guide_legend(
                        byrow = T, reverse = T, label.position = "bottom"))
    p.map
    map.adm2.list.choro[[l]] <- p.map ## each list element is one middle east map by variable
    
    
    ############################################################################
    cat(paste(colnames(adm2.quanti[l]),": Detail maps\n"))
    
    
    p.map.mena <- ggmap(basemap.mena) +
      scale_alpha(name = "", range = c(0.3, 0), guide = F)  +
      geom_polygon(data=mapdata.detail, aes(x = long, y = lat, group = group, alpha = 1)) + # administrative polygons
      geom_polygon(data=mapdata.detail, colour = "#939393", aes(x = long, y = lat, group = group, fill = brks)) +
      theme.base() +
      theme.choropleth() +   # map text and styling
      theme.detail() +
      scale_fill_manual("",
                        values = rev(magma(8, alpha = 0.8)[2:7]), breaks = rev(brks.scale),
                        drop = FALSE,
                        labels = labels.scale,
                        guide = guide_legend(
                          byrow = T, reverse = T, label.position = "bottom")) +
      labs(x = NULL, y = NULL,
           title = "MENA")
    #p.map.mena
    
    p.map.yem <- ggmap(basemap.yem) +
      scale_alpha(name = "", range = c(0.3, 0), guide = F)  +
      geom_polygon(data=mapdata.detail.yem, aes(x = long, y = lat, group = group, alpha = 1)) + # administrative polygons
      geom_polygon(data=mapdata.detail.yem, colour = "#939393", aes(x = long, y = lat, group = group, fill = brks)) +
      theme.base() +
      theme.choropleth() +   # map text and styling
      theme.detail() +
      scale_fill_manual("",
                        values = rev(magma(8, alpha = 0.8)[2:7]), breaks = rev(brks.scale),
                        drop = FALSE,
                        labels = labels.scale,
                        guide = guide_legend(
                          byrow = T, reverse = T, label.position = "bottom")) +
      labs(x = NULL, y = NULL,
           title = "YEMEN")
    #p.map.yem
    
    p.map.egy <- ggmap(basemap.egy) +
      scale_alpha(name = "", range = c(0.3, 0), guide = F)  +
      geom_polygon(data=mapdata.detail.egy, aes(x = long, y = lat, group = group, alpha = 1)) + # administrative polygons
      geom_polygon(data=mapdata.detail.egy, colour = "#939393", aes(x = long, y = lat, group = group, fill = brks)) +
      theme.base() +
      theme.choropleth() +   # map text and styling
      theme.detail() +
      coord_map(xlim = c(25, 36),ylim = c(22, 31.7)) +
      scale_fill_manual("",
                        values = rev(magma(8, alpha = 0.8)[2:7]), breaks = rev(brks.scale),
                        drop = FALSE,
                        labels = labels.scale,
                        guide = guide_legend(
                          byrow = T, reverse = T, label.position = "bottom")) +
      labs(x = NULL, y = NULL,
           title = "EGYPT")
    #p.map.egy
    
    p.map.lbn <- ggmap(basemap.lbn) +
      scale_alpha(name = "", range = c(0.3, 0), guide = F)  +
      geom_polygon(data=mapdata.detail.jor, aes(x = long, y = lat, group = group, alpha = 1)) + # administrative polygons
      geom_polygon(data=mapdata.detail.jor, colour = "#939393", aes(x = long, y = lat, group = group, fill = brks)) +
      theme.base() +
      theme.choropleth() +   # map text and styling
      theme.detail() +
      scale_fill_manual("",
                        values = rev(magma(8, alpha = 0.8)[2:7]), breaks = rev(brks.scale),
                        drop = FALSE,
                        labels = labels.scale,
                        guide = guide_legend(
                          byrow = T, reverse = T, label.position = "bottom")) +
      labs(x = NULL, y = NULL,
           title = "LEBANON & JORDAN")
    #p.map.lbn
    
    p.map.syr <- ggmap(basemap.syr) +
      scale_alpha(name = "", range = c(0.3, 0), guide = F)  +
      geom_polygon(data=mapdata.detail.syr, aes(x = long, y = lat, group = group, alpha = 1)) + # administrative polygons
      geom_polygon(data=mapdata.detail.syr, colour = "#939393", aes(x = long, y = lat, group = group, fill = brks)) +
      theme.base() +
      theme.choropleth() +   # map text and styling
      theme.detail() +
      scale_fill_manual("",
                        values = rev(magma(8, alpha = 0.8)[2:7]), breaks = rev(brks.scale),
                        drop = FALSE,
                        labels = labels.scale,
                        guide = guide_legend(
                          byrow = T, reverse = T, label.position = "bottom")) +
      labs(x = NULL, y = NULL,
           title = "SYRIA")
    #p.map.syr
    
    p.map.irq <- ggmap(basemap.irq) +
      scale_alpha(name = "", range = c(0.3, 0), guide = F)  +
      geom_polygon(data=mapdata.detail.irq, aes(x = long, y = lat, group = group, alpha = 1)) + # administrative polygons
      geom_polygon(data=mapdata.detail.irq, colour = "#939393", aes(x = long, y = lat, group = group, fill = brks)) +
      theme.base() +
      theme.choropleth() +   # map text and styling
      theme.detail() +
      scale_fill_manual("",
                        values = rev(magma(8, alpha = 0.8)[2:7]), breaks = rev(brks.scale),
                        drop = FALSE,
                        labels = labels.scale,
                        guide = guide_legend(
                          byrow = T, reverse = T, label.position = "bottom")) +
      labs(x = NULL, y = NULL,
           title = "IRAQ")
    #p.map.irq
    
    
    
    detailplot <- ggarrange(p.map.mena, p.map.yem, p.map.egy, p.map.irq, p.map.syr, p.map.lbn, 
                            ncol=2, nrow=3,
                            common.legend = TRUE, legend = "bottom")
    #detailplot
    map.adm2.list.choro.detail[[l]] <- detailplot
    
    ##############################################################################
    cat(paste(colnames(adm2.quanti[l]),": Map creation: z-scores\n"))
    
    
    ## only labels occur in legend which are existent in the map
    legend.z <- c("> 2"="#781307", "(1,2]" = "#b83413", "(0.001,1]" = "#f0731e", "~ 0" = "white",
                  "(-1,-0.001]" = "#3c4eff", "(-2,-1]" = "#0917a0", "<= -2" = "#1a045c")
    
    
    p.zscore <- ggplot(mapdata, aes(x = long, y = lat, group = group)) +
      geom_polygon(colour = "#aaaaaa", aes(fill = zscore.breaks)) +
      scale_fill_manual(values = legend.z, 
                        guide = guide_legend(reverse=T, title = "Z-Score", label.position = "right"), 
                        na.value = "#cccccc",
                        drop = FALSE) + 
      coord_equal() +
      theme.base() + #map text and styling attributes
      theme.confidence() +
      theme(legend.key.width = unit(6, units = "mm"), legend.direction = "vertical",
            legend.title = element_text(size = 8)) +
      labs(x = NULL, y = NULL,
           title = "",
           caption = "")
    
    
    p.zscore.detail <- ggplot(mapdata.detail, aes(x = long, y = lat, group = group)) +
      geom_polygon(colour = "#aaaaaa", aes(fill = zscore.breaks)) +
      scale_fill_manual(values = legend.z, 
                        guide = guide_legend(reverse=T, title = "Z-Score", label.position = "right"), 
                        na.value = "#cccccc",
                        drop = FALSE) + 
      coord_equal() +
      theme.base() + #map text and styling attributes
      theme.confidence() +
      theme(legend.key.width = unit(6, units = "mm"), legend.direction = "vertical",
            legend.title = element_text(size = 8))+
      labs(x = NULL, y = NULL,
           title = "",
           caption = "")+
      labs(x = NULL, y = NULL,
           title = "EGYPT, JORDAN, LEBANON")
    
    detailplot <- ggarrange(p.zscore, p.zscore.detail,
                            ncol=2, nrow=1,
                            common.legend = TRUE, legend = "right")
    #detailplot
    map.adm2.list.zscore[[l]] <- detailplot ## each list element is one middle east map by variable
    
    
    ##############################################################################
    cat(paste(colnames(adm2.quanti[l]),": Map creation: Margin of Error\n"))
    
    
    # create dataframe of rows where N is 1 and Margin of Error couldn't be calculated
    n.isone <- data.frame()
    n.isone <- mapdata[mapdata$N == 1, ]
    n.isone <- subset(n.isone, !is.na(N))
    
    
    color <- c("#9dce3a", "#ffce3a", "#f0592b", "#ac0f0f")
    labels <- c("<= 0.3", "(0.3,0.5]", "(0.5,0.8]", "> 0.8")
    legend <- c("<= 0.3" = "#9dce3a", "(0.3,0.5]" = "#ffce3a", "(0.5,0.8]" = "#f0592b", "> 0.8" = "#ac0f0f")
    
    
    if( nrow(n.isone) > 0) {
      
      p.confidence <- ggplot(mapdata, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill="#696969")) +     # administrative polygons
        geom_polygon(colour = "#aaaaaa", aes(fill = error.breaks)) +
        geom_polygon(data=n.isone,  aes(x = long, y = lat, group = group, fill= "#D7DBDD")) +
        scale_fill_manual(values = c("#D7DBDD","#696969", color), 
                          guide = guide_legend(title = "Margin of Error", label.position = "right"), 
                          labels = c('No data', "N* = 1", labels),
                          drop=FALSE) +
        coord_equal() +
        theme.base() + #map text and styling attributes
        theme.confidence() +
        theme(legend.key.width = unit(6, units = "mm"), legend.direction = "vertical",
              legend.title = element_text(size = 8)) +
        labs(x = NULL, y = NULL,
             title = "",
             caption = "N* is total number of cases.\nMargin of Error can't be calculated in this case.")
      
      n.isone.detail <- n.isone[(n.isone$iso3 == "EGY" | n.isone$iso3 == "JOR" | n.isone$iso3 == "LBN"), ]
      p.confidence.detail <- ggplot(mapdata.detail.conf, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill="#696969")) +     # administrative polygons
        geom_polygon(colour = "#aaaaaa", aes(fill = error.breaks)) +
        geom_polygon(data=n.isone.detail,  aes(x = long, y = lat, group = group, fill= "#D7DBDD")) +
        scale_fill_manual(values = c("#D7DBDD","#696969", color), 
                          guide = guide_legend(title = "Margin of Error", label.position = "right"), 
                          labels = c('No data', "N* = 1", labels)) +
        coord_equal() +
        theme.base() + #map text and styling attributes
        theme.confidence() +
        theme(legend.key.width = unit(6, units = "mm"), legend.direction = "vertical",
              legend.title = element_text(size = 8)) +
        labs(x = NULL, y = NULL,
             title = "EGYPT, JORDAN, LEBANON")
      #p.confidence.detail
      
      detailplot <- ggarrange(p.confidence, p.confidence.detail,
                              ncol=1, nrow=2,
                              common.legend = TRUE, legend = "right")
      #detailplot
      
    } else
    {
      p.confidence <- ggplot(mapdata, aes(x = long, y = lat, group = group)) +
        geom_polygon(colour = "#aaaaaa", aes(fill = error.breaks)) +  
        scale_fill_manual(values = legend, 
                          guide = guide_legend(title = "Margin of Error", label.position = "right"), 
                          na.value = "#cccccc",
                          drop=FALSE) + 
        coord_equal() +
        theme.base() + # map text and styling
        theme.confidence() +
        theme(legend.key.width = unit(6, units = "mm"), legend.direction = "vertical",
              legend.title = element_text(size = 8)) +
        labs(x = NULL, y = NULL,
             title = "",
             caption = "N* is total number of cases.\nMargin of Error can't be calculated in this case.")
      
      
      p.confidence.detail <- ggplot(mapdata.detail.conf, aes(x = long, y = lat, group = group)) +
        geom_polygon(colour = "#aaaaaa", aes(fill = error.breaks)) +  
        scale_fill_manual(values = legend, 
                          guide = guide_legend(title = "Margin of Error", label.position = "right"), 
                          na.value = "#cccccc",
                          drop=FALSE) + 
        coord_equal() +
        theme.base() + # map text and styling
        theme.confidence() + 
        theme(legend.key.width = unit(6, units = "mm"), legend.direction = "vertical",
              legend.title = element_text(size = 8)) +
        labs(x = NULL, y = NULL, 
             title = "")+
        labs(x = NULL, y = NULL,
             title = "EGYPT, JORDAN, LEBANON")
      
      detailplot <- ggarrange(p.confidence, p.confidence.detail,
                              ncol=1, nrow=2,
                              common.legend = TRUE, legend = "right")
      #detailplot
    }
    
    map.adm2.list.conf[[l]] <- detailplot ## each list element is one middle east map by variable
    
    
    
  } else {
    keytable.adm2[l,1] <- l
    keytable.adm2[l,2] <- "NED" ## NED = Not Enough Data
    keytable.adm2[l,3] <- map.titles[l,2]
    rm(data)
  }
}





##############################################################################
cat("Visualize and optionally save maps\n")

key = 1     ## enter number of map according to key table
map.adm2.list.choro[[key]]
ggsave("extra/out/maps/adm2/map_mena_Num_Inds.png", width=20, height=10, dpi=300)
map.adm2.list.choro.detail[[key]]
map.adm2.list.zscore[[key]]
map.adm2.list.conf[[key]]
p.number.adm2
p.number.detail.adm2
detailplot.numbers


##############################################################################
cat("Save keytable and lists containing all maps in adm2\n")
# save(keytable.adm2, file="out/maps/maplist/adm2/adm2_map_key.csv")
# save(map.adm2.list.choro, file="out/maps/maplist/adm2/adm2_maplist_choro.RData")
# save(map.adm2.list.choro.detail, file="out/maps/maplist/adm2/adm2_maplist_choro_detail.RData")
# save(map.adm2.list.zscore, file="out/maps/maplist/adm2/adm2_maplist_zscore.RData")
# save(map.adm2.list.conf, file="out/maps/maplist/adm2/adm2_maplist_conf.RData")

# # activate optional: load list containing all maps in adm2 and keytable
# map.adm2.list.choro <- load("out/maps/maplist/adm2/adm2_maplist_choro.RData")
# map.adm2.list.choro.detail <- load("out/maps/maplist/adm2/adm2_maplist_choro_detail.RData")
# map.adm2.list.zscore <- load("out/maps/maplist/adm2/adm2_maplist_zscore.RData")
# map.adm2.list.conf <- load("out/maps/maplist/adm2/adm2_maplist_conf.RData")
# keytable.adm2 <- read.csv("out/maps/maplist/adm2/adm2_map_key.csv")