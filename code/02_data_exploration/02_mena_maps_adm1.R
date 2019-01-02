#############################################################################################
cat("Spatial aggregation of numerical data and join with geodata by country\n")
#############################################################################################
#############################################################################################
keep(data.regression, data.maps, map.titles, mapdata.adm0, mapdata.adm1, 
     country.codes, 
     theme.base, theme.choropleth, theme.confidence, theme.detail, theme.zscore, check,
     basemap, basemap.mena, basemap.yem, basemap.egy, basemap.lbn,
     basemap.irq, basemap.syr,
     sure = TRUE)

#############################################################################################
cat("Necessary Data: Activate data load if variables are not existent\n")

if (!exists("map.titles") == F | exists("data.regression") == F | exists("data.maps")  == F |
    exists("country.codes")  == F | exists("mapdata.adm1")  == F) {
  
  ## data
  data.regression <- read.csv("data/data_processed/data_regression.csv")
  data.maps <- read.csv("data/data_processed/data_maps.csv", sep = ",")
  
  ## reference data
  country.codes <- read.csv("data/reference_data/countrycodes.csv", sep = ",")
  map.titles <- read.csv("data/reference_data/map_titles.csv", sep = ";")

}
data.maps$X <- NULL
country.codes$X <- NULL
map.titles$X <-NULL
data.regression$X <-NULL

############################################################################
cat("Creation of variables\n")

adm1.quanti <- splitmix(data.maps)$X.quanti  ## (splitmix is from package 'PCAmixdata')
adm1.quanti <- cbind(adm1.quanti, data.regression[,c(15:26)])

map.adm1.list.choro <- list()
map.adm1.list.choro.detail <- list()
map.adm1.list.conf <- list()
map.adm1.list.zscore <- list()
explain.detail <- list()
keytable.adm1 <- data.frame() ## to see at which position in list which variables is mapped

varmean <-  c("Num_Inds", "dem_age", "AVG_Age", "STDEV_Age", "status.span", 
              "Child_0_14_prop", "Youth_15_17_prop", "Work_18_64_prop", "Eldern_65_prop")


############################################################################
cat("Data aggregation by adm1 and join with geojson\n")
l=191
for (l in 191:length(adm1.quanti)) {  
  ###########################################################################
  cat(paste(colnames(adm1.quanti[l]),": Data aggregation by adm1\n"))
  
  ## functions for aggregating data: main basis for maps
  data <-summaryBy(adm1.quanti[l] ~ coal1id, data = data.maps, FUN = c(length, mean, sd, sum))
  
  num.inds <-summaryBy(Num_Inds ~ coal1id, data = data.maps, FUN = c(sum))
  data <- merge(x=data, y=num.inds, by="coal1id", type="left")
#  data <- cbind(data, as.numeric(num.inds$Num_Inds.sum))
  data[7] <- NULL
  colnames(data) <- c("idprogres", "N", "mean", "sd", "sum", "numinds")
  data$se <- data$sd / sqrt(data$N)   ## standard error of the mean
  data$pct <- data$sum / data$numinds * 100 ## percent
  data$zmean <- (data$mean - mean(data$mean)) / sd(data$mean)
  data$zpct <- (data$pct - mean(data$pct)) / sd(data$pct)
  ciMult <- qt(.95 / 2 + .5, data$N - 1)  ## conf.interval=.95
  data$ci <- data$se * ciMult
#  data <- merge(data, country.codes,by="P_Code") ## merge data with country codes to get iso3 for merge with json
  
  
  ## breaks z-score mean
  data$zmean.breaks <- cut(data$zmean,
                           breaks = c(-Inf, -2.5, -2, -1.5, -1, -0.5, -0.01, 0.01, 0.5, 1, 1.5, 2, 2.5, Inf),
                           include.lowest = FALSE,
                           right = TRUE)
  
  ## breaks z-score percent
  data$zpct.breaks <- cut(data$zpct,
                          breaks = c(-Inf, -2.5, -2, -1.5, -1, -0.5, -0.01, 0.01, 0.5, 1, 1.5, 2, 2.5, Inf),
                          include.lowest = FALSE,
                          right = TRUE)
  
  
  ## breaks margin of error
  data$error.breaks <- cut(data$ci, 
                           breaks=c(-Inf, 0.3, 0.5, 0.8, Inf))
  
  data$error.breaks <- as.character(data$error.breaks)
  data$error.breaks[data$N == 1] <- "N*=1"
  
  
  if (colnames(adm1.quanti[l]) %in% varmean) {
    f <- "mean"
    calculation <- "Average "
    g <- "zmean.breaks"
  } else {
    f <- "pct"
    calculation <- "Percentage of "
    g <- "zpct.breaks"
  }
  
  
  if (nrow(data) >= 1 && unique(data[,4]) > 0){
    
    ############################################################################
    cat(paste(colnames(adm1.quanti[l]),": Join data with geojson\n"))
    
    keytable.adm1[l,1] <- l
    keytable.adm1[l,2] <- colnames(adm1.quanti[l])
    keytable.adm1[l,3] <- map.titles[l,2]
    
    mapdata <- plyr::join(x=mapdata.adm1, y=data, by="idprogres", type = "left")
    mapdata$brks <- 0
    #reorder data so that column 'brks' is at 1
    mapdata <- mapdata[,c(which(colnames(mapdata)=="brks"),which(colnames(mapdata)!="brks"))] 
    
    
    ############################################################################
    cat(paste(colnames(adm1.quanti[l]),": Creation of fisher class breaks and rounded labels\n"))
    
    natural.breaks <- (classIntervals(mapdata[,f], n = 5, style = "fisher", intervalClosure='right')$brks) 
    
    # fill column breaks with data and use rounded breaks as labels
    labels <- c()
    brks <- natural.breaks
    labels <- round(brks, 2)
    labels <- labels[2:length(labels)] ## remove starting value
    
    mapdata$brks <- cut(mapdata[[f]], breaks = brks, include.lowest = TRUE, label = labels)
    brks.scale <- levels(mapdata$brks)
    labels.scale <- rev(brks.scale)
    
    
    ###################################################################################
    cat(paste(colnames(adm1.quanti[l]),": Data subsets for detail maps \n"))
    
    ## Detail map of region where data at adm 2 is available: EGY, JOR, LBN, SYR, IRQ, TUR, IRN, GCC, YEM
    mapdata.detail <- mapdata[(mapdata$iso3 == "EGY" | mapdata$iso3 == "JOR" | mapdata$iso3 == "IRQ" |
                               mapdata$iso3 == "LBN" | mapdata$iso3 == "SYR" | mapdata$iso3 == "TUR" |
                               mapdata$iso3 == "IRN"| mapdata$iso3 == "GCC"| mapdata$iso3 == "YEM"), ]
    mapdata.adm0.detail <- mapdata.adm0[(mapdata.adm0$iso3 == "EGY" | mapdata.adm0$iso3 == "JOR" | mapdata.adm0$iso3 == "IRQ" |
                                      mapdata.adm0$iso3 == "LBN" | mapdata.adm0$iso3 == "SYR" | mapdata.adm0$iso3 == "TUR" |
                                      mapdata.adm0$iso3 == "IRN" | mapdata.adm0$iso3 == "GCC" | mapdata.adm0$iso3 == "YEM"), ]
    
    ## Some country subsets for detail view
    mapdata.detail.yem <- mapdata[(mapdata$iso3 == "YEM"), ]
    mapdata.detail.egy <- mapdata[(mapdata$iso3 == "EGY"), ]
    mapdata.detail.lbn <- mapdata[(mapdata$iso3 == "LBN"), ]
    mapdata.detail.irq <- mapdata[(mapdata$iso3 == "IRQ"), ]
    mapdata.detail.syr <- mapdata[(mapdata$iso3 == "SYR"), ]
    mapdata.detail.jor <- mapdata[(mapdata$iso3 == "JOR"| mapdata$iso3 == "LBN"), ]
    
 
    ###################################################################################
    ###################################################################################
    cat(paste(colnames(adm1.quanti[l]),": Mapping Part\n"))
    
    
    if (check(p.number.adm1) == 0) {     ## absolute number by country is for all variables equal, map needed only once
      ###################################################################################
      cat(paste(colnames(adm1.quanti[l]),": Map creation: Cases in percent\n"))

      p.number.adm1 <- ggplot(mapdata, aes(x = long, y = lat, group = group)) +
        geom_polygon(colour = "#aaaaaa", aes(fill = N/sum(data$N), group = group)) +
        geom_path(data = mapdata.adm0, color = "#3f3f3f", size = 0.5, aes(group = group, x = long, y = lat)) +
        scale_fill_gradient(low="#deebf7", high="#08306b", na.value = "gray", 
                            name="% of cases by adm unit",
                            labels = percent, 
                            guide = "colourbar")+
        coord_equal() +
        theme.base() + #map text and styling attributes
        theme.confidence() +
        theme(legend.key.width = unit(6, units = "mm"), legend.direction = "vertical",
              legend.title = element_text(size = 8)) +
        labs(x = NULL, y = NULL,
             title = paste0("Percentage number of registered cases by administrative level 1 of asylum"),
             subtitle = paste0("Total registered cases: ", sum(data$N)))
      #p.number.adm1
      #ggsave("out/maps/maplist/adm1_map_number.png", width=8, height=6, dpi=300)
      
      

      p.number.detail.adm1 <- ggplot(mapdata.detail, aes(x = long, y = lat, group = group)) +
        geom_polygon(colour = "#aaaaaa", aes(fill = N/sum(data$N))) +
        geom_path(data = mapdata.adm0.detail, color = "#3f3f3f", size = 0.5, aes(group = group, x = long, y = lat)) +
        scale_fill_gradient(low="#deebf7", high="#08306b", na.value = "gray", 
                            name="% of cases by adm unit",
                            labels = percent, 
                            guide = "colourbar")+
        coord_equal() +
        theme(legend.key.width = unit(6, units = "mm"), legend.direction = "vertical",
              legend.title = element_text(size = 8)) +
        theme.base() + #map text and styling attributes
        theme(plot.margin = unit(c(2,0,1,0), "cm"))+
        labs(x = NULL, y = NULL,
             title = "")
      
      #p.number.detail.adm1
      #ggsave("out/maps/adm1/adm1_map_numberdetail.png", width=6, height=6, dpi=300)
      
      
      p.number.detail.yem.adm1 <- ggplot(mapdata.detail.yem, aes(x = long, y = lat, group = group)) +
        geom_polygon(colour = "#aaaaaa", aes(fill = N/sum(data$N))) +
        scale_fill_gradient(low="#deebf7", high="#08306b", na.value = "gray", 
                            name="% of cases by adm unit",
                            labels = percent, 
                            guide = "colourbar")+
        coord_equal() +
        theme(legend.key.width = unit(6, units = "mm"), legend.direction = "vertical",
              legend.title = element_text(size = 8)) +
        theme.base() + #map text and styling attributes
        theme(plot.margin = unit(c(2,0,1,0), "cm"))+
        labs(x = NULL, y = NULL,
             title = "YEMEN")
      #p.number.detail.yem.adm1
      #ggsave("out/maps/adm1/adm1_map_numberdetail_yem.png", width=6, height=6, dpi=300)
      
      detailplot.numbers <- ggarrange(p.number.adm1, p.number.detail.adm1, p.number.detail.yem.adm1,
                                      ncol=2, nrow=2,
                                      common.legend = TRUE, legend = "right")
      
      #detailplot.numbers
      #ggsave("out/maps/adm1/adm1_map_numberdetail_collage.png", width=6, height=6, dpi=300)
    }
    
    
    
    ############################################################################
    cat(paste(colnames(adm1.quanti[l]),": Map creation: Choropleth\n"))
    
  p.map <- ggmap(basemap) +
    scale_x_continuous(limits = c(-22.324219, 70.839844), expand = c(0, 0)) +
    scale_y_continuous(limits = c(6.004452, 45.371659), expand = c(0, 0)) +
    scale_alpha(name = "", range = c(0.3, 0), guide = F)  +
    geom_polygon(data=mapdata, aes(x = long, y = lat, group = group, alpha = 1)) + # administrative polygons
    geom_polygon(data=mapdata, colour = "#939393", aes(x = long, y = lat, group = group, fill = brks)) +
    geom_path(data = mapdata.adm0, color = "#3f3f3f", size = 0.5, aes(group = group, x = long, y = lat)) +
    theme.base() +
    theme.choropleth() +
    labs(x = NULL, y = NULL,
         title = paste0(calculation, map.titles[l,2],"\nby administrative level 1 of asylum"), subtitle =  paste0(""),
         caption = paste0("Source:\nThematic Data: UNHCR proGres\nShapefiles: UNHCR Github repository 'p-codes'\n
                          Basemap: Google\nCreated: ",Sys.Date())) +
    scale_fill_manual("",
                      values = rev(magma(8, alpha = 0.8)[2:7]), breaks = rev(brks.scale),
                      drop = FALSE,
                      labels = labels.scale,
                      guide = guide_legend(
                      byrow = T, reverse = T, label.position = "bottom"))
  #p.map
  map.adm1.list.choro[[l]] <- p.map ## each list element is one univariate regional map
    
    
  ############################################################################
  cat(paste(colnames(adm1.quanti[l]),": Detail maps\n"))
    
    
  p.map.mena <- ggmap(basemap.mena) +
    scale_alpha(name = "", range = c(0.3, 0), guide = F)  +
    geom_polygon(data=mapdata.detail, aes(x = long, y = lat, group = group, alpha = 1)) + # administrative polygons
    geom_polygon(data=mapdata.detail, colour = "#939393", aes(x = long, y = lat, group = group, fill = brks)) +
    geom_path(data = mapdata.adm0.detail, color = "#3f3f3f", size = 0.5, aes(group = group, x = long, y = lat)) +
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
    map.adm1.list.choro.detail[[l]] <- detailplot
    
    ##############################################################################
    cat(paste(colnames(adm1.quanti[l]),": Map creation: z-scores\n"))

    legend.z <- c("(-Inf,-2.5]"="#012900", "(-2.5,-2]"="#034500", "(-2,-1.5]"="#156711","(-1.5,-1]"="#328a2e", "(-1,-0.5]"="#5aac56","(-0.5,-0.01]"="#8dcf8a", 
                  "(-0.01,0.01]" = "white", 
                  "(0.01,0.5]"="#ff9898", "(0.5,1]"="#ff5757","(1,1.5]"="#f02727","(1.5,2]"="#c30202", "(2,2.5]"="#8b0000", "(2.5, Inf]"= "#450000")


    p.zscore.detail <- ggmap(basemap) +
      geom_polygon(data=mapdata.detail, alpha=0.8, colour = "#aaaaaa", aes(fill = zpct.breaks, x = long, y = lat, group = group)) +
      geom_path(data = mapdata.adm0.detail, color = "#3f3f3f", size = 0.5, aes(group = group, x = long, y = lat)) +
      scale_fill_manual(values = legend.z,
                        guide = guide_legend(reverse=T, title = "Z-Score", label.position = "right"),
                        na.value = "#cccccc",
                        drop = TRUE) +
      scale_x_continuous(limits = c(22.324219, 70.839844), expand = c(0, 0)) +
      scale_y_continuous(limits = c(6.004452, 45.371659), expand = c(0, 0)) +
      theme.base() + #map text and styling attributes
      theme.confidence() +
      theme(plot.title = element_text(size = 20),
            legend.key.width = unit(6, units = "mm"), legend.direction = "vertical",
            legend.title = element_text(size = 8))+
      labs(x = NULL, y = NULL,
            title = paste0("Z-score of ", map.titles[l,2],"\nby administrative level 1 of asylum"), 
            subtitle =  paste0(""))

    # p.zscore.detail
    map.adm1.list.zscore[[l]] <- p.zscore.detail ## each list element is one middle east map by variable
    
 
    ##############################################################################
    cat(paste(colnames(adm1.quanti[l]),": Map creation: Margin of Error\n"))
    
    legend <- c("N*=1" = "#696969", "(-Inf,0.3]" = "#9dce3a", "(0.3,0.5]" = "#ffce3a", "(0.5,0.8]" = "#f0592b", "(0.8, Inf]" = "#ac0f0f")

      p.confidence.detail <- ggmap(basemap) +
        geom_polygon(data=mapdata.detail, alpha=0.8, colour = "#aaaaaa", aes(fill = error.breaks, x = long, y = lat, group = group)) +
        geom_path(data = mapdata.adm0.detail, color = "#3f3f3f", size = 0.5, aes(group = group, x = long, y = lat)) +
        scale_fill_manual(values = legend, 
                          guide = guide_legend(title = "Margin of Error", label.position = "right"), 
                          drop=FALSE,
                          na.value="#cccccc") + 
        scale_x_continuous(limits = c(22.324219, 70.839844), expand = c(0, 0)) +
        scale_y_continuous(limits = c(6.004452, 45.371659), expand = c(0, 0)) +
        theme.base() + #map text and styling attributes
        theme.confidence() +
        theme(plot.title = element_text(size = 20),
              legend.key.width = unit(6, units = "mm"), legend.direction = "vertical",
              legend.title = element_text(size = 8)) +
        labs(x = NULL, y = NULL,
             title = paste0("Margin of error for a 90%-confidence interval"),
             caption = "N* is total number of cases.\nMargin of Error can't be calculated in this case.")
      #p.confidence.detail

    map.adm1.list.conf[[l]] <- p.confidence.detail ## each list element is one middle east map by variable
    
    
    ## compilation of z-score and margon or error
    detailplot <- ggarrange(p.zscore.detail, p.confidence.detail,
                            ncol=1, nrow=2)
    #detailplot
    explain.detail[[l]] <- detailplot
    
  } else {
    keytable.adm1[l,1] <- l
    keytable.adm1[l,2] <- "NED" ## NED = Not Enough Data
    keytable.adm1[l,3] <- map.titles[l,2]
    rm(data)
  }
}





##############################################################################
cat("visualize maps and save optionally")

View(keytable.adm1)
key = 191   ## enter number of map according to key table

for (key in 191:201) {
  
  p<- map.adm1.list.choro[[key]]
  ggsave(paste0("out/maps/adm1/",keytable.adm1[key,2],"_mena.png"), width=20, height=10, dpi=300, plot = p)
  graphics.off()
  
  q<- map.adm1.list.choro.detail[[key]]
  ggsave(paste0("out/maps/adm1/",keytable.adm1[key,2],"_detail.png"), width=10, height=10, dpi=300, plot = q)
  graphics.off()
  
  r<- explain.detail[[key]]
  ggsave(paste0("out/maps/adm1/",keytable.adm1[key,2],"_explain.png"), width=10, height=20, dpi=300, plot = r)
  graphics.off()

  # map.adm1.list.zscore[[key]]
  # ggsave(paste0("out/maps/adm1/",name,"_zscore.png"), width=20, height=10, dpi=300)
  # 
  # map.adm1.list.conf[[key]]
  # ggsave(paste0("out/maps/adm1/",name,"_error.png"), width=10, height=10, dpi=300)
  # p.number.adm1
  # p.number.detail.adm1
  
}



##############################################################################
cat("Save keytable and lists containing all maps in adm1\n")
# save(keytable.adm1, file="out/maps/maplist/adm1/adm1_map_key.csv")
# save(map.adm1.list.choro, file="out/maps/maplist/adm1/adm1_maplist_choro.RData")
# save(map.adm1.list.choro.detail, file="out/maps/maplist/adm1/adm1_maplist_choro_detail.RData")
# save(map.adm1.list.zscore, file="out/maps/maplist/adm1/adm1_maplist_zscore.RData")
# save(map.adm1.list.conf, file="out/maps/maplist/adm1/adm1_maplist_conf.RData")

# # activate optional: load list containing all maps in adm1 and keytable
# map.adm1.list.choro <- load("out/maps/maplist/adm1/adm1_maplist_choro.RData")
# map.adm1.list.choro.detail <- load("out/maps/maplist/adm1/adm1_maplist_choro_detail.RData")
# map.adm1.list.zscore <- load("out/maps/maplist/adm1/adm1_maplist_zscore.RData")
# map.adm1.list.conf <- load("out/maps/maplist/adm1/adm1_maplist_conf.RData")
# keytable.adm1 <- read.csv("out/maps/maplist/adm1/adm1_map_key.csv")