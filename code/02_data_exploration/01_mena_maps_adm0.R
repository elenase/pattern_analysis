#############################################################################################
cat("Spatial aggregation of numerical data and join with geodata by country\n")
#############################################################################################
keep(data.regression,
     data.cleaned,
     data.maps,
     map.titles,
     mapdata.adm0,
     mapdata.adm1,
     country.codes,
     theme.base,
     theme.choropleth,
     theme.confidence,
     theme.detail,
     theme.zscore,
     check,
     basemap,
     basemap.mena,
     basemap.yem,
     basemap.egy,
     basemap.lbn,
     basemap.irq,
     basemap.syr,
     sure = TRUE
     )

#############################################################################################
cat("Necessary Data: Activate data load if variables are not existent\n")

if (!exists("map.titles") == F |
    exists("data.regression") == F | exists("data.maps")  == F |
    exists("country.codes")  == F | exists("mapdata.adm0")  == F) {

    country.codes <- read.csv("data/reference_data/countrycodes.csv", sep = ",")
    data.regression <- read.csv("data/data_processed/data_regression.csv")
    data.maps <- read.csv("data/data_processed/data_maps.csv", sep = ",")
    map.titles <- read.csv("data/reference_data/map_titles.csv", sep = ";")
    data.cleaned <- read.csv("data/data_processed/data_cleaned.csv", sep = ",")
}
data.maps$X <- NULL
country.codes$X <- NULL
map.titles$X <-NULL
data.regression$X <-NULL
############################################################################
cat("Creation of variables\n")

##connect summarized risk variables with socioeconomic data
#data$maps <- plyr::join(x=data.maps, y=data.regression[,c(1,16:26)], by="CaseNo", type="left")

# activate for mapping of all columns
adm0.quanti <- splitmix(data.maps)$X.quanti  ## (splitmix is from package 'PCAmixdata')
adm0.quanti <- cbind(adm0.quanti, data.regression[,c(16:26)])

map.adm0.list.choro <- list()
map.adm0.list.conf <- list()
map.adm0.list.zscore <- list()
map.adm0.compilation <- list()
keytable.adm0 <- data.frame() ## to see at which position in list which variables is mapped


varmean <-c("Num_Inds", "dem_age", "AVG_Age", "STDEV_Age", "status.span", "Child_0_14_prop",
            "Youth_15_17_prop", "Work_18_64_prop", "Eldern_65_prop")


############################################################################
cat("Data aggregation by adm0 and join with geojson\n")

## summarized risk variables are 191 - 201 in map title reference table
for (l in 191:length(adm0.quanti)) {  #loop through all variables
  
  rm(p.zscore, p.map, p.confidence)
  
  ############################################################################
  cat(paste(colnames(adm0.quanti[l]), ": Data aggregation by adm0\n"))
  
  ## functions for aggregating data: main basis for maps
  data <-summaryBy(adm0.quanti[l] ~ CountryAsylum, data = data.maps, FUN = c(length, mean, sd, sum))
  
  num.inds <-summaryBy(Num_Inds ~ CountryAsylum, data = data.maps, FUN = c(sum))
  data <- merge(x=data, y=num.inds, by="CountryAsylum", type="left")
  colnames(data) <- c("P_Code", "N", "mean", "sd", "sum", "numinds")
  data$se <- data$sd / sqrt(data$N)   ## standard error of the mean
  data$pct <- data$sum / data$numinds * 100 ## percent
  data$zmean <- (data$mean - mean(data$mean)) / sd(data$mean)
  data$zpct <- (data$pct - mean(data$pct)) / sd(data$pct)
  ciMult <- qt(.95 / 2 + .5, data$N - 1)  ## conf.interval=.95
  data$ci <- data$se * ciMult
  
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

  ## merge data with country codes to get iso3 for merge with json
  data <- merge(data, country.codes, by = "P_Code")
  colnames(data)[15] <- "iso3"
  
  if (colnames(adm0.quanti[l]) %in% varmean) {
    f <- "mean"
    calculation <- "Average "
    g <- "zmean.breaks"
  } else {
    f <- "pct"
    calculation <- "Percentage of "
    g <- "zpct.breaks"
    }
  
  
  if (nrow(data) >= 1 && unique(data[, 3]) > 0) {
    ############################################################################
    cat(paste(colnames(adm0.quanti[l]), ": Join data with geojson\n"))
    keytable.adm0[l, 1] <- l
    keytable.adm0[l, 2] <- colnames(adm0.quanti[l])
    keytable.adm0[l, 3] <- map.titles[l, 2]
    
    mapdata <- plyr::join(x = mapdata.adm0, y = data, by = "iso3", type = "left")
    mapdata$brks <- 0
    #reorder data so that column 'brks' is at 1, first comma means keep all the rows
    mapdata <- mapdata[, c(which(colnames(mapdata) == "brks"), which(colnames(mapdata) != "brks"))]
    
    ############################################################################
    cat(paste(colnames(adm0.quanti[l]), ": Creation of fisher class breaks and rounded labels\n"))
    
    
    natural.breaks <- (classIntervals(data[, f], n = 5, style = "fisher", intervalClosure = 'right')$brks)
    
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
    ###################################################################################
    cat(paste(colnames(adm0.quanti[l]), ": Mapping Part\n"))
    
    
    if (check(p.number) == 0) {
      ## absolute number by country is for all variables equal, map needed only once
      ###################################################################################
      cat(paste(colnames(adm0.quanti[l]), ": Map creation: Symbol\n"))
      
      p.number <- ggplot(mapdata, aes(x = long, y = lat, group = group)) +
                  geom_polygon(colour = "#aaaaaa", aes(fill = N / sum(data$N))) +
                  scale_fill_gradient(low = "#deebf7",
                                      high = "#08306b",
                                      na.value = "gray",
                                      name = "%",
                                      labels = percent,
                                      guide = "colourbar") +
                  coord_equal() +
                  theme.base() + #map text and styling attributes
                  theme.confidence() +
                  theme(legend.key.width = unit(6, units = "mm"),
                        legend.direction = "vertical",
                        legend.title = element_text(size = 12),
                        plot.subtitle = element_text(size = 16),
                        plot.title = element_text(size = 16)) +
                  labs(x = NULL, y = NULL,
                      title = "Percentage number of cases",
                      subtitle = paste0("In totel ", sum(data$N)," registered cases are visualized"))
      #p.number
    }
    
    
    ############################################################################
    cat(paste(colnames(adm0.quanti[l]), ": Map creation: Choropleth\n"))

   p.map <- ggmap(basemap) +
            scale_alpha(name = "",
                        range = c(0.3, 0),
                        guide = F)  +
            geom_polygon(data = mapdata, aes(x = long, y = lat, group = group, alpha = 1)) + # administrative polygons
            geom_polygon(data = mapdata, colour = "#939393",
                         aes( x = long, y = lat, group = group, fill = brks )) +
            theme.base() +
            theme.choropleth() +     # map text and styling
            theme(plot.title = element_text(size = 18)) +
            scale_x_continuous(limits = c(-22.324219, 70.839844),
                               expand = c(0, 0)) +
            scale_y_continuous(limits = c(6.004452, 45.371659),
                               expand = c(0, 0)) +
            labs( x = NULL, y = NULL,
                  title = paste0(calculation, map.titles[l, 2], "\nby Country of Asylum"),
                  subtitle = "",
                  caption = paste0( "Source:\nThematic Data: UNHCR proGres\nShapefiles: UNHCR Github repository 'p-codes'\nBasemap: Google\nCreated: ", Sys.Date() ) ) +
            scale_fill_manual( "", values = rev(magma(8, alpha = 0.8)[2:7]),
                              breaks = rev(brks.scale),
                              drop = FALSE,
                              labels = labels.scale,
                              guide = guide_legend( byrow = T, reverse = T, label.position = "bottom" )
                              )
    #p.map
    map.adm0.list.choro[[l]] <- p.map ## each list element is one unvariate region map
    
    
    
    ##############################################################################
    cat(paste(colnames(adm0.quanti[l]), ": Map creation: z-scores\n"))
    
    legend.z <- c("(-Inf,-2.5]"="#012900", "(-2.5,-2]"="#034500", "(-2,-1.5]"="#156711","(-1.5,-1]"="#328a2e", "(-1,-0.5]"="#5aac56","(-0.5,-0.01]"="#8dcf8a", 
                  "(-0.01,0.01]" = "white", 
                  "(0.01,0.5]"="#ff9898", "(0.5,1]"="#ff5757","(1,1.5]"="#f02727","(1.5,2]"="#c30202", "(2,2.5]"="#8b0000", "(2.5, Inf]"= "#450000")
    
    
    p.zscore <- ggmap(basemap) +
      geom_polygon(data=mapdata, alpha=0.8, colour = "#aaaaaa", aes(fill = zmean.breaks, x = long, y = lat, group = group)) +
      scale_fill_manual(values = legend.z,
                        guide = guide_legend(reverse=T, title = "Z-Score", label.position = "right"),
                        na.value = "#cccccc",
                        drop = TRUE) +
      scale_x_continuous(limits = c(-22.324219, 70.839844), expand = c(0, 0)) +
      scale_y_continuous(limits = c(6.004452, 45.371659), expand = c(0, 0)) +
      theme.base() + #map text and styling attributes
      theme.confidence() +
      theme(plot.title = element_text(size = 16),
            legend.key.width = unit(6, units = "mm"), legend.direction = "vertical",
            legend.title = element_text(size = 12))+
      labs(x = NULL, y = NULL,
           title = "Z-Score", 
           subtitle = "")
    #p.zscore
    map.adm0.list.zscore[[l]] <- p.zscore ## each list element is one middle east map by variable
    
    
    
    ##############################################################################
    cat(paste(colnames(adm0.quanti[l]), ": Map creation: Margin of Error\n"))

    legend <- c("N*=1" = "#696969", "(-Inf,0.3]" = "#9dce3a", "(0.3,0.5]" = "#ffce3a", "(0.5,0.8]" = "#f0592b", "(0.8, Inf]" = "#ac0f0f")
    
    
    p.confidence <- ggmap(basemap) +
      geom_polygon(data=mapdata, alpha=0.8, colour = "#aaaaaa", aes(fill = error.breaks, x = long, y = lat, group = group)) +
      scale_fill_manual(values = legend, 
                        guide = guide_legend(title = "Margin of Error", label.position = "right"), 
                        drop=FALSE,
                        na.value="#cccccc") + 
      scale_x_continuous(limits = c(-22.324219, 70.839844), expand = c(0, 0)) +
      scale_y_continuous(limits = c(6.004452, 45.371659), expand = c(0, 0)) +
      theme.base() + #map text and styling attributes
      theme.confidence() +
      theme(plot.title = element_text(size = 16),
            legend.key.width = unit(6, units = "mm"), legend.direction = "vertical",
            legend.title = element_text(size = 12),
            plot.caption = element_text(size = 12)) +
      labs(x = NULL, y = NULL,
           title = paste0("Margin of Error for a 95%-confidence interval"),
           caption = "N* is total number of cases.\nMargin of Error can't be calculated in this case.")
    #p.confidence
    map.adm0.list.conf[[l]] <- p.confidence ## each list element is one middle east map by variable
    
    
  } else {
    keytable.adm0[l, 1] <- l
    keytable.adm0[l, 2] <- "NED" ## NED = Not Enough Data
    keytable.adm0[l, 3] <- map.titles[l, 2]
    rm(data)
  }
}






##############################################################################
cat("Explore created maps based on data frame keytable.adm0 and save optionally \n")

View(keytable.adm0)
key = 191  ## enter number of map according to key table
for (key in 191:201) {
     ## Single maps
     #map.adm0.list.choro[[key]]
     #map.adm0.list.zscore[[key]]
     #map.adm0.list.conf[[key]]
     #p.number
  
    ## Map compilation
    detailplot <- ggarrange(map.adm0.list.choro[[key]],
                            ggarrange(map.adm0.list.zscore[[key]], p.number, map.adm0.list.conf[[key]],
                                      align = "h", ncol = 1, nrow = 3),
                            ncol = 2, widths = c(1, 0.7))
    ## Detail plot
    ggsave(paste0("out/maps/adm0/",keytable.adm0[key,2], ".png"), width = 20, height = 10, dpi = 500, plot <- detailplot)
    graphics.off()
    }

#ggsave("out/data_exploration/adm0_map_number.png", width = 8, height = 6, dpi = 300)
##############################################################################
cat("Save keytable and lists containing all maps in adm0\n")

#keep(map.adm0.compilation, map.adm0.list.choro, map.adm0.list.zscore, map.adm0.list.conf, p.number, sure = TRUE)

# save(keytable.adm0, file="out/maps/maplist/adm0/adm0_map_key.csv")
# save(map.adm0.list.choro, file="out/maps/maplist/adm0/adm0_maplist_choro.RData")
# save(map.adm0.list.zscore, file="out/maps/maplist/adm0/adm0_maplist_zscore.RData")
# save(map.adm0.list.conf, file="out/maps/maplist/adm0/adm0_maplist_conf.RData")
# #save(map.adm0.compilation, file="out/maps/maplist/adm0/adm0_maplist_compilation.RData")

# # activate optional: load list containing all maps in adm0 and keytable
# map.adm0.list.choro <- load("out/maps/maplist/adm0/adm0_maplist_choro.RData")
# map.adm0.list.zscore <- load("out/maps/maplist/adm0/adm0_maplist_zscore.RData")
# map.adm0.list.conf <- load("out/maps/maplist/adm0/adm0_maplist_conf.RData")
# keytable.adm0 <- read.csv("out/maps/maplist/adm0/adm0_map_key.csv")
# map.adm0.compilation <- read.csv("out/maps/maplist/adm0/adm0_maplist_compilation.RData")
