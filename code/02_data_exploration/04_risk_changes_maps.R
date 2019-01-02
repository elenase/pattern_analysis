#############################################################################################
cat("How did the occurence of risks change over time?")
#############################################################################################


data.regression <- read.csv("data/data_processed/data_regression.csv")
data.regression$X <- NULL
data.analysis <- read.csv("data/data_processed/data_analysis.csv")
conversion.back <- read.csv("data/data_processed/conversion_back.csv")
conversion.back$X <- NULL
basemap.risks <- get_map(location = "Syria", zoom=4, maptype="terrain")
data.risks <- cbind(data.regression, data.analysis$YearArrivalcat)
colnames(data.risks)[61] <- "YearArrivalcat"



#############################################################################################
cat("Visualizing temporal distribution of single risk categories\n")

a <- grep("^child.risk$", colnames(data.risks))
b <- grep("^seriousmed$", colnames(data.risks))
c <- data.risks[,a:b]

d <- cbind(data.risks$YearArrival, c)

colnames(d)[1] <- "YearArrival"
risk.yeararrival <- melt(d, id="YearArrival")  # convert to long format

risk.yeararrival$variable <- revalue(risk.yeararrival$variable, c("child.risk"="Children", "child.risk.separate"="Separated or unaccompanied children",
                                      "woman.risk"="Woman", "elderly.risk"="Elderly",
                                      "disability.risk"="Disability", "seriousmed"="Serious medical condition",
                                      "legalprot"="Specific legel and physical protection needs", "singleparent"="Torture",
                                      "famunit"="Family unity", "torture.risk"="Single parents",
                                      "sgbv.risk"="SGBV"))

ggplot(risk.yeararrival, aes(fill=variable, y=value, x=YearArrival)) + 
       geom_bar( stat="identity") +
       scale_x_continuous(breaks=seq(1950,2017,2), expand = c(0, 0)) +
       scale_y_continuous(labels = unit_format(unit = "k",scale = 0.001)) +
       theme(legend.position="right", legend.direction="vertical",
             legend.title = element_text(size=12, face="plain", color = "darkgray", hjust = 1),
             panel.grid.major.y = element_blank(),
             panel.grid.minor.y = element_blank(),
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank(),
             axis.text.x=element_text(angle=45,hjust=1),
             axis.title.y = element_blank(),
             plot.subtitle = element_text(size = 9, hjust = 0, color = "#595851")) +
       scale_fill_brewer(palette = "Set3",
                         name="Risk related to") +
       geom_hline(yintercept = c(0,0.25,0.50,0.75,1), color="darkgray", size=0.25) +
       labs(x = "Year of Arrival", y = NULL,
            title = ("Registered risks by category and year of arrival in the Middle East and North Africa"), 
            subtitle = "Data Source: UNHCR proGres data")

ggsave("out/data_exploration/risk_categories_by_yeararrival.png", width = 26, height = 12, units = c("cm"), dpi = 100)


## check if temporal autocorrelation is apparent in the overall occurence of risk
riskacf <- summaryBy(value ~ YearArrival, data = risk.yeararrival, FUN=sum)
acf(riskacf$value.sum, type="correlation", level=95, main="Temporal autocorrelation of risk occurence")
mtext("with 5% significance limits for the autocorrelations")
dev.copy(png,"out/data_exploration/risk_temporalautocor.png")
dev.off()
#############################################################################################
cat("Visualization of spatial changes of summarized risks and specific needs")


data.risks[,"risks"] <- rowSums(data.risks[,15:25])
data <- summaryBy(risks ~ coal1id + YearArrivalcat, data=data.risks, FUN=c(sum)) 
data <- dcast(data, coal1id ~ YearArrivalcat, value.var=colnames(data[3]))
colnames(data) <- c("idprogres","one","two","three","four")

data$onetwo <- (data$two - data$one) / data$one
data$twothree <- (data$three - data$two) / data$two
data$threefour <- (data$four - data$three) / data$three
data$onefour <- (data$four - data$one) / data$one

for (i in 1:nrow(data)) {
    data$onetwo[i][is.infinite(data$onetwo[i])] <- data$two[i]
    data$twothree[i][is.infinite(data$twothree[i])] <- data$three[i]
    data$threefour[i][is.infinite(data$threefour[i])] <- data$four[i]
    data$onefour[i][is.infinite(data$onefour[i])] <- data$four[i]
    
    data$onetwo[i][is.nan(data$onetwo[i])] <- 0
    data$twothree[i][is.nan(data$twothree[i])] <- 0
    data$threefour[i][is.nan(data$threefour[i])] <- 0
    data$onefour[i][is.nan(data$onefour[i])] <- 0
    }

mapdata <- plyr::join(x=mapdata.adm1, y=data, by="idprogres")  ## join with fortified geojson adm1

mapdata$breaksonetwo <- cut(mapdata$onetwo, breaks=c(-Inf, -100, -75, -50, -25, -0.01, 0, 25, 50, 75, 100, Inf), 
                            labels=c("<= -100", "(-100,75]", "(-75,-50]", "(-50,-25]", "(-25,-0.01]", "0", "(0,25]", "(25,50]", "(50,75]", "(75,100]",">100"))
mapdata$breakstwothree <- cut(mapdata$twothree, breaks=c(-Inf, -100, -75, -50, -25, -0.01, 0, 25, 50, 75, 100, Inf), 
                            labels=c("<= -100", "(-100,75]", "(-75,-50]", "(-50,-25]", "(-25,-0.01]", "0", "(0,25]", "(25,50]", "(50,75]", "(75,100]",">100"))
mapdata$breaksthreefour <- cut(mapdata$threefour, breaks=c(-Inf, -100, -75, -50, -25, -0.01, 0, 25, 50, 75, 100, Inf), 
                            labels=c("<= -100", "(-100,75]", "(-75,-50]", "(-50,-25]", "(-25,-0.01]", "0", "(0,25]", "(25,50]", "(50,75]", "(75,100]",">100"))
mapdata$breaksonefour <- cut(mapdata$onefour, breaks=c(-Inf, -100, -75, -50, -25, -0.01, 0, 25, 50, 75, 100, Inf), 
                               labels=c("<= -100", "(-100,75]", "(-75,-50]", "(-50,-25]", "(-25,-0.01]", "0", "(0,25]", "(25,50]", "(50,75]", "(75,100]",">100"))


clr <- c("<= -100" = "#00441b", "(-100,75]" = "#006d2c", "(-75,-50]" = "#238b45", "(-50,-25]" = "#41ab5d", "(-25,-0.01]" = "#74c476", 
         "0" = "white", 
         "(0,25]" = "#fb6a4a", "(25,50]" = "#ef3b2c", "(50,75]" = "#cb181d", "(75,100]" = "#a50f15",">100" = "#67000d")

#### 


risk.onetwo <- ggmap(basemap.risks) + 
               geom_polygon(data = mapdata, alpha=0.8, aes(fill = breaksonetwo, group = group, x = long, y = lat)) +
               geom_path(data = mapdata.adm0, color = "#3f3f3f", size = 0.5, aes(group = group, x = long, y = lat)) +
               scale_fill_manual(na.value = "#cccccc",
                                drop = FALSE,
                                values = clr,
                                guide = guide_legend(reverse=T, title = "% change", label.position = "right")) + 
               theme.base() + 
               coord_map(xlim = c(20.5, 70), ylim = c(6, 45)) +
               theme.confidence(plot.title = element_text(hjust = 0, color = "black", size = 17, face="plain")) +
               theme(legend.key.width = unit(6, units = "mm"), legend.direction = "vertical",
                    legend.title = element_text(size = 12, face="bold"),
                    legend.key = element_rect(colour = "#545454")) +
               labs(x = NULL, y = NULL,
                    title = "Percentage change of specific needs and risks from 1950-2011 to 2012",
                    subtitle = "",
                    caption = paste0("Thematic Data: UNHCR proGres\nShapefiles: UNHCR Github repository 'p-codes'\nBasemap: Google\nCreated: ",Sys.Date())
                    )
risk.onetwo
ggsave("out/maps/risk_change/risk_change1.png", width = 26, height = 26, units = c("cm"), dpi = 300)

####
risk.twothree <- ggmap(basemap.risks) + 
                 geom_polygon(data = mapdata, alpha=0.8, aes(fill = breakstwothree, group = group, x = long, y = lat)) +
                 geom_path(data = mapdata.adm0, color = "#3f3f3f", size = 0.5, aes(group = group, x = long, y = lat)) +
                 scale_fill_manual(na.value = "#cccccc",
                                   drop = FALSE,
                                   values = clr,
                                   guide = guide_legend(reverse=T, title = "% change", label.position = "right")) + 
                 theme.base() + 
                 coord_map(xlim = c(20.5, 70), ylim = c(6, 45)) +
                 theme.confidence(plot.title = element_text(hjust = 0, color = "black", size = 17, face="plain")) +
                 theme(legend.key.width = unit(6, units = "mm"), legend.direction = "vertical",
                       legend.title = element_text(size = 12, face="bold"),
                       legend.key = element_rect(colour = "#545454")) +
                 labs(x = NULL, y = NULL,
                      title = "Percentage change of specific needs and risks from 2012 to 2013",
                      subtitle = "",
                      caption = paste0("Thematic Data: UNHCR proGres\nShapefiles: UNHCR Github repository 'p-codes'\nBasemap: Google\nCreated: ",Sys.Date()))
risk.twothree
ggsave("out/maps/risk_change/risk_change2.png", width = 26, height = 26, units = c("cm"), dpi = 300)

####
risk.threefour <- ggmap(basemap.risks) + 
                  geom_polygon(data = mapdata, alpha=0.8, aes(fill = breaksthreefour, group = group, x = long, y = lat)) +
                  geom_path(data = mapdata.adm0, color = "#3f3f3f", size = 0.5, aes(group = group, x = long, y = lat)) +
                  scale_fill_manual(na.value = "#cccccc",
                                    drop = FALSE,
                                    values = clr,
                                    guide = guide_legend(reverse=T, title = "% change", label.position = "right")) + 
                  theme.base() + 
                  coord_map(xlim = c(20.5, 70), ylim = c(6, 45)) +
                  theme.confidence(plot.title = element_text(hjust = 0, color = "black", size = 17, face="plain")) +
                  theme(legend.key.width = unit(6, units = "mm"), legend.direction = "vertical",
                        legend.title = element_text(size = 12, face="bold"),
                        legend.key = element_rect(colour = "#545454")) +
                  labs(x = NULL, y = NULL,
                       title = "Percentage change of specific needs and risks from 2013 to 2014-2016",
                       subtitle = "",
                       caption = paste0("Thematic Data: UNHCR proGres\nShapefiles: UNHCR Github repository 'p-codes'\nBasemap: Google\nCreated: ",Sys.Date()))
risk.threefour
ggsave("out/maps/risk_change/risk_change3.png", width = 26, height = 26, units = c("cm"), dpi = 300)



####
distcenters <- aggregate(cbind(long, lat) ~ idprogres, data=mapdata, FUN=function(x)mean(range(x)))
colnames(distcenters) <- c("idprogres", "c.long", "c.lat")
mapdata <- plyr::join(x=mapdata, y=distcenters, by="idprogres")



####
risk.onefour <- ggmap(basemap.risks) + 
                geom_polygon(data = mapdata, alpha=0.8, aes(fill = breaksonefour, group = group, x = long, y = lat)) +
                geom_point(data=mapdata, aes(x=c.long, y=c.lat, size=four), shape=21, alpha=0.8, colour="#3f3f3f", stroke=1) + 
                geom_path(data = mapdata.adm0, color = "#3f3f3f", size = 0.5, aes(group = group, x = long, y = lat)) +
                scale_size_area(max_size = 10, guide = guide_legend(title= "absolute number\n2014-2016")) +
                scale_fill_manual(na.value = "#cccccc",
                                  drop = FALSE,
                                  values = clr,
                                  guide = guide_legend(reverse=T, title = "% change", label.position = "right")) + 
                coord_map(xlim = c(20.5, 70),ylim = c(6, 45)) +
                theme.confidence(plot.title = element_text(hjust = 0, color = "#3f3f3f", size = 17, face="plain"),
                                 axis.ticks=element_blank(), axis.text=element_blank(),
                                 panel.background = element_blank()) +
                theme(legend.key.width = unit(6, units = "mm"), legend.direction = "vertical",
                      legend.title = element_text(size = 12, face="bold"),
                      legend.key = element_rect(colour = "#545454")) +
                labs(x = NULL, y = NULL,
                     title = "Percentage change of specific needs and risks from 1950-2011 to 2014-2016",
                     subtitle = "",
                     caption = paste0("Thematic Data: UNHCR proGres\nShapefiles: UNHCR Github repository 'p-codes'\nBasemap: Google\nCreated: ",Sys.Date()))
risk.onefour
ggsave("out/maps/risk_change/risk_change1_4.png", width = 26, height = 26, units = c("cm"), dpi = 300)

####
risk.onefour <- ggmap(basemap.risks) + 
                geom_polygon(data = mapdata, alpha=0.8, aes(fill = breaksonefour, group = group, x = long, y = lat)) +
                geom_point(data=mapdata, aes(x=c.long, y=c.lat, size=four), shape=21, alpha=0.2, colour="black", stroke=1) + 
                geom_path(data = mapdata.adm0, color = "#3f3f3f", size = 0.5, aes(group = group, x = long, y = lat)) +
                scale_size_area(max_size = 10, guide = guide_legend(title= "absolute number\n2014-2016")) +
                scale_fill_manual(na.value = "#cccccc",
                                  drop = FALSE,
                                  values = clr,
                                  guide = guide_legend(reverse=T, title = "% change", label.position = "right")) + 
                coord_map(xlim = c(30, 50),ylim = c(30, 40)) +
                theme.confidence(plot.title = element_text(hjust = 0, color = "#3f3f3f", size = 17, face="plain"),
                                 axis.ticks=element_blank(), axis.text=element_blank(),
                                 panel.background = element_blank()) +
                theme(legend.key.width = unit(6, units = "mm"), legend.direction = "vertical",
                      legend.title = element_text(size = 10, face="bold"),
                      legend.key = element_rect(colour = "#545454")) +
                labs(x = NULL, y = NULL,
                     title = "Percentage change of specific needs and risks from 1950-2011 to 2014-2016",
                     subtitle = "",
                     caption = paste0("Thematic Data: UNHCR proGres\nShapefiles: UNHCR Github repository 'p-codes'\nBasemap: Google\nCreated: ",Sys.Date()))
risk.onefour
ggsave("out/maps/risk_change/risk_change_detail1_4.png", width = 26, height = 26, units = c("cm"), dpi = 300)



