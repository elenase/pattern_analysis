#############################################################################################
cat("TIME ANALYSIS: Development of classes over time\n")
#############################################################################################
## load class data
class.data <- read.csv("data/lca/lca_class_data_16.csv", sep = ",")

## reconnect raw data 'Year of Arrival" with class data
cdt <- join(class.data, data.orig[,c("CaseNo","YearArrival")], by = "CaseNo")
cdt <- cdt[,c("YearArrival", "lc.predclass")]

## reshape data to table format for plot
cdt <- data.frame(table(cdt))


#############################################################################################
cat("Creation of line plot for visulaization of classes over time\n")
ggplot() + theme_fivethirtyeight() + 
  theme(plot.title=element_text(family="Atlas Grotesk Medium"),
        text=element_text(family="Atlas Grotesk Light")) +
  geom_bar(aes(y = Freq, x = YearArrival, group=lc.predclass, colour = lc.predclass), size=1.2, alpha=0.9,
              data = cdt, stat="identity") + 
  theme(legend.position="right", legend.direction="vertical",
        legend.title = element_blank()) +
  scale_x_discrete(breaks=seq(1950,2016,10)) +
  #scale_colour_manual(values=lc.predclass) +
  ggtitle("Number of cases by class over time") + labs(x="Year of arrival", y="Number of cases")


#############################################################################################
cat("Check temporal autocorrelation of classes\n")

tcordf <- data.frame()
lag.models <- list()
tscatter <- list()
tscatterdet <- list()
class <- 1
for (class in 1:16) {
  class.df <- cdt[ which(cdt$lc.predclass==class), ]
  class.df <- class.df$Freq
  a <- class.df[-length(class.df)]
  b <- class.df[-1]
  tcorplot <- plot(a, b, xlab='t', ylab='t-1')
  tcorplotdet <- plot(a, b, xlab='t', ylab='t-1', ylim=c(0, 100), xlim=c(0, 100)) ## detail view for low values
  tscatter[[class]] <- tcorplot
  tscatterdet[[class]] <- tcorplotdet
  
  tcor <- cor(a, b)
  tcordf[class, 1] <- class
  tcordf[class, 2] <- tcor
  
  name <- paste0("class ",class)
  ## lag-k autocorrelation
  acf(class.df, type="correlation", level=95, main=paste0("Temporal autocorrelation of ",name))
  mtext("with 5% significance limits for the autocorrelations")
  filename <- paste0("class",class)
  filepath <- paste0("out/lca/temporal_acf_", filename, ".png")
  dev.copy(png,filepath)
  dev.off()
}

## very strong temporal autocorrelation of classes
#############################################################################################

##This sample autocorrelation plots show that the time series is not random, 
##but rather has a high degree of autocorrelation between adjacent and near-adjacent 
##observations.
