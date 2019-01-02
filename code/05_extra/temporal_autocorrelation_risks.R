##############################################################################################
cat("Check temporal autocorrelation of risk categories in dependence on year of arrival \n")

a <- grep("^child.risk$", colnames(data))
b <- grep("^seriousmed$", colnames(data))
c <- grep("^YearArrival$", colnames(data))
data.time <- data[c(c,a:b)]
colnames(data.time) <- c("YearArrival",
                         "Children", "Separated or unaccompanied children", 
                         "Woman", "Elderly", "Disability", "Serious medical condition",
                         "Specific legel and physical protection needs", "Torture", "SGBV", 
                         "Family unity", "Single parents")

keytable.risk <- data.frame()
tcordf <- data.frame()
lag.models <- list()
tscatter <- list()
tscatterdet <- list()

riskcat = 2
## loop through all risk categories
for (riskcat in 2:length(data.time)) {
  #time.table <- data.frame(table(data.time[,c("child.risk", "YearArrival")]))
  time.table <- data.frame(table(data.time[,c(riskcat, 1)]))
  time.table <- time.table$Freq
  d <- time.table[-length(time.table)]
  e <- time.table[-1]
  
  ## scatterplot riskcat over time
  tcorplot <- plot(d, e, xlab='t', ylab='t-1')
  tcorplotdet <- plot(d, e, xlab='t', ylab='t-1', ylim=c(0, 100), xlim=c(0, 100)) ## detail view for low values
  tscatter[[riskcat-1]] <- tcorplot
  tscatterdet[[riskcat-1]] <- tcorplotdet
  
  ## temporal correlation
  tcor <- cor(d, e)
  tcordf[riskcat-1, 1] <- colnames(data.time[riskcat])
  tcordf[riskcat-1, 2] <- tcor
  
  ## lag-k autocorrelation (acf = autocorrelation function):
  ## The “lag” (time span between observations) is shown along the horizontal
  ## the autocorrelation is on the vertical 
  ## values are correlation coefficients
  ## Blue lines indicate bounds for statistical significance (95%)  
  
  acf(time.table, type="correlation", level=95, main=paste0("Autocorrelation Function for ",colnames(data.time[riskcat]), " related risks"))
  mtext("with 5% significance limits for the autocorrelations")
  name <- paste0("acf_risk_", colnames(data.time[riskcat]))
  filepath <- paste0("out/regression/temporal_autocorrelation/", name, ".png")
  
  dev.copy(png,filepath)
  dev.off()
  #ggsave(filepath, width = 26, height = 12, units = c("cm"), dpi = 100)
  #  lag.models[[riskcat-1]] <- lagmod
  
  keytable.risk[riskcat-1, 1] <- riskcat
  keytable.risk[riskcat-1, 2] <- colnames(data.time[riskcat])
}

## no or only low temporal autocorrelation
## risk categories are more randomly distributed over time

## lag models of risk categories
lagmod
