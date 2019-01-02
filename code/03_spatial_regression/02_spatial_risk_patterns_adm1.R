# IN PROGRESS
#############################################################################################
cat("Geographically weighted regression (GWR) on UNHCR risk categories \n")
#############################################################################################
cat("Necessary Data: Activate data load if variables are not existent\n")
# source("code/00_preprocessing/00_packages.R")
# source("code/00_preprocessing/06_data_processing_regression.R")
# source("code/01_univariate_maps/00_get_geodata.R")
mapdata.adm1.shp <- readOGR("data/mapping/mapdata.adm1.spatial", "coal1id")

admnames <- as.character(unique(data$coal1i)) ## extract unique admnames
admnames <- admnames[!((nchar(admnames))<6)]


data <- read.csv("data/data_processed/data_regression.csv")
data$X <- NULL

## data cleaning
admnames <- unique(data$coal1id)
data <- data[!is.na(data$coal1id), ] ## removes rows without spatial id
data <- data[!(data$coal1id == ""),]
admnames <- as.character(unique(data$coal1i)) ## extract unique admnames
admnames <- admnames[!((nchar(admnames))<6)] ## remove incorrect admnames

mapdata.adm1.shp <- mapdata.adm1.shp[!is.na(mapdata.adm1.shp$coal1id), ]


#############################################################################################
cat("1) Model specification\n")
#############################################################################################

cat("1.1) Child at risks cohort\n")
## building formula with risk category as dependent variable
regfun <- function(x)  {
  dat <- data[data$coal1id == x, ]
  #dat <- data[data$coal1id == admnames, ]
  # m <- glm(child.risk~Num_Inds+Child_0_14+Work_18_64+Youth_15_17+
  #            AVG_Age+STDEV_Age+YearArrival+dem_age+female.headed+status_span+
  #            edu_highestcat.dummy.ElementarySchool+edu_highestcat.dummy.HigherEducation+edu_highestcat.dummy.HighSchool+edu_highestcat.dummy.MiddleSchool+edu_highestcat.dummy.NE+  ##education
  #            occupationcat.dummy.Agricultural.Craft.Machine+occupationcat.dummy.Elementary+occupationcat.dummy.Manager.Professional.Technician.Clerk+ ## occupation
  #            occupationcat.dummy.Military+occupationcat.dummy.ServiceMarket+occupationcat.dummy.Student.or.NoOccup+  ## occupation
  #            CountryOrigincat.dummy.AFG+CountryOrigincat.dummy.AFR+CountryOrigincat.dummy.ASIA+CountryOrigincat.dummy.HORN+CountryOrigincat.dummy.IRN+CountryOrigincat.dummy.IRQ+CountryOrigincat.dummy.MENA+CountryOrigincat.dummy.OTH+  ## country origin cohort
  #            dem_religioncat.dummy.Christian+dem_religioncat.dummy.Muslim+dem_religioncat.dummy.Other+dem_religioncat.dummy.Shia+ ## religion cohort
  #            RefStatus.dummy.ASR+RefStatus.dummy.NOC, data=dat) ## Refugee Status
  m <- glm(child.risk~
             edu_highestcat.dummy.ElementarySchool+edu_highestcat.dummy.MiddleSchool+edu_highestcat.dummy.NE+  ##education
             occupationcat.dummy.Manager.Professional.Technician.Clerk+ ## occupation
             CountryOrigincat.dummy.AFG+CountryOrigincat.dummy.HORN+CountryOrigincat.dummy.IRQ, data=dat)  ## country origin cohort
          
  
  attributeNames <- summary(m)$coefficients[, 0] 
  estimate <- summary(m)$coefficients[, 1]
  t.values <- summary(m)$coefficients[, 3]
  comb <- data.frame(estimate,t.values)
  #return(m) # A
  #return(comb)
  return(estimate)
  #return(t.values)
}

## check summary to see global linear relations and significance values
## A must be activated in function regfun to see summary
m<-regfun(admnames) 
summary(m)
vif(m) # vif identifies colinearity important for model specification


## regression coefficients (estimates)
res <- sapply(admnames, regfun)

e.list <- list()

for (i in 1:length(res)) {
  
  df <- res[[i]]
  df <- data.frame(df)
  colnames(df) <- admnames[i]
  df$var <- rownames(df)
  e.list[[i]] <- df
}


## local t-values
res.t <- sapply(admnames, regfun)

t.list <- list()

for (i in 1:length(res.t)) {
  
  df <- res.t[[i]]
  df <- data.frame(df)
  colnames(df) <- admnames[i]
  df$var <- rownames(df)
  t.list[[i]] <- df
}




## Plot of regression coefficients
############################################################################################

res.e = Reduce(function(...) merge(..., all=T), e.list)
res.e = res.e[!(res.e$var=="1"),]
rownames(res.e) <- res.e$var
res.e$var <- NULL

png(filename="out/spatial_regression/dotchart_elementary.png")
dotchart(sort(as.numeric(res.e[,5])), labels=row.names(res.tm), cex=0.65)
#shows variation of the regression coefficient (beta)
dev.off()

resdf <- data.frame(name=colnames(res.e), t(res.e))
colnames(resdf)[1] <- "idprogres"
colnames(resdf)[2] <- "child.risk"

cnres <- merge(mapdata.adm1.shp, resdf, by='idprogres')
## spplot(cnres, 'Num_Inds') optional to see the result for one variable

# a copy of the data
cnres.childrisk <- cnres
# scale all variables, except the spatial names and spatial identifier
cnres.childrisk@data = data.frame(scale(data.frame(cnres)[, -c(1,2)]))

my.palette <- rev(brewer.pal(n = 11, name = "RdBu"))
png(filename="out/spatial_regression/e_value_child_coo.png")
spplot(cnres.childrisk[,c(1:4)], main = "", colorkey=list(space="bottom"), 
       col = "#666666", col.regions = my.palette, cuts = 10)    ##
dev.off()

png(filename="out/spatial_regression/e_value_child_education.png")
spplot(cnres.childrisk[,c(1,5:7)], main = "", colorkey=list(space="bottom"), 
       col = "#666666", col.regions = my.palette, cuts = 10)    ##
dev.off()



## Plot of t-values
############################################################################################

res.t = Reduce(function(...) merge(..., all=T), t.list)
res.t = res.t[!(res.t$var=="1"),]
rownames(res.t) <- res.t$var
res.t$var <- NULL

#dotchart(sort(res.tm["edu_highestcat.dummy.ElementarySchool",]), labels=row.names(res.tm), cex=0.65)
#shows variation of the regression coefficient (beta)

resdf <- data.frame(name=colnames(res.e), t(res.e))
colnames(resdf)[1] <- "idprogres"
colnames(resdf)[2] <- "child.risk"

cnres <- merge(mapdata.adm1.shp, resdf, by='idprogres')
## spplot(cnres, 'Num_Inds') optional to see the result for one variable


# a copy of the data
cnres.childrisk <- cnres
# scale all variables, except the spatial names and spatial identifier
cnres.childrisk@data = data.frame(scale(data.frame(cnres)[, -c(1,2)]))

my.palette <- brewer.pal(n = 11, name = "BrBG")
png(filename="out/spatial_regression/t_value_child_coo.png")
spplot(cnres.childrisk[,c(1:4)], main = "", colorkey=list(space="bottom"), 
       col = "#666666", col.regions = my.palette, cuts = 10)    ##
dev.off()

png(filename="out/spatial_regression/t_value_child_education.png")
spplot(cnres.childrisk[,c(1,5:7)], main = "", colorkey=list(space="bottom"), 
       col = "#666666", col.regions = my.palette, cuts = 10)    ##
dev.off()



#############################################################################################
#############################################################################################
cat("PART 2: Check for significance - spatial autocorrelation or randomness? \n")
library(spdep)
p.values <- data.frame()
## Moran's I p-value: significant negative or positive means significant positive or negative autocorrelation
## -> no random distribution, null-hypothesis can be rejected

## Monte Carlo simulation of Moran's I tests the significance of Moran's I p-value
## Here, a pseudo-p-value is calculated that
## Null hypothesis: there is no spatial autocorrelation -> the result of GWR is random
## -> (Complete Spatial Randomness, CSR)
##  It calculates tha probability that the null hypothesis is true
## -> if z-values are significant positive or negative there is high probability that
## the null-hypothesis is true, the result random
## Small P-values = small probability that null-hypothesis is true, are evidence against
## the null hypothesis and in favor of a real effect in the population
## http://www.math.chalmers.se/Stat/Grundutb/CTH/tms150/1516/moore14.pdf

# ## Create neighbors list
# We begin by reproducing the contiguity neighbours used by Waller & Gotway; tracts sharing boundary
# points are taken as neighbours, using the poly2nb function
nb <- poly2nb(cnres.childrisk)
plot(cnres.childrisk, col='gray', border='black', lwd=2)
plot(nb, coordinates(cnres.childrisk), col='red', lwd=2, add=TRUE)

## check nb list
nb

## check position of country in nb list where 1 connection is found
which(card(nb) == 1)
# [1]   3  15  28  54 140 145 240

## check position of country in spatialpolygonsobject
attr(nb, "region.id")[which(card(nb) == 1)]
# [1] "50"  "62"  "75"  "101" "209" "221" "340"

cnres$idprogres[62]
# [1] DZA EGY GCC IRN IRQ JOR LBY LBN MAR SYR TUN TUR YEM
# Levels: DZA EGY GCC IRN IRQ JOR LBN LBY MAR SYR TUN TUR YEM
# -> LBN is without link
# -> Link to SYR must be added manually
# count position of SYR in Levels of cnres = 10
# check by looking at neihbors of 10 in nb is also SYR


## add link between LBN and SYR manually (8 = LBN, 10 = SYR)
nb[[8]] <- as.integer(10)

plot(cnres.childrisk, col='gray', border='black', lwd=2)
plot(nb, coordinates(cnres.childrisk), col='red', lwd=2, add=TRUE)

## add also links between EGY and JOR as well as GCC
## count position of Country in Level of spatialpolygonsdataframe = position in nb 
## and check  consistency with neighboring links
## EGY = nb[[2]]
## JOR = nb[[6]]
## GCC = nb[[3]]
## add links at 2. and 3. position as one link is already existing
nb[[1]][3] <- as.integer(6)
nb[[2]][3] <- as.integer(3)

plot(cnres.childrisk, col='gray', border='black', lwd=2)
plot(nb, coordinates(cnres.childrisk), col='red', lwd=2, add=TRUE)


lw <- nb2listw(nb)
mc1 <- moran.mc(cnres.childrisk[[1]], lw,  na.action=na.omit, zero.policy=TRUE, nsim=10000)
mc2 <- moran.mc(cnres.childrisk[[2]], lw,  na.action=na.omit, zero.policy=TRUE, nsim=10000)
mc3 <- moran.mc(cnres.childrisk[[3]], lw,  na.action=na.omit, zero.policy=TRUE, nsim=10000)
mc4 <- moran.mc(cnres.childrisk[[4]], lw,  na.action=na.omit, zero.policy=TRUE, nsim=10000)
mc5 <- moran.mc(cnres.childrisk[[5]], lw,  na.action=na.omit, zero.policy=TRUE, nsim=10000)
mc6 <- moran.mc(cnres.childrisk[[6]], lw,  na.action=na.omit, zero.policy=TRUE, nsim=10000)
mc7 <- moran.mc(cnres.childrisk[[7]], lw,  na.action=na.omit, zero.policy=TRUE, nsim=10000)
mc8 <- moran.mc(cnres.childrisk[[8]], lw,  na.action=na.omit, zero.policy=TRUE, nsim=10000)

OP <- par(mfrow = c(2,2))

plot(mc1, main=sprintf("Child risk Moran's I (p = %0.3f)", mc1$p.value),xlab=NA,sub=NA)
abline(v=mc1$statistic, col="red", lw=2)
plot(mc2, main=sprintf("Country of Origin AFG, Moran's I (p = %0.3f)", mc2$p.value),xlab=NA,sub=NA)
abline(v=mc2$statistic, col="red", lw=2)
plot(mc3, main=sprintf("Country of Origin HORN, Moran's I (p = %0.3f)", mc3$p.value),xlab=NA,sub=NA)
abline(v=mc3$statistic, col="red", lw=2)
plot(mc4, main=sprintf("Country of Origin IRQ, Moran's I (p = %0.3f)", mc4$p.value),xlab=NA,sub=NA)
abline(v=mc4$statistic, col="red", lw=2)
plot(mc5, main=sprintf("Elementary School, Moran's I (p = %0.3f)", mc5$p.value),xlab=NA,sub=NA)
abline(v=mc5$statistic, col="red", lw=2)
plot(mc6, main=sprintf("Middle School, Moran's I (p = %0.3f)", mc6$p.value),xlab=NA,sub=NA)
abline(v=mc6$statistic, col="red", lw=2)
plot(mc7, main=sprintf("No Education, Moran's I (p = %0.3f)", mc7$p.value),xlab=NA,sub=NA)
abline(v=mc7$statistic, col="red", lw=2)
plot(mc8, main=sprintf("Occupation Man.Tec.Cl, Moran's I (p = %0.3f)", mc8$p.value),xlab=NA,sub=NA)
abline(v=mc8$statistic, col="red", lw=2)





for (i in 1:length(cnres.childrisk)) {
  
  p.values[i,1] <- round(mc$p.value,2)
  p.values[i,2] <- round(mc$statistic,2)
  p.values[i,3] <- names(cnres.childrisk[1])
}


## calculate Monte Carlo Simulation of Moran's I to test significance of spatial regressions
## the smaller the p-values the more significant the result is
for (i in 1:length(tmp.list)) {
  for (j in 2:length(tmp.list[[1]])) {

    ## defines that at least 6 coefficients (~50%) must be calculated to consider variable
    ## n is adjusted automatically to number of not NA in mc.moran
    if (length(unique(tmp.list[[i]][,j])) > 6){

      mc <- moran.mc(tmp.list[[i]][,j], lw, na.action=na.omit, zero.policy=TRUE, nsim=599)
      p.values [j-1,i+1] <- round(mc$p.value,1)

      significance.values[j-1,1] <- colnames(tmp.list[[i]][j])
      significance.values[j-1,i+1] <- round(mc$statistic,2)
    } else {
      p.values [j-1,1] <- colnames(tmp.list[[i]][j])
      p.values [j-1,i+1] <- "NA"

      significance.values[j-1,1] <- colnames(tmp.list[[i]][j])
      significance.values[j-1,i+1] <- "NA"
    }
  }
}

colnames(significance.values) <- c("ind.Variable", "child.risk", "child.Sep", "women.risk", "disability",
                                   "elderly.risk", "family.unit", "legal.prot", "torture", "sgbv",
                                   "single.parent", "sermed.condition")


