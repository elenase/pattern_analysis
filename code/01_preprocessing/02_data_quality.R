#source("code/00_preprocessing/00_packages.R")
#source("code/00_preprocessing/01_get_data.R")
#############################################################################################
cat("Data Quality of sociodemographic data\n")
## This script does not produce new data or clean existing one
## It gives an overview of some data quality measurements

########################################################################################
cat("Necessary Data: Activate data load if variables not existent\n")

data <- data.orig 




#############################################################################################
cat("Data Summary of sociodemographic data\n")

#data.summary <- data.frame(unclass(summary(data.orig)), check.names = FALSE, stringsAsFactors = FALSE)
summary(original.prcase)

#############################################################################################
cat("Number of rows with missing values in percent\n")

missing.values <- data.frame()
original.prcase$X <- NULL
for (i in 2: 35) {
  missing.values[i,1] <- colnames(original.prcase[i])
  missing.values[i,2] <- round(sum(original.prcase[i] == "")/nrow(original.prcase)*100, 2)
  missing.values[i,3] <- round(sapply(original.prcase[i], function(x) sum(length(which(is.na(x)))))/nrow(original.prcase)*100, 2)
  missing.values[i,4] <- round(sum(original.prcase[i] == "-")/nrow(original.prcase)*100, 2)
  missing.values[i,5] <- rowSums(missing.values[i,c(2:4)], na.rm=TRUE)
}
colnames(missing.values) <- c("variable", "pct.empty", "pct.NA", "pct.minus", "sum")
missing.values[is.na(missing.values)] <- 0
missing.values.top15 <- missing.values[order(missing.values$sum,decreasing=T)[1:15],]
z <- ztable(missing.values.top15,align="ccccc")
z



#############################################################################################
cat("Misspellings: only relevant for categorical data\n")
X.quanti <- splitmix(original.prcase)$X.quanti  ## (splitmix is from package 'PCAmixdata')
X.quali <- splitmix(original.prcase)$X.quali
X.quali$CaseNo <- NULL ##remove ID
misspellings <- list()
for (i in 1:length(X.quali)) { 
  df <- unique(X.quali[i])
  misspellings[[i]] <- df
}
misspellings[[1]]
## -> visual analysis of misspellings neccessary


#############################################################################################
cat("Violated attribute dependencies\n")

## shows inconsistencies in spatial data
# ## check of consistency of child levels with parent levels
adm.codes <- summarySE(original.prcase, measurevar="Num_Inds", groupvars=c("CountryAsylum", "coal1", "coal1id"), na.rm=TRUE)
adm.codes.consistent <- adm.codes[adm.codes$CountryAsylum == substr(adm.codes$coal1id, 1, 3),]
pct.adm.codes.consistent <- sum(adm.codes.consistent$N)/sum(adm.codes$N)*100 ## percent consistent cases

## Number of observations where coal1id has 2 instead of three characters
adm.codes.twochar <- adm.codes[(substr(adm.codes$CountryAsylum, 1, 2) == substr(adm.codes$coal1id, 1, 2)),]
no.adm.codes.twochar <- nrow(adm.codes.twochar)-nrow(adm.codes.consistent) ##

all.yem <- adm.codes.twochar[(adm.codes.twochar$CountryAsylum == "YEM"),]

x <- rbind(adm.codes.twochar, adm.codes.consistent)
adm.codes.twochar <- x[! duplicated(x, fromLast=TRUE) & seq(nrow(x)) <= nrow(adm.codes.twochar), ] ## observations where coal1id is 2 character

##amount of incorrect data of Yemen
incorrect.yem <- round(sum(adm.codes.twochar$N)/sum(all.yem$N)*100, 2)
## YEMEN ADM1 can be be corrected

######### adm 2
adm.codes <- summarySE(original.prcase, measurevar="Num_Inds", groupvars=c("CountryAsylum", "coal2", "coal2id"), na.rm=TRUE)
adm.codes.consistent <- adm.codes[adm.codes$CountryAsylum == substr(adm.codes$coal2id, 1, 3),]
pct.adm.codes.consistent <- sum(adm.codes.consistent$N)/sum(adm.codes$N)*100 ## percent consistent cases

## observations where coal1id has 2 instead of three character
adm.codes.twochar <- adm.codes[(substr(adm.codes$CountryAsylum, 1, 2) == substr(adm.codes$coal1id, 1, 2)),]
no.adm.codes.twochar <- nrow(adm.codes.twochar)-nrow(adm.codes.consistent)


x <- rbind(adm.codes.twochar, adm.codes.consistent)
adm.codes.twochar <- x[! duplicated(x, fromLast=TRUE) & seq(nrow(x)) <= nrow(adm.codes.twochar), ] ## observations where coal1id is 2 character



######### adm 1+2
adm.codes <- summarySE(original.prcase, measurevar="Num_Inds", groupvars=c("CountryAsylum", "coal1", "coal1id", "coal2", "coal2id"), na.rm=TRUE)
adm.codes.consistent1 <- adm.codes[adm.codes$CountryAsylum == substr(adm.codes$coal1id, 1, 3),]
adm.codes.consistent2 <- adm.codes.consistent1[(substr(adm.codes.consistent1$coal1id, 1, 3) == substr(adm.codes.consistent1$coal2id, 1, 3)),]
pct.adm.codes.consistent <- sum(adm.codes.consistent2$N)/sum(adm.codes$N)*100 ## percent consistent cases

## percent number show that more inconsistencies among adm0 and adm2 than adm1 and adm2

rm(adm.codes, adm.codes.consistent, adm.codes.consistent1, adm.codes.consistent2, adm.codes.twochar)
rm(all.yem)
rm(missing.values, missing.values.top15)

rm(original.spneed, original.prcase) #could be removed to safe memory
